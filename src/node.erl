-module(node).
-export([start_system/0, test/0]).

% Globale "Variablen"
-define(Config, '../config/system.cfg').
-define(Else, true).

-record( node, {
	name,
	state=sleeping,
	level=0,
	find_count=0,
	basic_edges=[],
	branch_edges=[],
	rejected_edges=[],
	in_branch=nil,
	test_edge=nil,
	frag_name=0,
	best_edge=nil,
	best_weight=infinity}).

start_system() ->
	pman:start(),
	start_system(nodecount())
.%

start_system(0) ->
	{ok, Hosts} = file:consult("../config/hosts.cfg"),
	net_adm:world_list(Hosts),
	timer:sleep(1000),
	n0 ! wakeup,
	true;
start_system(Count) ->
	Name = list_to_atom("n" ++ integer_to_list(Count-1)),
	spawn(fun() ->start(Name) end),
	start_system(Count-1)
.%

start(Name) ->
	Info = init(Name),
	loop(Info)
.%

init(Name) ->
	register(Name, self()),
	global:register_name(Name, self()),
	{ok, Hosts} = file:consult("../config/hosts.cfg"),
	net_adm:world_list(Hosts),
	E = get_edges(),
	%log(werkzeug:to_String(E)),
	#node{name=Name, basic_edges=E}
.%

loop(Info) ->
	receive
		wakeup ->
			Info_new = wakeup(Info),
			loop(Info_new);
	  {initiate,Level,FragName,NodeState,Edge} ->
			Info_new = initiate(Level,FragName,NodeState,Edge, Info),
			loop(Info_new);
		{test,Level,Fragname,Edge} ->
			Info_new = test(Level,Fragname,Edge, Info),
			loop(Info_new);
		{accept,Edge} ->
			Info_new = accept(Edge, Info),
			loop(Info_new);
		{reject,Edge} ->
			Info_new = reject(Edge, Info),
			loop(Info_new);
		{report,Weight,Edge} ->
			Info_new = report(Weight, Edge, Info),
			loop(Info_new);
		{changeroot,Edge} ->
			Info_new = changeroot(Edge, Info),
			loop(Info_new);
		{connect,Level,Edge} ->
			Info_new = connect(Level, Edge, Info),
			loop(Info_new);
		status ->
			log(werkzeug:to_String(Info)),
			loop(Info);
		finished ->
			finished(Info);
		A ->
			log("### Help, I was killed by message " ++ s(A) ++ " ###")
	end	
.%

% (1) Response to spontaneous awakening	
wakeup(Info) ->
	log("wakeup Empfangen"),
	
	if Info#node.state == sleeping ->	% can occur only at a node in the sleeping state
		procedure_wakeup(Info);			% execute procedure wakeup
	?Else ->
		Info
	end
.%

% (2) procedure wakeup
procedure_wakeup(Info) ->
	[Edge|_] = Info#node.basic_edges,		% Edge = m
	Info_new = move_to_branch(Info, Edge),	% SE(m) <- Branch
	Info_new2 = Info_new#node{
						level=0,		% LN <- 0
						state=found,	% SN <- Found;
						find_count=0}, % Find-count <- 0
	send_over_edge(Edge, {connect, 0, Edge}),	% send Connect(0) on edge m
	Info_new2
.%

% (3) Response to receipt of Connect(L) on edge j
connect(Level,Edge,Info) ->							% Edge = j
	log("connect Empfangen" ++ from(Edge)),
	
	Info_new = if Info#node.state == sleeping ->	% if SN = Sleeping then
		procedure_wakeup(Info);						% execute procedure wakeup
	?Else ->
		Info
	end,
	
	if (Level < Info_new#node.level) ->				% if L < LN then
		Info_new2 = move_to_branch(Info_new, Edge),	% SE(j) <- Branch
		LN = Info_new2#node.level,
		FN = Info_new2#node.frag_name,
		SN = Info_new2#node.state,
		send_over_edge(Edge, {initiate, LN, FN, SN, Edge}),	% send Initiate(LN, FN, SN) on edge j
		
		if SN == find ->								% if SN = Find then
			Find_count = Info_new2#node.find_count + 1,	% find-count <- find-count + 1
			Info_new2#node{
				find_count=Find_count};
		?Else ->
			Info_new2
		end;
	?Else ->
		IsBasic = node_tools:equals(node_tools:get_edge(Info_new#node.basic_edges, Edge) , Edge),
		if (IsBasic) ->	% if SE(j) = Basic then
			requeue({connect,Level,Edge});	% place received message on end of queue
		?Else ->
			W = weight(Edge),	% w(j)
			LN_plus_1 = Info_new#node.level + 1,
			send_over_edge(Edge, {initiate, LN_plus_1, W, find, Edge})	% send Initiate(LN +1, w(j), Find) on edge j
		end,
		Info_new
	end
.%

% (4) Response to receipt of Initiate(L, F, S) on edge j
initiate(Level, FragName, NodeState, Edge, Info) ->
	log("Initiate Empfangen" ++ from(Edge)),
	
	Info_new = Info#node{
			level=Level,			% LN <- L
			frag_name=FragName,		% FN <- F
			state=NodeState,		% SN <- S
			in_branch=Edge,			% in-branch <- j
			best_edge=nil,			% best-edge <- nil
			best_weight=infinity},	% best-wt <- infinity
	
	Find_count = send_initiate(Info_new#node.branch_edges, Level, FragName, NodeState, Edge, Info_new#node.find_count),	% for all i != j such that SE(i) = Branch
	Info_new2 = Info_new#node{find_count=Find_count},	% set new find-count
	
	if NodeState == find ->			% if S = Find then
		procedure_test(Info_new2);	% execute procedure test
	?Else ->
		Info_new2
	end		
.%

send_initiate([], _,_,_,_, Find_count) ->										% no edges (i) remaining
	Find_count;																	% return new find-count 
send_initiate([Edge_j|T], Level, FragName, NodeState, Edge_j, Find_count) ->	% do nothing when i = j (first in branch list is equal to j)
	send_initiate(T, Level, FragName, NodeState, Edge_j, Find_count);
send_initiate([Edge_i|T], Level, FragName, NodeState, Edge_j, Find_count) ->	% everything is ok, do the main work
	send_over_edge(Edge_i, {initiate, Level, FragName, NodeState, Edge_i}),		% send Initiate(L, F, S) on edge i
	Find_count_new = if NodeState == find ->	% if S = Find then
		Find_count + 1;							% find-count <- find-count + 1
	?Else ->
		Find_count
	end,
	send_initiate(T, Level, FragName, NodeState, Edge_j, Find_count_new)
.%

% (5) procedure test
procedure_test(Info) ->
	if Info#node.basic_edges /= [] ->	% if there are adjacent edges in the state Basic then
		[Test_edge | _] = Info#node.basic_edges,	% test-edge <- the minimum-weight adjacent edge in state Basic
		LN = Info#node.level,
		FN = Info#node.frag_name,
		send_over_edge(Test_edge , {test, LN, FN, Test_edge}),	% send Test(LN, FN) on test-edge
		Info#node{test_edge=Test_edge};				% test-edge <- the minimum-weight adjacent edge in state Basic
	?Else ->
		procedure_report(Info#node{test_edge=nil})	% test-edge <- nil; execute procedure report end
	end
.%

% (6) Response to receipt of Test(L, F) on edge j
test(Level,Frag_name,Edge, Info) ->
	log("test Empfangen" ++ from(Edge)),
	
	Info_new = if Info#node.state == sleeping ->	% if SN = Sleeping then execute procedure wakeup
		procedure_wakeup(Info);						% execute procedure wakeup
	?Else ->
		Info
	end,
	
	if Level > Info_new#node.level ->				% if L > LN then
		requeue({test, Level, Frag_name, Edge}),	% place received message on end of queue
		Info_new;
	?Else ->
		if Frag_name /= Info_new#node.frag_name ->	% if F != FN then
			send_over_edge(Edge, {accept, Edge}),	% send Accept on edge j
			Info_new;
		?Else ->
			Is_basic = node_tools:equals(node_tools:get_edge(Info_new#node.basic_edges, Edge), Edge),
			Info_new3 = if Is_basic ->				% if SE (j) = Basic then
				move_to_rejected(Info_new, Edge);	% SE(j) <- Rejected
			?Else ->
				Info_new
			end,
			
			Equal = node_tools:equals(Info_new3#node.test_edge, Edge),
			if not Equal ->								% if test-edge != j then
				send_over_edge(Edge, {reject,Edge}),	% send Reject on edge j
				Info_new3;
			?Else ->
				procedure_test(Info_new3)				% execute procedure test
			end
		end
	end
.%

% (7) Response to receipt of Accept on edge j
accept(Edge, Info) ->
	log("accept Empfangen" ++ from(Edge)),
	
	Info_new = Info#node{
			test_edge=nil},	% test-edge=nil
	Weight_j = weight(Edge),
	Best_weight = Info_new#node.best_weight,
	Info_new2 = if Weight_j < Best_weight ->	% if w(j) < best-wt then
		Info_new#node{
				best_edge=Edge,			% best-edge <- j
				best_weight=Weight_j};	% best-wt <- w(j)
	?Else ->
		Info_new
	end,
	procedure_report(Info_new2)	% execute procedure report
.%

% (8) Response to receipt of Reject on edge j
reject(Edge, Info) ->
	log("reject Empfangen" ++ from(Edge)),
	
	Is_basic = node_tools:equals(node_tools:get_edge(Info#node.basic_edges, Edge), Edge),
	Info_new = if Is_basic ->			% if SE(j) = Basic then
		move_to_rejected(Info, Edge);	% SE(j) <- Rejected
	?Else ->
		Info
	end,
	procedure_test(Info_new)	% execute procedure test
.%

% (9) procedure report
procedure_report(Info) ->
	if (Info#node.find_count == 0) and (Info#node.test_edge == nil) ->	% if find-count = 0 and test-edge = nil then
		send_over_edge(Info#node.in_branch, {report, Info#node.best_weight, Info#node.in_branch}),	% send Report(best-wt) on in-branch
		Info#node{state=found};	% SN <- Found
	?Else ->
		Info
	end
.%

% (10) Response to receipt of Report(w) on edge j
report(Weight, Edge, Info) ->
	log("report Empfangen" ++ from(Edge)),
	
	Equals = node_tools:equals(Info#node.in_branch, Edge),
	if not Equals ->	% if j != in-branch
		Info_new = Info#node{
						find_count=Info#node.find_count-1},		% find-count <- find-count - 1

		Info_new2 = if Weight < Info_new#node.best_weight ->	% if w < best-wt then
			Info_new#node{
					best_weight=Weight,							% begin best-wt <- w
					best_edge=Edge};							% best-edge <- j
		?Else ->
			Info_new
		end,
		procedure_report(Info_new2);							% execute procedure report
	?Else ->
		if Info#node.state == find ->		% if SN = Find then
			requeue({report,Weight,Edge}),	% place received message on end of queue
			Info;
		?Else ->
			if Weight > Info#node.best_weight ->	% if w > best-wt then
				procedure_changeroot(Info);			% execute procedure change-root
			?Else ->
				if (Weight == Info#node.best_weight) and (Weight == infinity) ->	% if w = best-wt = infinity then
					finished(Info);													% halt					
				?Else ->
					Info
				end
			end
		end
	end
.%

% (11) procedure change-root
procedure_changeroot(Info) ->
	Best_edge = Info#node.best_edge,
	Is_branch = node_tools:equals(node_tools:get_edge(Info#node.branch_edges, Best_edge), Best_edge),
	if Is_branch ->												% if SE(best-edge) = Branch then
		send_over_edge(Best_edge, {changeroot, Best_edge}),		% send Change-root on best-edge
		Info;
	?Else ->
		LN = Info#node.level,
		send_over_edge(Best_edge, {connect, LN, Best_edge}),	% send Connect(LN) on best-edge
		move_to_branch(Info, Best_edge)							% SE(best-edge) <- Branch
	end
.%

% (12) Response to receipt of Change-root
changeroot(Edge, Info) ->
	log("changeroot Empfangen" ++ from(Edge)),
	
	procedure_changeroot(Info)	% execute procedure change-root
.%

finished(Info) ->
	log_result("### Finished: " ++ s(Info#node.branch_edges)),
	lists:foreach(fun(E) -> send_over_edge(E, finished) end, get_edges()),
	timer:sleep(1000),
	exit(normal)
.%

%%% helper

from(Edge) ->
	" Sender: " ++ s(sender(Edge))
.%

s(V) ->
	werkzeug:to_String(V)
.%

requeue(Message) ->
	timer:sleep(200),
	self() ! Message,
	log(s(Message) ++ " wurde zurueckgestellt")
.%

send_over_edge(Edge, Message) ->
	S = sender(Edge),
	S ! Message,
	log(s(Message) ++ " an " ++ s(S) ++ " gesendet"),
	timer:sleep(10)
.%

sender(Edge) ->
	{_, _, NodeY} = node_tools:transform_edge(Edge),
	NodeY
.%

weight({W,_,_}) ->
	W
.%

move(From, To, Edge) ->
	From_new = lists:delete(node_tools:transform_edge(Edge), From),
	To_new = To ++ [node_tools:transform_edge(Edge)],
	{From_new, To_new}
.%

move_to_branch(Info, Edge) ->
	log("Move To Branch: " ++ s(Edge)),
	{From, To} = move(Info#node.basic_edges, Info#node.branch_edges ,Edge),
	Info#node{basic_edges=From, branch_edges=To}
.%

move_to_rejected(Info, Edge) ->
	log("Move To Rejected: " ++ s(Edge)),
	{From, To} = move(Info#node.basic_edges, Info#node.rejected_edges ,Edge),
	Info#node{basic_edges=From, rejected_edges=To}
.%

get_edges() ->
	{ok, Neighbours} = file:consult("../config/" ++ integer_to_list(nodecount()) ++ "/" ++ node_tools:name_string() ++ ".cfg"),
	node_tools:sort_edges(node_tools:convert_to_edges(Neighbours))
.%

log(Text) ->
	{ok, Hostname} = inet:gethostname(),
	Var = io_lib:format("~s: ~s~n", [node_tools:name_string() ++ "@" ++ Hostname, s(Text)]),
	werkzeug:logging("../logs/nodes.log",Var)
.%

log_result(Text) ->
	{ok, Hostname} = inet:gethostname(),
	Var = io_lib:format("~s: ~s~n", [node_tools:name_string() ++ "@" ++ Hostname, s(Text)]),
	werkzeug:logging("../logs/result.log",Var)
.%

nodecount() ->
	{ok, Nodecount} = werkzeug:get_config_value(nodecount, tools:read_config(?Config)),
	Nodecount
.%

%%% TEST

test() ->
	Name = test,
	register(Name, self()),
	global:register_name(Name, self()),
	test_move(),
	test_move_to_branch(),
	test_move_to_rejected(),
	test_weight(),
	test_sender(),
	test_send_over_edge(),
	io:format("Alle Tests ok~n"),
	halt()
.%

test_move() ->
	Edge = {1, test, neighbour},
	From = [Edge],
	To = [],
	% From <--> To
	{To, From} = move(From, To, Edge)
.%

test_move_to_branch() ->
	Edge = {1, test, neighbour},
	E = [Edge],
	Info = #node{basic_edges=E},
	true = (Info#node{basic_edges=[], branch_edges=E} == move_to_branch(Info, Edge))
.%

test_move_to_rejected() ->
	Edge = {1, test, neighbour},
	E = [Edge],
	Info = #node{basic_edges=E},
	true = (Info#node{basic_edges=[], rejected_edges=E} == move_to_rejected(Info, Edge))
.%

test_weight() ->
	Edge = {123, a, b},
	123 = weight(Edge)
.%

test_sender() ->
	Edge1 = {123, test, a},
	Edge2 = {123, a, test},
	
	a = sender(Edge1),
	a = sender(Edge2)
.%

test_process(Name) ->
	register(Name, self()),
	global:register_name(Name, self()),
	log("test_process now in receive state"),
	receive
		{test, Sender} -> Sender ! {test, Sender}
	after
		1000 -> true
	end,
	exit(normal)
.%

test_send_over_edge() ->
	%Edge = {123, test, dest},
	%Message = {test, self()},
	spawn(fun() -> test_process(dest) end),
	%log(sender(Edge)),
	
	%sender(Edge) ! Message,
	%send_over_edge(Edge, Message),
	%receive
	%	Msg -> Msg == Message
	%end
	timer:sleep(1000)
.%
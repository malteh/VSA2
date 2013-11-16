-module(node).
-export([start_system/0]).

% Globale "Variablen"
-define(Config,  '../config/system.cfg').
-define(Else, true).

-record( node, { name, state=sleeping, level=0, find_count=0, basic_edges=[], branch_edges=[], rejected_edges=[], in_branch=nil, test_edge=nil, frag_name=0, best_edge=nil, best_weight=infinity }).

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
		alldone ->
			alldone();
		A ->
			log("### Help, I was killed by message " ++ s(A) ++ " ###")
	end	
.%

% (1) & (2)
wakeup(Info) ->
	if Info#node.state == sleeping ->
		[Edge|_] = Info#node.basic_edges,
		Info_new = move_to_branch(Info, Edge),
		Info_new2 = Info_new#node{level=0, state=found, find_count=0},
		send_over_edge(Edge, {connect, 0, Edge}),
		Info_new2;
	?Else ->
		Info
	end
.%

% (3)
connect(Level,Edge,Info) ->
	log("connect Empfangen" ++ from(Edge)),
	Info_new = if Info#node.state == sleeping ->
		wakeup(Info);
	?Else ->
		Info
	end,
	
	if (Level < Info_new#node.level) ->
		Info_new2 = move_to_branch(Info_new, Edge),
		send_over_edge(Edge, {initiate, Info_new2#node.level,Info_new2#node.frag_name, Info_new2#node.state, Edge}),
		if Info_new2#node.state == find ->
			Info_new2#node{find_count=Info_new2#node.find_count+1};
		?Else ->
			Info_new2
		end;
	?Else ->
		IsBasic = node_tools:equals(node_tools:get_edge(Info_new#node.basic_edges, Edge) , Edge),
		if (IsBasic) ->
			requeue({connect,Level,Edge});
		?Else ->
			{Weight, _, _} = Edge,
			send_over_edge(Edge, {initiate, Info_new#node.level+1, Weight, find, Edge})
		end,
		Info_new
	end
.%

% (4)
initiate(Level, FragName, NodeState, Edge, Info) ->
	log("Initiate Empfangen" ++ from(Edge)),
	Info_new = Info#node{level=Level, frag_name=FragName, state=NodeState, in_branch=Edge, best_edge=nil, best_weight=infinity},
	log("Level gesetzt " ++ s(Level)),
	Find_count = send_initiate(Info_new#node.branch_edges, Level, FragName, NodeState, Edge, Info_new#node.find_count),
	Info_new2 = Info_new#node{find_count=Find_count},
	if NodeState == find ->
		procedure_test(Info_new2);
	?Else ->
		Info_new2
	end		
.%

send_initiate([], _,_,_,_, Find_count) ->
	Find_count;
send_initiate([Edge_j|T], Level, FragName, NodeState, Edge_j, Find_count) ->
	send_initiate(T, Level, FragName, NodeState, Edge_j, Find_count);
send_initiate([Edge_i|T], Level, FragName, NodeState, Edge_j, Find_count) ->
	send_over_edge(Edge_i, {initiate,Level,FragName,NodeState,Edge_i}),
	Find_count_new = if NodeState == find ->
		Find_count + 1;
	?Else ->
		Find_count
	end,
	send_initiate(T, Level, FragName, NodeState, Edge_j, Find_count_new)
.%

% (5)
procedure_test(Info) ->
	if Info#node.basic_edges /= [] ->
		[Test_edge | _] = Info#node.basic_edges,
		log("Test_edge: " ++ s(Test_edge)),
		send_over_edge(Test_edge , {test, Info#node.level, Info#node.frag_name, Test_edge}),
		Info#node{test_edge=Test_edge};
	?Else ->
		procedure_report(Info#node{test_edge=nil})
	end
.%

% (6)
test(Level,Fragname,Edge, Info) ->
	log("test Empfangen" ++ from(Edge)),
	Info_new = if Info#node.state == sleeping ->
		wakeup(Info);
	?Else ->
		Info
	end,
	if Level > Info_new#node.level ->
		requeue({test, Level, Fragname, Edge}),
		Info_new;
	?Else ->
		if Fragname /= Info_new#node.frag_name ->
			send_over_edge(Edge, {accept, Edge}),
			Info_new;
		?Else ->
			Is_basic = node_tools:equals(node_tools:get_edge(Info_new#node.basic_edges, Edge), Edge),
			Info_new3 = if Is_basic ->
				move_to_rejected(Info_new, Edge);
			?Else ->
				Info_new
			end,
			Equal = node_tools:equals(Info_new3#node.test_edge, Edge),
			if not Equal ->
				send_over_edge(Edge, {reject,Edge}),
				Info_new3;
			?Else ->
				procedure_test(Info_new3)
			end
		end
	end
.%

% (7)
accept(Edge, Info) ->
	log("accept Empfangen" ++ from(Edge)),
	Info_new = Info#node{test_edge=nil},
	Weight = weight(Edge),
	Best_weight = Info_new#node.best_weight,
	Info_new2 = if Weight < Best_weight ->
		Info_new#node{best_edge=Edge, best_weight=Weight};
	?Else ->
		Info_new
	end,
	procedure_report(Info_new2)
.%

% (8)
reject(Edge, Info) ->
	log("reject Empfangen" ++ from(Edge)),
	Is_basic = node_tools:equals(node_tools:get_edge(Info#node.basic_edges, Edge) , Edge),
	Info_new = if Is_basic ->
		move_to_rejected(Info, Edge);
	?Else ->
		Info
	end,
	procedure_test(Info_new)
.%

% (9)
procedure_report(Info) ->
	if (Info#node.find_count == 0) and (Info#node.test_edge == nil) ->
		send_over_edge(Info#node.in_branch, {report, Info#node.best_weight, Info#node.in_branch}),
		Info#node{state=found};
	?Else ->
		Info
	end
.%

% (10)
report(Weight, Edge, Info) ->
	log("report Empfangen" ++ from(Edge)),
	Equals = node_tools:equals(Info#node.in_branch, Edge),
	if not Equals ->
		Info_new = Info#node{find_count=Info#node.find_count-1},
		Info_new2 = if Weight < Info_new#node.best_weight ->
			Info_new#node{best_edge=Edge, best_weight=Weight};
		?Else ->
			Info_new
		end,
		procedure_report(Info_new2);
	?Else ->
		if Info#node.state == find ->
			requeue({report,Weight,Edge}),
			Info;
		?Else ->
			if Weight > Info#node.best_weight ->
				procedure_changeroot(Info);
			?Else ->
				if (Weight == Info#node.best_weight) and (Weight == infinity) ->
					log("### " ++ s(Info) ++ " ###"),
					timer:sleep(2000),
					exit(normal);
				?Else ->
					Info
				end
			end
		end
	end
.%

% (11)
procedure_changeroot(Info) ->
	Best_edge = Info#node.best_edge,
	Is_branch = node_tools:equals(node_tools:get_edge(Info#node.branch_edges, Best_edge), Best_edge),
	if Is_branch ->
		send_over_edge(Best_edge, {changeroot, Best_edge}),
		Info;
	?Else ->
		send_over_edge(Best_edge, {connect, Info#node.level, Best_edge}),
		move_to_branch(Info, Best_edge)
	end
.%

% (12)
changeroot(Edge, Info) ->
	log("changeroot Empfangen" ++ from(Edge)),
	procedure_changeroot(Info)
.%

alldone() ->
	log("AllDone")
.%

from(Edge) ->
	" Sender: " ++ s(sender(Edge))
.%

s(V) ->
	werkzeug:to_String(V)
.%

requeue(Message) ->
	timer:sleep(5000),
	self() ! Message,
	log(s(Message) ++ " wurde zurueckgestellt")
.%

send_over_edge(Edge, Message) ->
	S = sender(Edge),
	S ! Message,
	log(s(Message) ++ " an " ++ s(S) ++ " gesendet"),
	timer:sleep(1000)
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

nodecount() ->
	{ok, Nodecount} = werkzeug:get_config_value(nodecount, tools:read_config(?Config)),
	Nodecount
.%




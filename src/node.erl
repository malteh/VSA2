-module(node).
-export([start_system/0]).

% Globale "Variablen"
-define(Config,  '../config/system.cfg').
-define(Else, true).

-record( node, { name, status=sleeping, level=0, find_count=0, basic_edges=[], in_branch=nil, branch_edges=[], rejected_edges=[], test_edge, frag_name=0, akmg, best_weight }).

start_system() ->
	start_system(nodecount())
.%

start_system(0) ->
	{ok, Hosts} = file:consult("../config/hosts.cfg"),
	net_adm:world_list(Hosts),
	timer:sleep(1000),
	n0 ! wakeup,
	n0 ! wakeup,
	true;
start_system(Count) ->
	Name = list_to_atom("n" ++ integer_to_list(Count-1)),
	spawn(fun() -> start(Name) end),
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
	[{Weight, _ ,_} | _] = E,
	#node{name=Name, basic_edges=E, akmg=Weight}
.%

loop(Info) ->
	put(info,Info),
	%log(Info#node.status),
	receive
		wakeup ->
			Info_neu = wakeup(Info),
			loop(Info_neu);
	    {initiate,Level,FragName,NodeState,Edge} ->
			Info_neu = initiate(Level,FragName,NodeState,Edge, Info),
			loop(Info_neu);
		{test,Level,Fragname,Edge} ->
			Info_neu = test(Level,Fragname,Edge, Info),
			loop(Info_neu);			
		{accept,Edge} ->
			Info_neu = accept(Edge, Info),
			loop(Info_neu);			
		{reject,Edge} ->
			Info_neu = reject(Edge, Info),
			loop(Info_neu);
		{report,Weight,Edge} ->
			Info_neu = report(Weight, Edge, Info),
			loop(Info_neu);
		{changeroot,Edge} ->
			Info_neu = changeroot(Edge, Info),
			loop(Info_neu);			
		{connect,Level,Edge} ->
			Info_neu = connect(Level, Edge, Info),
			loop(Info_neu);			
		alldone ->
			alldone();
		A -> log(A)
	end	
.%

% (1) & (2)
wakeup(Info) ->
	if Info#node.status == sleeping ->
		[Edge|_] = Info#node.basic_edges,
		BasicEdges = lists:delete(node_tools:transform_edge(Edge),Info#node.basic_edges),
		Branch_Edges = Info#node.branch_edges ++ [node_tools:transform_edge(Edge)],

		sender(Edge) ! {connect, 0, Edge},
		
		Info#node{status=found, basic_edges=BasicEdges, branch_edges=Branch_Edges};
	?Else ->
		Info
	end
.%

% (3)
connect(Level,Edge,Info) -> 
	log("connect Empfangen"),
	if Info#node.status == sleeping ->
		Info_neu = wakeup(Info);
	?Else ->
		Info_neu = Info
	end,
	
	if (Level < Info#node.level) ->
		Info_neu2 = move_to_branch(Info_neu, Edge),
		sender(Edge) ! {initiate, Level,Info_neu2#node.frag_name, Info_neu2#node.status, Edge},
		if Info_neu2#node.status == find ->
			Info_neu2#node{find_count=Info_neu2#node.find_count+1};
		?Else ->
			Info_neu2
		end;
	% then 	place received message on end of queue
	?Else ->
		IsBasic = tools:contains(Info#node.basic_edges, node_tools:transform_edge(Edge)),
		if (not IsBasic) ->
			{Weight, _, _} = Edge,
			sender(Edge) ! {initiate, Level+1, Weight, find, Edge}; ?Else -> true
		end,
		Info_neu
	end
.%

% (4)
initiate(Level, FragName, NodeState, Edge, Info) -> 
	log("Initiate Empfangen"),
	Info_neu = Info#node{level=Level, frag_name=FragName, in_branch = Edge, akmg=nil, best_weight=infinity},
	Find_count = send_initiate(Info_neu#node.branch_edges, Level, FragName, NodeState, Edge, Info_neu#node.find_count),
	Info_neu2 = Info#node{find_count=Find_count},
	if NodeState == find ->
		procedure_test(Info_neu2);
	?Else ->
		Info_neu2
	end		
.%

send_initiate([], _,_,_,_, Find_count) ->
	Find_count;
send_initiate(Branch_liste, Level, FragName, NodeState, Edge_j, Find_count) ->
	[Edge_i|T] = Branch_liste,
	if Edge_i /= Edge_j ->
		sender(Edge_i) ! {initiate,Level,FragName,NodeState,Edge_i},
		if NodeState == find ->
			Find_count_new = Find_count + 1;
		?Else -> 
			Find_count_new = Find_count
		end,
		R = Find_count_new;
	?Else ->
		R = Find_count
	end,
	send_initiate(T, Level, FragName, NodeState, Edge_j, R)
.%

% (5)
procedure_test(Info) ->
	if Info#node.basic_edges /= [] ->
		[Test_edge | _] = Info#node.basic_edges,
		sender(Test_edge) ! {test, Info#node.level, Info#node.frag_name, Test_edge},
		Info#node{test_edge=Test_edge};
	?Else ->
		procedure_report(Info#node{test_edge=nil})
	end
.%

% (6)
test(Level,Fragname,Edge, Info) -> 
	log("test Empfangen"),
	New_info = if Info#node.status == sleeping ->
		wakeup(Info);
	?Else ->
		Info
	end,
	if Level > New_info#node.level ->
		self() ! {test, Level, Fragname, Edge},
		New_info;
	?Else ->
		if Fragname /= New_info#node.frag_name ->
			sender(Edge) ! {accept, Edge},
			New_info;
		?Else ->
			Is_basic = (node_tools:get_edge(New_info#node.basic_edges, Edge) == Edge),
			New_info3 = if Is_basic ->
				New_info#node{status=rejected};
			?Else ->
				New_info
			end,
			if New_info3#node.test_edge /= Edge ->
				sender(Edge) ! {reject,Edge},
				New_info3;
			?Else ->
				procedure_test(New_info3)
			end
		end
	end
.%

% (7)
accept(Edge, Info) -> 
	log("accept Empfangen"),
	Info_new = Info#node{test_edge=nil},
	Weight = weight(Edge),
	Best_weight = weight(Info_new#node.akmg),
	Info_new2 = if Weight < Best_weight ->
		Info_new#node{akmg=Edge};
	?Else ->
		Info_new
	end,
	procedure_report(Info_new2)
.%

% (8)
reject(Edge, Info) -> 
	log("reject Empfangen"),
	Is_basic = (node_tools:get_edge(Info#node.basic_edges, Edge) == Edge),
	if Is_basic ->
		move_to_rejected(Info, Edge);
	?Else ->
		Info
	end,
	procedure_test(Info)
.%

% (9)
procedure_report(Info) ->
	if (Info#node.find_count == 0) and (Info#node.test_edge == nil) ->
		sender(Info#node.in_branch) ! {report, weight(Info#node.akmg), Info#node.in_branch},
		Info#node{status=found};
	?Else ->
		Info
	end
.%

% (10)
report(Weight, Edge, Info) ->
	log("report Empfangen"),
	if Edge /= Info#node.in_branch ->
		Info_new = Info#node{find_count=Info#node.find_count-1},
		Best_weight = Info_new#node.best_weight,
		if Weight < Best_weight ->
			Info_new2 = Info_new#node{akmg=Edge},
			procedure_report(Info_new2);
		?Else ->
			Info_new
		end;
	?Else ->
		if Info#node.status == find ->
			self() ! {report,Weight,Edge};
		?Else ->
			if Weight > Info#node.best_weight ->
				procedure_changeroot(Info);
			?Else ->
				if (Weight == Info#node.best_weight) and (Weight == infinity) ->
					halt();
				?Else ->
					Info
				end
			end
		end
	end
.%

% (11)
procedure_changeroot(Info) ->
	Best_edge = Info#node.akmg,
	Is_branch = (node_tools:get_edge(Info#node.branch_edges, Best_edge) == Best_edge),
	if Is_branch ->
		sender(Best_edge) ! {changeroot, Best_edge},
		Info;
	?Else ->
		sender(Best_edge) ! {connect, Info#node.level, Best_edge},
		move_to_branch(Info, Best_edge)
	end
.%

% (12)
changeroot(_Edge, Info) -> 
	log("changeroot Empfangen"),
	procedure_changeroot(Info)
.%

alldone() ->
	log("AllDone")
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
	{From, To} = move(Info#node.basic_edges, Info#node.branch_edges ,Edge),
	Info#node{basic_edges=From, branch_edges=To}
.%

move_to_rejected(Info, Edge) ->
	{From, To} = move(Info#node.basic_edges, Info#node.rejected_edges ,Edge),
	Info#node{basic_edges=From, rejected_edges=To}
.%

get_edges() ->
	{ok, Neighbours} = file:consult("../config/" ++ integer_to_list(nodecount()) ++ "/" ++ node_tools:name_string() ++ ".cfg"),
	node_tools:sort_edges(node_tools:convert_to_edges(Neighbours))
.%

log(Text) ->
	{ok, Hostname} = inet:gethostname(),
	io:format("~s: ~s~n", [node_tools:name_string() ++ "@" ++ Hostname, werkzeug:to_String(Text)])
.%

nodecount() ->
	{ok, Nodecount} = werkzeug:get_config_value(nodecount, tools:read_config(?Config)),
	Nodecount
.%


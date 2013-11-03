-module(node).
-export([start_system/0]).

% Globale "Variablen"
-define(Config,  '../config/system.cfg').
-define(Else, true).

-record( node, { name, status=sleeping, level=0, find_count=0, basic_edges=[], branch_edges=[], frag_name=0, akmg }).

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
			Info_Neu = wakeup(Info),
			loop(Info_Neu);
	    {initiate,Level,FragName,NodeState,Edge} ->
			Info_Neu = initiate(Level,FragName,NodeState,Edge),
			loop(Info_Neu);
		{test,Level,Fragname,Edge} ->
			Info_Neu = test(Level,Fragname,Edge),
			loop(Info_Neu);			
		{accept,Edge} ->
			Info_Neu = accept(Edge),
			loop(Info_Neu);			
		{reject,Edge} ->
			Info_Neu = reject(Edge),
			loop(Info_Neu);
		{report,Weight,Edge} ->
			Info_Neu = report(Weight, Edge, Info),
			loop(Info_Neu);
		{changeroot,Edge} ->
			Info_Neu = changeroot(Edge),
			loop(Info_Neu);			
		{connect,Level,Edge} ->
			Info_Neu = connect(Level, Edge, Info),
			loop(Info_Neu);			
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
		Info_Neu = wakeup(Info);
	?Else ->
		Info_Neu = Info
	end,
	
	if (Level < Info#node.level) ->
		Info_Neu2 = move_to_branch(Info_Neu, Edge),
		sender(Edge) ! {initiate, Level,Info_Neu2#node.frag_name, Info_Neu2#node.status, Edge},
		if Info_Neu2#node.status == find ->
			Info_Neu2#node{find_count=Info_Neu2#node.find_count+1};
		?Else ->
			Info_Neu2
		end;
	% then 	place received message on end of queue
	?Else ->
		IsBasic = tools:contains(Info#node.basic_edges, node_tools:transform_edge(Edge)),
		if (not IsBasic) ->
			{Weight, _, _} = Edge,
			sender(Edge) ! {initiate, Level+1, Weight, find, Edge}; ?Else -> true
		end,
		Info_Neu
	end
.%

% (4)
initiate(Level,FragName,NodeState,Edge) -> 
	log("Initiate Empfangen")
.%

% (5)
%procedure_test()

% (6)
test(Level,Fragname,Edge) -> 
	log("test Empfangen")
.%

% (7)
accept(Edge) -> 
	log("accept Empfangen")
.%

% (8)
reject(Edge) -> 
	log("reject Empfangen")
.%

% (9)
%procedure_report()

% (10)
report(Weight, Edge, Info) ->
	log("report Empfangen"),
	Info
.%

% (11)
%procedure_changeroot()

% (12)
changeroot(Edge) -> 
	log("changeroot Empfangen")
.%

alldone() ->
	log("AllDone")
.%

sender(Edge) ->
	{_, _, NodeY} = node_tools:transform_edge(Edge),
	NodeY
.%

move_to_branch(Info, Edge) ->
	BasicEdges = lists:delete(node_tools:transform_edge(Edge),Info#node.basic_edges),
	Branch_Edges = Info#node.branch_edges ++ [node_tools:transform_edge(Edge)],
	Info#node{basic_edges=BasicEdges,branch_edges=Branch_Edges}
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


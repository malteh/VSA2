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

wakeup(Info) ->
	if Info#node.status == sleeping ->
		[Edge|_] = Info#node.basic_edges,
		BasicEdges = lists:delete(transform_edge(Edge),Info#node.basic_edges),
		Branch_Edges = Info#node.branch_edges ++ [transform_edge(Edge)],

		sender(Edge) ! {connect, 0, Edge},
		
		Info#node{status=found, basic_edges=BasicEdges, branch_edges=Branch_Edges};
	?Else ->
		Info
	end
.%

sender(Edge) ->
	{_, _, NodeY} = transform_edge(Edge),
	NodeY
.%

initiate(Level,FragName,NodeState,Edge) -> 
	log("Initiate Empfangen")
.%

test(Level,Fragname,Edge) -> 
	log("test Empfangen")
.%

accept(Edge) -> 
	log("accept Empfangen")
.%

reject(Edge) -> 
	log("reject Empfangen")
.%

changeroot(Edge) -> 
	log("changeroot Empfangen")
.%

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
		IsBasic = tools:contains(Info#node.basic_edges, transform_edge(Edge)),
		if (not IsBasic) ->
			{Weight, _, _} = Edge,
			sender(Edge) ! {initiate, Level+1, Weight, find, Edge}; ?Else -> true
		end,
		Info_Neu
	end
.%

report(Weight, Edge, Info) ->
	log("report Empfangen"),
	Info
.%

move_to_branch(Info, Edge) ->
	BasicEdges = lists:delete(transform_edge(Edge),Info#node.basic_edges),
	Branch_Edges = Info#node.branch_edges ++ [transform_edge(Edge)],
	Info#node{basic_edges=BasicEdges,branch_edges=Branch_Edges}
.%

transform_edge(Edge) ->
	{Weight,NodeX,NodeY} = Edge,
	Name = name(),
	if (NodeX == Name) ->
		Edge;
	?Else -> 
		{Weight,NodeY, NodeX}
	end
.%

alldone() ->
	log("AllDone")
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

convert_to_edge(Neighbour) ->
	{Gewicht,Name} = Neighbour,
	{Gewicht,name(),Name}
.%
convert_to_edges(Neighbours) ->
	convert_to_edges(Neighbours,[])
.%
convert_to_edges([], ErgebnisListe) ->
	ErgebnisListe;
convert_to_edges(Neighbours, ErgebnisListe) ->
	[Head | Tail] = Neighbours,
	convert_to_edges(Tail,ErgebnisListe ++ [convert_to_edge(Head)])
.%

get_edge([],_) ->
	undefined;
get_edge(Liste, Name) ->
	[ H | T ] = Liste,
	{Weight, NodeX, NodeY} = H,
	if NodeY == Name ->
		{ok, H};
	?Else ->
		get_edge(T,Name)
	end
.%

get_edges() ->
	{ok, Neighbours} = file:consult("../config/" ++ integer_to_list(nodecount()) ++ "/" ++ name_string() ++ ".cfg"),
	sort_edges(convert_to_edges(Neighbours))
.%

sort_edges(List) ->
	F = fun(X, Y) -> {XV, _, _} = X, {YV, _, _} = Y, {XV} < {YV} end,
	lists:sort(F, List)
.%

name() ->
	{_, Name} = process_info(self(), registered_name),
	Name
.%

name_string() ->
	atom_to_list(name())
.%

log(Text) ->
	{ok, Hostname} = inet:gethostname(),
	io:format("~s: ~s~n", [name_string() ++ "@" ++ Hostname, werkzeug:to_String(Text)])
.%

nodecount() ->
	{ok, Nodecount} = werkzeug:get_config_value(nodecount, tools:read_config(?Config)),
	Nodecount
.%


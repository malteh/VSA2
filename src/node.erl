-module(node).
-export([start_system/0]).

% Globale "Variablen"
-define(Config,  '../config/system.cfg').

start_system() ->
	start_system(nodecount())
.%

start_system(0) ->
	{ok, Hosts} = file:consult("../config/hosts.cfg"),
	net_adm:world_list(Hosts),
	n1 ! wakeup,
	true;
start_system(Count) ->
	%if Count < 0 ->
	%	log("start_system: wrong parameter"),
	%	halt()
	%end,
	Name = list_to_atom("n" ++ integer_to_list(Count)),
	spawn(fun() -> start(Name) end),
	start_system(Count-1)
.%

start(Name) ->
	init(Name),
	N = global:whereis_name(n1),
	log(N),
	log(global:registered_names()),
	receive
		A -> log(A)
	end
.%


init(Name) ->
	register(Name, self()),
	global:register_name(Name, self()),
	{ok, Hosts} = file:consult("../config/hosts.cfg"),
	net_adm:world_list(Hosts),
	Neighbours = neighbours( ),
	log(Neighbours)
.%

neighbours() ->
	{ok, Neighbours} = file:consult("../config/" ++ integer_to_list(nodecount()) ++ "/" ++ name_string() ++ ".cfg"),
	Neighbours
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
	io:format("~s: ~w~n", [name_string() ++ "@" ++ Hostname, Text])
.%

nodecount() ->
	{ok, Nodecount} = werkzeug:get_config_value(nodecount, tools:read_config(?Config)),
	Nodecount
.%
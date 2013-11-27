-module(node_tools).
-compile(export_all).
-define(Else, true).

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

get_edge([], _) ->
	undefined;
get_edge(Liste, Edge) ->
	[ H | T ] = Liste, 
	Eq = equals(Edge, H), 
	if Eq ->
		H;
	?Else ->
		get_edge(T, Edge)
	end
.%

equals(A, A) ->
	true;
equals({Weight, NodeX, NodeY}, {Weight, NodeX, NodeY}) ->
	true;
equals({Weight, NodeX, NodeY}, {Weight, NodeY, NodeX}) ->
	true;
equals(_, _) ->
	false.

convert_to_edge(Neighbour, Name) ->
	{Gewicht, Name2} = Neighbour, 
	{Gewicht, Name, Name2}
.%

convert_to_edges(Neighbours, Name) ->
	convert_to_edges(Neighbours, [], Name)
.%
convert_to_edges([], ErgebnisListe, _) ->
	ErgebnisListe;
convert_to_edges(Neighbours, ErgebnisListe, Name) ->
	[Head | Tail] = Neighbours, 
	convert_to_edges(Tail, ErgebnisListe ++ [convert_to_edge(Head, Name)], Name)
.%

transform_edge(Edge) ->
	{Weight, NodeX, NodeY} = Edge, 
	Name = name(), 
	if (NodeX == Name) ->
		Edge;
	?Else -> 
		{Weight, NodeY, NodeX}
	end
.%
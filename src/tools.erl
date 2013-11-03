-module(tools).
-export([now_milliseconds/0, now_seconds/0, read_config/1, contains/2, time_string/0, date_string/0, date_time_string/0, pushSL/2, max_in_list/1]).
-define(Else, true).

now_milliseconds() ->
	K = 1000,
	{Megasecs, Secs, Microsecs} = erlang:now(),
	MS = (Megasecs * K * K * K) + (Secs * K) + (Microsecs / K),
	MS
.%

now_seconds() ->
	now_milliseconds() / 1000
.%

read_config(Config) ->
	{ok, ConfigList} = file:consult(Config),
	ConfigList
.%

contains([], _) -> false;
contains([Elem|_], Elem) -> true;
contains([_|Tail],Elem ) -> contains(Tail, Elem)
.%

time_string() ->
	{_, {Hour,Min,Seconds}} = erlang:localtime(),
	integer_to_list(Hour) ++ ":" ++ integer_to_list(Min) ++ ":" ++ integer_to_list(Seconds)
.%

date_string() ->
	{{Year,Month,Day}, _} = erlang:localtime(),
	integer_to_list(Year) ++ "-" ++ integer_to_list(Month) ++ "-" ++ integer_to_list(Day)
.%

date_time_string() ->
	date_string() ++ "_" ++ time_string()
.%

max_in_list([H|T]) ->
	Max = H,
	max_in_list(Max, T)
.%
max_in_list(Max, []) ->
	Max;
max_in_list(Max, [H|T]) ->
	if (Max < H) ->
		Max_neu = H;
	?Else ->
		Max_neu = Max
	end,
	max_in_list(Max_neu, T)
.%

pushSL([{ElemNr, Elem}|TSL], {NElemNr,NElem}) when ElemNr < NElemNr ->
	[{ElemNr, Elem}|pushSL(TSL,{NElemNr,NElem})];
pushSL([{ElemNr, Elem}|TSL], {NElemNr,NElem}) when ElemNr >= NElemNr ->
	[{NElemNr,NElem},{ElemNr, Elem}|TSL];
pushSL([], {NElemNr,NElem}) ->
	[{NElemNr,NElem}].
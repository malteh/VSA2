-module(team).
-export([nummer/0, gruppe/0]).
-define(Config,  '../config/team.cfg').

nummer() ->
	ConfigList = tools:read_config(?Config),
	{ok, Nummer} = werkzeug:get_config_value(nummer, ConfigList),
	Nummer
.%

gruppe() ->
	ConfigList = tools:read_config(?Config),
	{ok, Gruppe} = werkzeug:get_config_value(gruppe, ConfigList),
	Gruppe
.%
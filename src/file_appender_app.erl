-module(file_appender_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    file_appender_sup:start_link().

stop(_State) ->
    ok.

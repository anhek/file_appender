-module(file_appender_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
        intensity => 10,
        period => 30},
    ChildSpecs = [
        #{
            id => file_appender_srv,
            start => {file_appender_srv, start_link, []},
            type => worker
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

-module(file_appender_srv).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(TABLE, file_descriptors).
-define(CLOSE_FILE_TIMEOUT, 10). %% seconds
-record(state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?TABLE = ets:new(?TABLE, [
        public, named_table, set, {write_concurrency, true}, {read_concurrency, true}]),
    erlang:send_after(1000, self(), gc),
    {ok, #state{}}.

handle_call(Request, _From, State = #state{}) ->
    ?LOG_INFO("Unhandled call ~p", [Request]),
    {reply, ok, State}.

handle_cast(Request, State = #state{}) ->
    ?LOG_INFO("Unhandled cast ~p", [Request]),
    {noreply, State}.

handle_info(gc, State = #state{}) ->
    Threshold = os:system_time(second) - 10,
    Selected = ets:select(?TABLE, [{{'$1', '$2', '$3'}, [{'>', Threshold, '$3'}], [['$1', '$2']]}]),
    lists:foreach(fun([Path, Fd]) ->
        true = ets:delete(?TABLE, Path),
        ok = file:close(Fd)
    end, Selected),
    erlang:send_after(1000, self(), gc),
    {noreply, State};
handle_info(Info, State = #state{}) ->
    ?LOG_INFO("Unhandled info ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State = #state{}) ->
    ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

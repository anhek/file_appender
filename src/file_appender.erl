-module(file_appender).

-export([append/2]).

-define(TABLE, file_descriptors).

%%%===================================================================
%%% API
%%%===================================================================

-spec append(Path :: filelib:path(), String :: string() | binary()) ->
    ok | {error, file:posix() | badarg | system_limit}.
append(Path, String) ->
    case ets:lookup(?TABLE, Path) of
        [{Path, Fd, _LastAccessTime} | _] ->
            ets:update_element(?TABLE, Path, {3, os:system_time(second)}),
            ok = file:write(Fd, [String, <<"\n">>]);
        [] ->
            case file:open(Path, [append]) of
                {ok, Fd} ->
                    ets:insert(?TABLE, {Path, Fd, os:system_time(second)}),
                    ok = file:write(Fd, [String, <<"\n">>]),
                    ok = check_race_condition(Path, Fd),
                    ok;
                {error, enoent} ->
                    ok = filelib:ensure_path(filename:dirname(Path)),
                    {ok, Fd} = file:open(Path, [append]),
                    ets:insert(?TABLE, {Path, Fd, os:system_time(second)}),
                    ok = file:write(Fd, [String, <<"\n">>]),
                    ok = check_race_condition(Path, Fd),
                    ok;
                {error, _} = Error -> Error
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_race_condition(Path, Fd) ->
    case ets:lookup(?TABLE, Path) of
        [{Path, Fd, _}] -> ok;
        _ -> ok = file:close(Fd) %% Stored another fd
    end.

%%%===================================================================
%%% Tests
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

multiple_files_test() ->
    {ok, _} = application:ensure_all_started(file_appender),
    BasePath = "testing/",
    file:del_dir_r(BasePath),
    ok = file_appender:append(BasePath++"f1.txt", <<"text_a1">>),
    ok = file_appender:append(BasePath++"f2.txt", <<"text_b1">>),
    ok = file_appender:append(BasePath++"f3.txt", <<"text_c1">>),
    ok = file_appender:append(BasePath++"f1.txt", <<"text_a2">>),
    ok = file_appender:append(BasePath++"f2.txt", <<"text_b2">>),
    ok = file_appender:append(BasePath++"f3.txt", <<"text_c2">>),
    ?assertEqual({ok, <<"text_a1\ntext_a2\n">>}, file:read_file(BasePath++"f1.txt")),
    ?assertEqual({ok, <<"text_b1\ntext_b2\n">>}, file:read_file(BasePath++"f2.txt")),
    ?assertEqual({ok, <<"text_c1\ntext_c2\n">>}, file:read_file(BasePath++"f3.txt")),
    file:del_dir_r(BasePath),
    ok.

close_files_test_() ->
    {timeout, 15, fun() ->
        {ok, _} = application:ensure_all_started(file_appender),
        BasePath = "testing/",
        file:del_dir_r(BasePath),
        ok = file_appender:append(BasePath++"f1.txt", <<"text_a1">>),
        ok = file_appender:append(BasePath++"f2.txt", <<"text_b1">>),
        ok = file_appender:append(BasePath++"f3.txt", <<"text_c1">>),
        ok = file_appender:append(BasePath++"f1.txt", <<"text_a2">>),
        ok = file_appender:append(BasePath++"f2.txt", <<"text_b2">>),
        ok = file_appender:append(BasePath++"f3.txt", <<"text_c2">>),
        timer:sleep(12*1000),
        ?assertEqual([], ets:tab2list(?TABLE)),
        file:del_dir_r(BasePath),
        ok
    end}.

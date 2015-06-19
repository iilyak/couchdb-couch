-module(couch_db_plugin_tests).

-export([
    validate_dbname/2
]).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-record(ctx, {pid, handle}).

setup() ->
    error_logger:tty(false),
    application:start(couch_epi),
    {ok, FunctionsPid} = couch_epi_functions:start_link(
        test_app, {epi_key, couch_db}, {modules, [?MODULE]},
        [{interval, 100}]),
    ok = couch_epi_functions:wait(FunctionsPid),
    #ctx{pid = FunctionsPid, handle = couch_epi:get_handle(couch_db)}.

teardown(#ctx{pid = FunctionsPid}) ->
    erlang:unlink(FunctionsPid),
    couch_epi_functions:stop(FunctionsPid),
    application:stop(couch_epi),
    ok.

validate_dbname({true, _Db}, _) -> true;
validate_dbname({false, _Db}, _) -> false;
validate_dbname({fail, _Db}, _) -> throw(validate_dbname).

callback_test_() ->
    {
        "callback tests",
        {
            foreach, fun setup/0, fun teardown/1,
            [
                fun validate_dbname_match/0,
                fun validate_dbname_no_match/0,
                fun validate_dbname_throw/0
            ]
        }
    }.


validate_dbname_match() ->
    ?_assertMatch(
        {true, [validate_dbname, db]},
        couch_db_plugin:validate_dbname({true, [db]}, db)).

validate_dbname_no_match() ->
    ?_assertMatch(
        {false, [db]},
        couch_db_plugin:validate_dbname({false, [db]}, db)).

validate_dbname_throw() ->
    ?_assertThrow(
        validate_dbname,
        couch_db_plugin:validate_dbname({fail, [db]}, db)).

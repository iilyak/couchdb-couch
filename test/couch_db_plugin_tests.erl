-module(couch_db_plugin_tests).

-export([
    validate_dbname/2,
    before_doc_update/2,
    after_doc_read/2
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

before_doc_update({fail, _Doc}, _Db) -> throw(before_doc_update);
before_doc_update({true, Doc}, Db) -> [{true, [before_doc_update|Doc]}, Db];
before_doc_update({false, Doc}, Db) -> [{false, Doc}, Db].

after_doc_read({fail, _Doc}, _Db) -> throw(after_doc_read);
after_doc_read({true, Doc}, Db) -> [{true, [after_doc_read|Doc]}, Db];
after_doc_read({false, Doc}, Db) -> [{false, Doc}, Db].

callback_test_() ->
    {
        "callback tests",
        {
            foreach, fun setup/0, fun teardown/1,
            [
                fun validate_dbname_match/0,
                fun validate_dbname_no_match/0,
                fun validate_dbname_throw/0,

                fun before_doc_update_match/0,
                fun before_doc_update_no_match/0,
                fun before_doc_update_throw/0,

                fun after_doc_read_match/0,
                fun after_doc_read_no_match/0,
                fun after_doc_read_throw/0
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

before_doc_update_match() ->
    ?_assertMatch(
        {true, [before_doc_update, doc]},
        couch_db_plugin:before_doc_update(#db{}, {true, [doc]})).

before_doc_update_no_match() ->
    ?_assertMatch(
        {false, [doc]},
        couch_db_plugin:before_doc_update(#db{}, {false, [doc]})).

before_doc_update_throw() ->
    ?_assertThrow(
        before_doc_update,
        couch_db_plugin:before_doc_update(#db{}, {fail, [doc]})).


after_doc_read_match() ->
    ?_assertMatch(
        {true, [after_doc_read, doc]},
        couch_db_plugin:after_doc_read(#db{}, {true, [doc]})).

after_doc_read_no_match() ->
    ?_assertMatch(
        {false, [doc]},
        couch_db_plugin:after_doc_read(#db{}, {false, [doc]})).

after_doc_read_throw() ->
    ?_assertThrow(
        after_doc_read,
        couch_db_plugin:after_doc_read(#db{}, {fail, [doc]})).

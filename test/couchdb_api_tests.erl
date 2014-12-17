% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couchdb_api_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

start() ->
    ok = test_util:start_couch(),
    ok.

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_USER]),
    couch_db:close(Db),
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
    Host = "http://" ++ Addr ++ ":" ++ Port,
    {Host, ?b2l(DbName)}.

teardown({_, DbName}) ->
    teardown(DbName);
teardown(DbName) ->
    ok = couch_server:delete(?l2b(DbName), []),
    ok.

api_test_() ->
    {
      "API tests",
      {
        setup,
        fun start/0, fun test_util:stop_couch/1,
        [
         api_tests()
        ]
      }
    }.

api_tests() ->
    [
     {
       "Public API tests",
       [
        {
          foreach,
          fun setup/0, fun teardown/1,
          [
           fun should_get_design_docs/1
          ]
        }
       ]
     }
    ].

should_get_design_docs({_Host, DbName}) ->
    ?_test(begin
        {ok, Db} = open_db(DbName),
        lists:foreach(fun(Idx) ->
            add_design_doc(DbName, <<"_design/example-", ($A + Idx)>>)
        end, lists:seq(1, 10)),
        {ok, Db1} = couch_db:reopen(Db),
        {ok, Docs} = couch_db:get_design_docs(Db1),
        ?assertEqual(10, length(Docs)),
        ?assertMatch([#full_doc_info{}|_], Docs)
    end).

open_db(DbName) ->
    couch_db:open_int(list_to_binary(DbName), [?ADMIN_USER]).

add_design_doc(DbName, Id) ->
    Body = design_doc_body(Id),
    add_doc(DbName, Body).

add_doc(DbName, DocBody) ->
    {ok, Db} = open_db(DbName),
    Doc = couch_doc:from_json_obj(DocBody),
    {ok, Rev} = couch_db:update_doc(Db, Doc, []),
    couch_db:close(Db),
    Rev.

design_doc_body(Id) ->
    {[
        {<<"_id">>, Id},
        {<<"views">>, {[
            {<<"foo">>, {[
                <<"map">>, <<"function(doc){ emit(doc._id, doc._rev)">>
            ]}}
        ]}}
    ]}.

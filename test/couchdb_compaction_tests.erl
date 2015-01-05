% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couchdb_compaction_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(ATT_TXT_NAME, <<"test_file.txt">>).
-define(ATT_SIZE, 123456).

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

attachments_test_() ->
    {
        "Attachments tests",
        {
            setup,
            fun start/0, fun test_util:stop_couch/1,
            [
                compaction_tests()
            ]
        }
    }.

compaction_tests() ->
    [
        {
            "Attachment compaction tests",
            [
                {
                    foreach,
                    fun setup/0, fun teardown/1,
                    [
                        fun should_preserve_att_when_delete_shared/1,
                        fun should_not_duplicate_inline_atts/1,
                        fun should_calcualte_active_size_on_doc_update/1
                    ]
                }
            ]
        }
    ].

should_not_duplicate_inline_atts(Ctx) ->
    ?_test(begin
        create_doc_with_attach(Ctx, "doc1"),
        create_doc_with_attach(Ctx, "doc2"),

        ?assertEqual(2, count_unique_atts(Ctx)),

        SizeBeforeCompaction = db_file_size(Ctx),

        compact_db(Ctx),

        ?assertEqual(1, count_unique_atts(Ctx)),

        AttData = attach_data(),
        ?assertMatch(AttData, get_att(Ctx, "doc1")),
        ?assertMatch(AttData, get_att(Ctx, "doc2")),

        SizeAfterCompaction = db_file_size(Ctx),

        ?assert(SizeAfterCompaction < SizeBeforeCompaction)

    end).

should_preserve_att_when_delete_shared(Ctx) ->
    ?_test(begin
        Rev = create_doc_with_attach(Ctx, "doc1"),
        create_doc_with_attach(Ctx, "doc2"),
        ?assertEqual(2, count_unique_atts(Ctx)),

        compact_db(Ctx),
        delete_doc(Ctx, "doc1", Rev),
        compact_db(Ctx),

        AttData = attach_data(),
        ?assertMatch(AttData, get_att(Ctx, "doc2"))
    end).

%% 1. Create doc A with attachment
%% 2. Create doc B with same attachment that will be de-duped
%% 3. Compact the database
%% 4. Check our active size
%% 5. Modify doc B (in a way that keeps the de-duped attachment without copying it)
%% 6. Check that active size isn't increased by more than the attachment size

should_calcualte_active_size_on_doc_update(Ctx) ->
    ?_test(begin
        create_doc_with_attach(Ctx, "doc1"),
        create_doc_with_attach(Ctx, "doc2"),

        ?assertEqual(2, count_unique_atts(Ctx)),

        CompactedLen = get_compacted_att_len(Ctx, "doc1"),

        compact_db(Ctx),

        {_, ActiveSizeBeforeUpdate, _} = db_sizes(Ctx),

        modify_doc(Ctx, "doc2"),

        %% make sure attach data is available
        AttData = attach_data(),
        ?assertMatch(AttData, get_att(Ctx, "doc1")),
        ?assertMatch(AttData, get_att(Ctx, "doc2")),

        {_, ActiveSizeAfterUpdate, _} = db_sizes(Ctx),
        ?assert((ActiveSizeAfterUpdate - ActiveSizeBeforeUpdate) < CompactedLen)
    end).

create_doc_with_attach(Ctx, Id) ->
    create_doc(Ctx, Id),
    add_att(Ctx, Id).

attach_data() ->
    %% We need file bigger than 4096 (gzipped)
    random:seed({1,2,3}),
    list_to_binary(lists:map(
        fun(_) -> $A + random:uniform(25) end, lists:seq(0, ?ATT_SIZE))).

%% doc API
create_doc({Host, DbName}, Id) ->
    Url = Host ++ "/" ++ DbName ++ "/" ++ Id,
    {ok, Code, _Headers, _Body} = test_request:put(Url, [], <<"{}">>),
    ?assertEqual(201, Code),
    ok.

get_doc(Host, DbName, Id) ->
    Url = Host ++ "/" ++ DbName ++ "/" ++ Id,
    {ok, 200, _, Body} = test_request:get(Url),
    Json = jiffy:decode(Body),
    Json.


modify_doc({Host, DbName}, Id) ->
    Url = Host ++ "/" ++ DbName ++ "/" ++ Id,
    Doc = get_doc(Host, DbName, Id),
    EJson = couch_util:json_apply_field({<<"extra">>, 1}, Doc),
    {ok, Code, _Headers, Body} = test_request:put(Url, [], jiffy:encode(EJson)),
    ?assertEqual(201, Code),
    NewDoc = jiffy:decode(Body),
    couch_util:get_nested_json_value(NewDoc, [<<"rev">>]).

delete_doc({Host, DbName}, Id, Rev) ->
    Url = Host ++ "/" ++ DbName ++ "/" ++ Id
        ++ "?rev=" ++ Rev,
    {ok, 200, _, _Body} = test_request:delete(Url),
    ok.

%% att API
add_att({Host, DbName}, DocId) ->
    Doc = get_doc(Host, DbName, DocId),
    Rev = couch_util:get_nested_json_value(Doc, [<<"_rev">>]),
    Url = Host ++ "/" ++ DbName ++ "/" ++ DocId ++ "/foo"
        ++ "?rev=" ++ Rev,
    {ok, Code, _Headers, Body} = test_request:put(Url, [], attach_data()),
    ?assertEqual(201, Code),
    NewDoc = jiffy:decode(Body),
    couch_util:get_nested_json_value(NewDoc, [<<"rev">>]).

get_att({Host, DbName}, DocId) ->
    Url = Host ++ "/" ++ DbName ++ "/" ++ DocId ++ "/foo",
    {ok, 200, _, Body} = test_request:get(Url),
    Body.

%% db API
db_sizes({Host, DbName}) ->
    Url = Host ++ "/" ++ DbName,
    {ok, 200, _, Body} = test_util:request(Url, [], get),
    J = jiffy:decode(Body),
    FileSize = couch_util:get_nested_json_value(J, [<<"sizes">>, <<"file">>]),
    Active = couch_util:get_nested_json_value(J, [<<"sizes">>, <<"active">>]),
    Ext = couch_util:get_nested_json_value(J, [<<"sizes">>, <<"external">>]),
    {FileSize, Active, Ext}.

%% db low level functions
get_compacted_att_len({_Host, DbName}, Id) ->
    {ok, Db} = couch_db:open_int(list_to_binary(DbName), []),
    {Att, _} = read_attach(Db, list_to_binary(Id)),
    Len = couch_att:fetch(att_len, Att),
    couch_db:close(Db),
    Len.

compact_db({_Host, DbName}) ->
    {ok, Db} = couch_db:open_int(list_to_binary(DbName), []),
    couch_db:start_compact(Db),
    couch_db:wait_for_compaction(Db),
    ok.

count_unique_atts({_, DbName}) ->
    {ok, Db} = couch_db:open_int(list_to_binary(DbName), []),
    {ok, _, Atts} = couch_btree:foldl(Db#db.id_tree,
        fun(#full_doc_info{rev_tree = Tree}, Acc) ->
            Atts = couch_key_tree:fold(
                fun(_, #leaf{atts = P}, _, A) ->
                        [P|A];
                   (_, [], _, A) ->
                        A
                end, [], Tree),
                {ok, Atts ++ Acc}
        end, []),
    couch_db:close(Db),
    sets:size(sets:from_list(lists:flatten(Atts))).

db_file_size({_Host, DbName}) ->
    {ok, #db{filepath = FilePath} = Db} = couch_db:open_int(list_to_binary(DbName), []),
    couch_db:close(Db),
    {ok, Data} = file:read_file(FilePath),
    size(Data).

read_attach(Db, DocId) ->
    {ok, #doc{atts = [Att]}} = couch_db:open_doc(Db, DocId, []),
    Content = couch_att:foldl_decode(Att, fun(A, Acc) -> Acc ++ A end, []),
    {Att, iolist_to_binary(Content)}.

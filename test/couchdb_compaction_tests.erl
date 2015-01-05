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

should_not_duplicate_inline_atts({_Host, DbName}) ->
    ?_test(begin
        create_inline_text_att(DbName, <<"doc1">>),
        create_inline_text_att(DbName, <<"doc2">>),
        ?assertEqual(2, count_unique_atts(DbName)),

        {ok, Db1} = couch_db:open_int(list_to_binary(DbName), []),

        SizeBeforeCompaction = db_file_size(Db1),

        couch_db:start_compact(Db1),
        couch_db:wait_for_compaction(Db1),
        couch_db:close(Db1),

        ?assertEqual(1, count_unique_atts(DbName)),

        {ok, Db2} = couch_db:open_int(list_to_binary(DbName), []),
        AttData = attach_data(),
        ?assertMatch({_, AttData}, read_attach(Db2, <<"doc1">>)),
        ?assertMatch({_, AttData}, read_attach(Db2, <<"doc2">>)),


        SizeAfterCompaction = db_file_size(Db2),

        ?assert(SizeAfterCompaction < SizeBeforeCompaction)

    end).

should_preserve_att_when_delete_shared({_Host, DbName}) ->
    ?_test(begin
        Rev = create_inline_text_att(DbName, <<"doc1">>),
        create_inline_text_att(DbName, <<"doc2">>),
        ?assertEqual(2, count_unique_atts(DbName)),

        {ok, Db1} = couch_db:open_int(list_to_binary(DbName), []),

        couch_db:start_compact(Db1),
        couch_db:wait_for_compaction(Db1),
        couch_db:close(Db1),

        {ok, Db2} = couch_db:open_int(list_to_binary(DbName), []),
        ok = delete_doc(Db2, <<"doc1">>, Rev),

        couch_db:start_compact(Db2),
        couch_db:wait_for_compaction(Db2),
        couch_db:close(Db2),

        {ok, Db3} = couch_db:open_int(list_to_binary(DbName), []),
        AttData = attach_data(),
        ?assertMatch({_, AttData}, read_attach(Db3, <<"doc2">>))
    end).

%% 1. Create doc A with attachment
%% 2. Create doc B with same attachment that will be de-duped
%% 3. Compact the database
%% 4. Check our active size
%% 5. Modify doc B (in a way that keeps the de-duped attachment without copying it)
%% 6. Check that active size isn't increased by more than the attachment size

should_calcualte_active_size_on_doc_update({_Host, DbName}) ->
    ?_test(begin
        create_inline_text_att(DbName, <<"doc1">>),
        create_inline_text_att(DbName, <<"doc2">>),
        ?assertEqual(2, count_unique_atts(DbName)),

        {ok, Db1} = couch_db:open_int(list_to_binary(DbName), []),

        couch_db:start_compact(Db1),
        couch_db:wait_for_compaction(Db1),
        couch_db:close(Db1),

        {ok, Db2} = couch_db:open_int(list_to_binary(DbName), []),
        {_, ActiveSizeBeforeUpdate, _} = sizes(Db2),
        modify_doc(DbName, <<"doc2">>),

        {ok, Db3} = couch_db:reopen(Db2),

        {_, ActiveSizeAfterUpdate, _} = sizes(Db2),
        SizesAfterUpdate = sizes(Db3),
        couch_db:close(Db3),

        ?assert((ActiveSizeAfterUpdate - ActiveSizeBeforeUpdate) < ?ATT_SIZE)
    end).

delete_doc(Db, Id, Rev) ->
    %% Until COUCHDB-2515 is fixed we cannot use couch_doc:delete_doc
    %%{ok, _Result} = couch_db:delete_doc(Db, Id, [{0, []}]),
    EJson = body_with_attach(Id),
    EJson1 = couch_util:json_apply_field({<<"_deleted">>, true}, EJson),
    RevStr = couch_doc:rev_to_str(Rev),
    EJson2 = couch_util:json_apply_field({<<"_rev">>, RevStr}, EJson1),
    Doc = couch_doc:from_json_obj(EJson2),
    {ok, _Rev} = couch_db:update_doc(Db, Doc, []),
    ok.

sizes(Db) ->
    {ok, Info} = couch_db:get_db_info(Db),
    {Sizes} = proplists:get_value(sizes, Info),
    {
        proplists:get_value(file, Sizes),
        proplists:get_value(active, Sizes),
        proplists:get_value(external, Sizes)
    }.

create_inline_text_att(DbName, Id) ->
    {ok, Db} = couch_db:open_int(list_to_binary(DbName), []),
    Doc = doc_with_attach(Id),
    {ok, Rev} = couch_db:update_doc(Db, Doc, []),
    couch_db:close(Db),
    Rev.

count_unique_atts(DbName) ->
    {ok, Db} = couch_db:open_int(list_to_binary(DbName), []),
    {ok, _, Atts} = couch_btree:foldl(Db#db.id_tree,
        fun(#full_doc_info{rev_tree = Tree}, Acc) ->
            Atts = couch_key_tree:fold(
                fun(_, #leaf{atts = P}, _, A) -> [P|A] end, [], Tree),
            {ok, Atts ++ Acc}
        end, []),
    couch_db:close(Db),
    sets:size(sets:from_list(Atts)).

db_file_size(#db{filepath = FilePath}) ->
    {ok, Data} = file:read_file(FilePath),
    size(Data).

read_attach(Db, DocId) ->
    {ok, #doc{atts = [Att]} = D} = couch_db:open_doc(Db, DocId, []),
    Content = couch_att:foldl_decode(Att, fun(A, Acc) -> Acc ++ A end, []),
    {Att, iolist_to_binary(Content)}.


doc_with_attach(Id) ->
    EJson = body_with_attach(Id),
    couch_doc:from_json_obj(EJson).

modify_doc(DbName, Id) ->
    {ok, Db} = couch_db:open_int(list_to_binary(DbName), []),
    {ok, #doc{revs = Revs, body = Body}} = couch_db:open_doc(Db, Id, [ejson_body]),
    RevStr = couch_doc:rev_to_str(Revs),
    EJson = couch_util:json_apply_field({<<"_rev">>, RevStr}, Body),
    Doc = couch_doc:from_json_obj(EJson),
    {ok, Rev} = couch_db:update_doc(Db, Doc, []),
    couch_db:close(Db),
    Rev.

body_with_attach(Id) ->
    {[
        {<<"_id">>, Id},
        {<<"_attachments">>, {[
            {?ATT_TXT_NAME, {[
                {<<"content_type">>, <<"text/plain">>},
                {<<"data">>, base64:encode(attach_data())}
            ]}
        }]}}
    ]}.

attach_data() ->
    %% We need file bigger than 4096 (gzipped)
    random:seed({1,2,3}),
    list_to_binary(lists:map(
        fun(_) -> $A + random:uniform(25) end, lists:seq(0, ?ATT_SIZE))).

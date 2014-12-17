-module(couch_db_api).

-export([validate_namespace/1, oneof/2]).

-type validation_result() :: {true, NS :: binary()}
    | {error, Reason :: term()}.

-spec validate_namespace(NS :: binary()) -> validation_result().
validate_namespace(NS) ->
    oneof(NS, [<<"_local">>, <<"_design">>]).


-spec oneof(Thing :: term(), Allowed :: [term()]) -> validation_result().
oneof(Thing, Allowed) ->
    case lists:any(fun(E) -> E == Thing end, Allowed) of
        true ->
            {true, Thing};
        false ->
            {error, [{expected, {oneof, Allowed}}, {got, Thing}]}
    end.

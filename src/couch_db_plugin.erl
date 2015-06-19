-module(couch_db_plugin).

-export([validate_dbname/2]).

-include_lib("couch/include/couch_db.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

validate_dbname(DbName, Normalized) ->
    Handle = couch_epi:get_handle(couch_db),
    %% callbacks return true only if it specifically allow the given Id
    couch_epi:any(Handle, couch_db, validate_dbname, [DbName, Normalized], [ignore_providers]).


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

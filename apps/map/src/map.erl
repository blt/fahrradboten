-module(map).

-export([path/2]).

-opaque vertex() :: map_srv:vertex().
-type path() :: [vertex()].

-export_type([vertex/0, path/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec path(A :: map_srv:vertex(),
           B :: map_srv:vertex()) -> {ok, map_srv:path()} | {error, no_path}.
path(A, B) ->
    map_srv:path(A, B).

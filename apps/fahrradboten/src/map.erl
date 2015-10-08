-module(map).

-export([path/2, distance/2, headquarters/0, verticies/0]).

-type vertex() :: map_srv:vertex().
-type path() :: [map_srv:vertex()].
-type distance() :: map_srv:distance().

-export_type([vertex/0, path/0, distance/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec path(A :: map:vertex(),
           B :: map:vertex()) -> {ok, {map:path(), map:distance()}}
                               | {error, no_path}.
path(A, B) -> map_srv:path(A, B).

-spec distance(A :: map:vertex(),
               B :: map:vertex()) -> {ok, map:distance()}
                                   | {error, no_direct_connection}.
distance(A, B) -> map_srv:distance(A, B).

-spec headquarters() -> map:vertex().
headquarters() -> map_srv:headquarters().

-spec verticies() -> [map:vertex()].
verticies() -> map_srv:verticies().

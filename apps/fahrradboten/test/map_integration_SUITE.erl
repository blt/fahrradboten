-module(map_integration_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([
         all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2,
         %% group: boot
         test_boot_map_srv/1,
         %% group: data
         test_load_map_srv/1,
         %% group: logic
         test_map_path/1, test_map_distance/1, test_headquarters/1
        ]).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() -> [
          {group, boot},
          {group, data},
          {group, logic}
         ].

groups() -> [
             {boot, [],
              [
               test_boot_map_srv
              ]
             },
             {data, [],
              [
               test_load_map_srv
              ]
             },
             {logic, [shuffle, {repeat_until_any_fail, 10}],
              [
               test_map_path,
               test_map_distance,
               test_headquarters
              ]
             }
            ].


init_per_suite(Config) ->
    stop_system(),
    start_system(),
    timer:sleep(1000),
    Config.

end_per_suite(_Config) ->
    stop_system(),
    ok.

init_per_testcase(_Suite, Config) ->
    Config.

end_per_testcase(_Suite, _Config) ->
    ok.

%%%===================================================================
%%% TESTS
%%%===================================================================

%%--------------------------------------------------------------------
%% Group : boot
%%--------------------------------------------------------------------

test_boot_map_srv(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Group : data
%%--------------------------------------------------------------------

test_load_map_srv(_Config) ->
    Verticies = lists:sort(map:verticies()),
    ?assertMatch([a,b,c], Verticies).

%%--------------------------------------------------------------------
%% Group : logic
%%--------------------------------------------------------------------

test_map_path(_Config) ->
    %% see map_sup.erl for default graph
    ?assertMatch({ok, {[], 0}},      map:path(a, a)),
    ?assertMatch({ok, {[a,c,b], 2}}, map:path(a, b)),
    ?assertMatch({ok, {[b,c], 1}},   map:path(b, c)),
    ?assertMatch({ok, {[a,c], 1}},   map:path(a, c)).

test_map_distance(_Config) ->
    ?assertMatch({error, no_direct_connection}, map:distance(a, b)),
    ?assertMatch({ok, 1}, map:distance(b, c)),
    ?assertMatch({ok, 1}, map:distance(a, c)).

test_headquarters(_Config) ->
    ?assertMatch(a, map:headquarters()).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

start_system() ->
    ok = application:start(fahrradboten).

stop_system() ->
    _ = application:stop(fahrradboten).

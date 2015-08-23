-module(map_integration_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([
         all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2,
         %% group: boot
         test_boot_map_srv/1,
         %% group: logic
         test_map_path/1
        ]).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() -> [
          {group, boot},
          {group, logic}
         ].

groups() -> [
             {boot, [],
              [
               test_boot_map_srv
              ]
             },
             {logic, [shuffle, {repeat_until_any_fail, 10}],
              [
               test_map_path
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
%% Group : logic
%%--------------------------------------------------------------------

test_map_path(_Config) ->
    %% see map_sup.erl for default graph
    ?assertMatch({ok, [a,c, b]}, map:path(a, b)),
    ?assertMatch({ok, [b,c]}, map:path(b, c)),
    ?assertMatch({ok, [a,c]}, map:path(a, c)).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

start_system() ->
    ok = application:start(map),
    ok.

stop_system() ->
    _ = application:stop(map).

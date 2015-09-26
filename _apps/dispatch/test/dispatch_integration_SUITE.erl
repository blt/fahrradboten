-module(dispatch_integration_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([
         all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2,
         %% group: boot
         test_boot_dispatch/1,
         %% group: logic
         test_clock_in_dispatch/1, test_location_dispatch/1
        ]).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() -> [
          {group, boot}
          %% , {group, logic}
         ].

groups() -> [
             {boot, [],
              [
               test_boot_dispatch
              ]
             },
             {logic, [],
              [
               test_clock_in_dispatch,
               test_location_dispatch
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

test_boot_dispatch(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Group : logic
%%--------------------------------------------------------------------

test_clock_in_dispatch(_Config) ->
    Messenger = <<"a_test_clock_in_dispatch_bicycle">>,

    ?assertMatch(ok, dispatch:clock_in(Messenger)).

test_location_dispatch(_Config) ->
    Messenger = <<"a_test_location_dispatch_bicycle">>,
    Headquarters = dispatch:headquarters(),

    ?assertMatch(ok, dispatch:clock_in(Messenger)),
    ?assertMatch({ok, {stationary, Headquarters}}, dispatch:location()).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

start_system() ->
    ok = application:start(orders),
    ok = application:start(map),
    ok = application:start(dispatch).

stop_system() ->
    _ = application:stop(map),
    _ = application:stop(orders),
    _ = application:stop(dispatch).

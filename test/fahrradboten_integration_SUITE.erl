-module(fahrradboten_integration_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([
         all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2,
         %% group: boot
         test_boot_fahrradboten/1
        ]).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() -> [
          {group, boot}
         ].

groups() -> [
             {boot, [],
              [
               test_boot_fahrradboten
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

test_boot_fahrradboten(_Confif) ->
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

start_system() ->
    ok = application:start(fahrradboten).

stop_system() ->
    _ = application:stop(fahrradboten).

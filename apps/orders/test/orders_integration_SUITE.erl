-module(orders_integration_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([
         all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2,
         %% group: boot
         test_boot_orders/1,
         %% group: logic
         test_submit_orders/1, test_cancel_orders/1
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
               test_boot_orders
              ]
             },
             {logic, [],
              [
               test_submit_orders,
               test_cancel_orders
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

test_boot_orders(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Group : logic
%%--------------------------------------------------------------------

test_submit_orders(_Config) ->
    OrderID = <<"test_submit_orders_id">>,
    PickupLocation = a,
    DropOffLocation = b,
    Worth = 10,

    %% order has not been created
    ?assertMatch({error, unknown_order_id}, orders:status(OrderID)),

    %% create order
    ?assertMatch(ok, orders:submit(OrderID, PickupLocation, DropOffLocation, Worth)),
    ?assertMatch({ok, waiting_assignment}, orders:status(OrderID)).

test_cancel_orders(_Config) ->
    OrderID = <<"test_cancel_orders_id">>,
    PickupLocation = a,
    DropOffLocation = b,
    Worth = 10,

    %% order has not been created
    ?assertMatch({error, unknown_order_id}, orders:status(OrderID)),

    %% can cancel what does not exist
    ?assertMatch(ok, orders:cancel(OrderID)),

    %% create order
    ?assertMatch(ok, orders:submit(OrderID, PickupLocation, DropOffLocation, Worth)),
    ?assertMatch({ok, waiting_assignment}, orders:status(OrderID)),

    %% can cancel what does exist
    ?assertMatch(ok, orders:cancel(OrderID)),

    %% order is gone
    ?assertMatch({error, unknown_order_id}, orders:status(OrderID)).


%%%===================================================================
%%% Internal Functions
%%%===================================================================

start_system() ->
    ok = application:start(orders).

stop_system() ->
    _ = application:stop(orders).

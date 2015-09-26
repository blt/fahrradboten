-module(orders_integration_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2,
         %% group: boot
         test_boot_orders/1,
         %% group: logic
         test_submit_orders/1, test_cancel_orders/1,
         test_details_orders/1, test_assign_orders/1,
         test_delivered_orders/1, test_available_orders/1
        ]).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() -> [{group, boot},
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
               test_cancel_orders,
               test_details_orders,
               test_assign_orders,
               test_delivered_orders,
               test_available_orders
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
    ?assertMatch({ok, waiting_assignment}, orders:status(OrderID)),

    %% cancel order
    ?assertMatch(ok, orders:cancel(OrderID)).

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

test_details_orders(_Config) ->
    OrderID = <<"test_details_orders_id">>,
    PickupLocation = a,
    DropOffLocation = b,
    Worth = 10,

    %% order has not been created
    ?assertMatch({error, unknown_order_id}, orders:details(OrderID)),

    %% create order
    ?assertMatch(ok, orders:submit(OrderID, PickupLocation, DropOffLocation, Worth)),
    ?assertMatch({ok, {PickupLocation, DropOffLocation, Worth}}, orders:details(OrderID)),

    %% can cancel what does exist
    ?assertMatch(ok, orders:cancel(OrderID)),

    %% order is gone
    ?assertMatch({error, unknown_order_id}, orders:details(OrderID)).

test_assign_orders(_Config) ->
    OrderID = <<"test_assign_orders_id">>,
    Messenger = <<"a_test_assign_bicycle">>,
    PickupLocation = a,
    DropOffLocation = b,
    Worth = 10,

    %% order has not been created
    ?assertMatch({error, unknown_order_id}, orders:status(OrderID)),

    %% create order
    ?assertMatch(ok, orders:submit(OrderID, PickupLocation, DropOffLocation, Worth)),
    ?assertMatch({ok, waiting_assignment}, orders:status(OrderID)),

    %% assign order
    ?assertMatch(ok, orders:assign(OrderID, Messenger)),

    %% determine that order has been assigned
    ?assertMatch({ok, {assigned, Messenger}}, orders:status(OrderID)),

    %% can cancel what does exist
    ?assertMatch(ok, orders:cancel(OrderID)),

    %% order is gone
    ?assertMatch({error, unknown_order_id}, orders:details(OrderID)).

test_delivered_orders(_Config) ->
    OrderID = <<"test_delivered_orders_id">>,
    Messenger = <<"a_test_delivered_bicycle">>,
    PickupLocation = a,
    DropOffLocation = b,
    Worth = 10,

    %% order has not been created
    ?assertMatch({error, unknown_order_id}, orders:status(OrderID)),

    %% create order
    ?assertMatch(ok, orders:submit(OrderID, PickupLocation, DropOffLocation, Worth)),
    ?assertMatch({ok, waiting_assignment}, orders:status(OrderID)),

    %% assign order
    ?assertMatch(ok, orders:assign(OrderID, Messenger)),

    %% determine that order has been assigned
    ?assertMatch({ok, {assigned, Messenger}}, orders:status(OrderID)),

    %% deliver the order
    ?assertMatch(ok, orders:delivered(OrderID)),

    %% ensure status change
    ?assertMatch({ok, delivered}, orders:status(OrderID)).

test_available_orders(_Config) ->
    OrderID0 = <<"test_available_orders_id_0">>,
    OrderID1 = <<"test_available_orders_id_1">>,
    OrderID2 = <<"test_available_orders_id_2">>,
    PickupLocation = a,
    DropOffLocation = b,
    Worth = 10,

    %% order has not been created
    ?assertMatch({error, unknown_order_id}, orders:status(OrderID0)),
    ?assertMatch({error, unknown_order_id}, orders:status(OrderID1)),
    ?assertMatch({error, unknown_order_id}, orders:status(OrderID2)),

    %% create order
    ?assertMatch(ok, orders:submit(OrderID0, PickupLocation, DropOffLocation, Worth)),
    ?assertMatch(ok, orders:submit(OrderID1, PickupLocation, DropOffLocation, Worth)),
    ?assertMatch(ok, orders:submit(OrderID2, PickupLocation, DropOffLocation, Worth)),

    ?assertMatch({ok, waiting_assignment}, orders:status(OrderID0)),
    ?assertMatch({ok, waiting_assignment}, orders:status(OrderID1)),
    ?assertMatch({ok, waiting_assignment}, orders:status(OrderID2)),

    %% check all available
    ?assertMatch({ok, [OrderID2, OrderID1, OrderID0]}, orders:available()).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

start_system() ->
    ok = application:start(fahrradboten).

stop_system() ->
    _ = application:stop(fahrradboten).

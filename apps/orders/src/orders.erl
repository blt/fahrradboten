-module(orders).

-export([submit/4, status/1, cancel/1, details/1, assign/2, delivered/1, available/0]).

-type order_id() :: binary().

-export_type([order_id/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec submit(OrderID :: order_id(),
             PickupLocation :: map:vertex(),
             DropOffLocation :: map:vertex(),
             Worth :: billing:worth()) -> ok.
submit(OrderID, PickupLocation, DropOffLocation, Worth) ->
    orders_srv:submit(OrderID, PickupLocation, DropOffLocation, Worth).

-spec details(OrderID :: order_id()) -> {ok, {PickupLocation :: map:vertex(),
                                              DropOffLocation :: map:vertex(),
                                              Worth :: billing:worth()}}
                                            | {error, unknown_order_id}.
details(OrderID) ->
    orders_srv:details(OrderID).

-spec assign(OrderID :: order_id(),
             Messenger :: dispatch:messenger()) -> ok.
assign(OrderID, Messenger) ->
    orders_srv:assign(OrderID, Messenger).

-spec delivered(OrderID :: order_id()) -> ok.
delivered(OrderID) ->
    orders_srv:delivered(OrderID).

-spec status(OrderID :: order_id()) -> {ok,
                                        waiting_assignment |
                                        {assigned, dispatch:messenger()} |
                                        delivered
                                       } |
                                       {error, unknown_order_id}.
status(OrderID) ->
    orders_srv:status(OrderID).

-spec cancel(OrderID :: order_id()) -> ok.
cancel(OrderID) ->
    orders_srv:cancel(OrderID).

-spec available() -> [order_id()].
available() ->
    orders_srv:available().

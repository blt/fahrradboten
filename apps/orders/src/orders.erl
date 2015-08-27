-module(orders).

-export([submit/4, status/1, cancel/1]).

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

-spec status(OrderID :: order_id()) -> {ok,
                                        waiting_assignment |
                                        {assigned, dispatch:messenger()}
                                       } |
                                       {error, unknown_order_id}.
status(OrderID) ->
    orders_srv:status(OrderID).

-spec cancel(OrderID :: order_id()) -> ok.
cancel(OrderID) ->
    orders_srv:cancel(OrderID).

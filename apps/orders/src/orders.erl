-module(orders).

-export([submit/4, status/1]).

-type order_id() :: binary().

-export_type([order_id/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec submit(OrderID :: order_id(),
             PickupLocation :: map:vertex(),
             DropOffLocation :: map:vertex(),
             Worth :: billing:worth()) -> ok.
submit(_OrderID, _PickupLocation, _DropOffLocation, _Worth) ->
    ok.

-spec status(OrderID :: order_id()) -> {ok,
                                        waiting_assignment |
                                        {assigned, dispatch:messenger()}
                                       } |
                                       {error, unknown_order_id}.
status(_OrderID) ->
    {error, unknown_order_id}.

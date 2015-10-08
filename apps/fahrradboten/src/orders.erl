-module(orders).

-export([submit/4,
         status/1,
         cancel/1,
         details/1,
         assign/2,
         all_ids/0,
         delivered/1,
         available/0]).

-type order_id() :: binary().

-export_type([order_id/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec all_ids() -> {ok, [order_id()]}.
all_ids() -> orders_srv:all_ids().

%%--------------------------------------------------------------------
%% Submit an order
%%--------------------------------------------------------------------

-spec submit(OrderID :: order_id(),
             PickupLocation :: map:vertex(),
             DropOffLocation :: map:vertex(),
             Worth :: billing:worth()) -> ok.
submit(OrderID, PickupLocation, DropOffLocation, Worth) ->
    orders_srv:submit(OrderID, PickupLocation, DropOffLocation, Worth).

%%--------------------------------------------------------------------
%% Return order details
%%--------------------------------------------------------------------

-spec details(OrderID :: order_id()) -> {ok, {PickupLocation :: map:vertex(),
                                              DropOffLocation :: map:vertex(),
                                              Worth :: billing:worth()}}
                                            | {error, unknown_order_id}.
details(OrderID) ->
    orders_srv:details(OrderID).

%%--------------------------------------------------------------------
%% Assign an order to a messenger
%%--------------------------------------------------------------------

-spec assign(OrderID :: order_id(),
             Messenger :: dispatch:messenger()) -> ok.

assign(OrderID, Messenger) ->
    orders_srv:assign(OrderID, Messenger).

%%--------------------------------------------------------------------
%% Mark a order as delivered
%%--------------------------------------------------------------------

-spec delivered(OrderID :: order_id()) -> ok.

delivered(OrderID) ->
    orders_srv:delivered(OrderID).

%%--------------------------------------------------------------------
%% Return order status
%%--------------------------------------------------------------------

-spec status(OrderID :: order_id()) -> {ok,
                                        waiting_assignment |
                                        {assigned, dispatch:messenger()} |
                                        delivered
                                       } |
                                       {error, unknown_order_id}.
status(OrderID) ->
    orders_srv:status(OrderID).

%%--------------------------------------------------------------------
%% Cancel an order
%%--------------------------------------------------------------------

-spec cancel(OrderID :: order_id()) -> ok.

cancel(OrderID) ->
    orders_srv:cancel(OrderID).

%%--------------------------------------------------------------------
%% Return available orders
%%--------------------------------------------------------------------

-spec available() -> {ok, [order_id()]}.

available() ->
    orders_srv:available().

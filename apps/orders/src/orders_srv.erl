-module(orders_srv).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([submit/4, status/1, cancel/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(order, {
          order_id         :: undefined | orders:order_id(),
          pickup_location  :: undefined | map:vertex(),
          dropoff_location :: undefined | map:vertex(),
          worth            :: undefined | billing:worth()
         }).

-record(state, {
          orders = [] :: [#order{}]
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

submit(OrderID, PickupLocation, DropOffLocation, Worth) ->
    Order = #order{
               order_id = OrderID,
               pickup_location = PickupLocation,
               dropoff_location = DropOffLocation,
               worth = Worth
              },
    gen_server:call(?MODULE, {submit, Order}, timer:seconds(5)).

status(OrderID) ->
    gen_server:call(?MODULE, {status, OrderID}, timer:seconds(5)).

cancel(OrderID) ->
    gen_server:call(?MODULE, {cancel, OrderID}, timer:seconds(5)).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({submit, Order}, _From, State = #state{orders = OrderList}) ->
    NewOrderList = [Order | OrderList],
    {reply, ok, State#state{orders = NewOrderList}};
handle_call({cancel, OrderID}, _From, State = #state{orders = OrderList}) ->
    NewOrderList = lists:keydelete(OrderID, 2, OrderList),
    {reply, ok, State#state{orders = NewOrderList}};
handle_call({status, OrderID}, _From, State = #state{orders = OrderList}) ->
    %% Note: does not handle concept of being assigned by dispatch to a
    %% messenger
    Resp = case lists:keyfind(OrderID, 2, OrderList) of
               false -> {error, unknown_order_id};
               _Other -> {ok, waiting_assignment}
           end,
    {reply, Resp, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

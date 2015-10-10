-module(orders_srv).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([submit/4, status/1, cancel/1, available/0, delivered/1, details/1, assign/2, all_ids/0, total_ids/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(order, {
          order_id           :: undefined | orders:order_id(),
          status = available :: available | completed | in_progress,
          pickup_location    :: undefined | map:vertex(),
          dropoff_location   :: undefined | map:vertex(),
          messenger          :: undefined | dispatch:messenger(),
          worth              :: undefined | billing:worth()
         }).

-record(state, {
          orders = [] :: [#order{}],
          total = 0   :: integer()
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

all_ids() ->
    gen_server:call(?MODULE, all_ids, timer:seconds(5)).

total_ids() ->
    gen_server:call(?MODULE, total_ids, timer:seconds(5)).

submit(OrderID, PickupLocation, DropOffLocation, Worth) ->
    Order = #order{
               status = available,
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

available() ->
    gen_server:call(?MODULE, available, timer:seconds(5)).

delivered(OrderID) ->
    gen_server:call(?MODULE, {delivered, OrderID}, timer:seconds(5)).

details(OrderID) ->
    gen_server:call(?MODULE, {details, OrderID}, timer:seconds(5)).

assign(OrderID, Messenger) ->
    gen_server:call(?MODULE, {assign, OrderID, Messenger}, timer:seconds(5)).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(all_ids, _From, State = #state{orders = OrderList}) ->
    OrderIDs = lists:map(fun(#order{order_id = ID}) -> ID end, OrderList),
    {reply, {ok, OrderIDs}, State};
handle_call(total_ids, _From, State = #state{total = Total}) ->
    {reply, {ok, Total}, State};
handle_call({assign, OrderID, Messenger}, _From, State = #state{orders = OrderList}) ->
    NewOrderList =
        case lists:keyfind(OrderID, 2, OrderList) of
            false ->
                OrderList;
            Order ->
                lists:keyreplace(
                  OrderID, 2, OrderList,
                  Order#order {
                    status = in_progress,
                    messenger = Messenger
                   })
        end,
    {reply, ok, State#state{orders = NewOrderList}};
handle_call({details, OrderID}, _From, State = #state{orders = OrderList}) ->
    Resp = case lists:keyfind(OrderID, 2, OrderList) of
               false ->
                   {error, unknown_order_id};
               Order ->
                   {ok,
                    {
                      Order#order.pickup_location,
                      Order#order.dropoff_location,
                      Order#order.worth
                    }
                   }
           end,
    {reply, Resp, State};
handle_call({delivered, OrderID}, _From, State = #state{orders = OrderList}) ->
    NewOrderList = case lists:keyfind(OrderID, 2, OrderList) of
                       false ->
                           OrderList;
                       Order ->
                           lists:keyreplace(OrderID, 2, OrderList,
                                            Order#order{status = completed})
                   end,
    {reply, ok, State#state{orders = NewOrderList}};
handle_call(available, _From, State = #state{orders = OrderList}) ->
    AvailableOrders = lists:filter(fun
                                       (#order{status = available}) -> true;
                                       (_)                          -> false
                                   end, OrderList),
    OrderIDS = lists:map(fun(#order{order_id = ID}) -> ID end, AvailableOrders),
    {reply, {ok, OrderIDS}, State};
handle_call({submit, Order}, _From, State = #state{orders = OrderList, total = Total}) ->
    NewOrderList = [Order | OrderList],
    {reply, ok, State#state{orders = NewOrderList, total = Total + 1}};
handle_call({cancel, OrderID}, _From, State = #state{orders = OrderList}) ->
    NewOrderList = lists:keydelete(OrderID, 2, OrderList),
    {reply, ok, State#state{orders = NewOrderList}};
handle_call({status, OrderID}, _From, State = #state{orders = OrderList}) ->
    %% Note: does not handle concept of being assigned by dispatch to a
    %% messenger
    Resp = case lists:keyfind(OrderID, 2, OrderList) of
               false -> {error, unknown_order_id};
               Order ->
                   case Order#order.status of
                       available ->
                           {ok, waiting_assignment};
                       completed ->
                           {ok, delivered};
                       in_progress ->
                           {ok, {assigned, Order#order.messenger}}
                   end
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

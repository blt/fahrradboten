-module(dispatch_srv).

-behaviour(gen_server).

%% API
-export([start_link/0, clock_in/1, delivered/2, location/1, poll_for_orders/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-type messenger_tpl() :: {dispatch:messenger(), pid()}.

-record(state, {
          order_poll_tref        :: timer:tref() | undefined,

          messengers        = [] :: [messenger_tpl()],
          free_messengers   = [] :: [messenger_tpl()],
          active_messengers = [] :: [messenger_tpl()]
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% creates a Messenger if it doesn't already exist
clock_in(Messenger) ->
    gen_server:call(?MODULE, {clock_in, Messenger}, timer:seconds(5)).

delivered(Messenger, OrderID) ->
    gen_server:call(?MODULE, {delivered, Messenger, OrderID}, timer:seconds(5)).

location(Messenger) ->
    gen_server:call(?MODULE, {location, Messenger}, timer:seconds(5)).

poll_for_orders() ->
    case orders:available() of
        {ok, []} -> ok;
        {ok, Available} ->
            process_orders(Available)
    end.

-spec process_orders([orders:order_id()]) -> ok.
process_orders(Orders) ->
    gen_server:cast(?MODULE, {process, Orders}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, OrderPollTRef} = timer:apply_interval(100, ?MODULE, poll_for_orders, []),
    State = #state{
               order_poll_tref = OrderPollTRef
              },
    {ok, State}.

handle_call({delivered, Messenger, OrderID}, _From, State = #state{
                                                               free_messengers = FM,
                                                               active_messengers = AM
                                                              }) ->
    Tup = {Messenger, _Pid} = lists:keyfind(Messenger, 1, AM),
    Actives = lists:keydelete(Messenger, 1, AM),
    NewState = State#state{
                 active_messengers = Actives,
                 free_messengers = [Tup|FM]
                },
    ok = orders:delivered(OrderID),
    {reply, ok, NewState};
handle_call({location, Messenger}, _From, State) ->
    case lists:keyfind(Messenger, 1, State#state.messengers) of
        {Messenger, Pid} ->
            {reply, {ok, dispatch_messenger:location(Pid)}, State};
        false ->
            {reply, {error, unknown}}
    end;
handle_call({clock_in, Messenger}, _From, State = #state{messengers = M,
                                                         free_messengers = FM}) ->
    case lists:keyfind(Messenger, 1, M) of
        {Messenger, _Pid} = Tup ->
            {reply, ok, State#state{free_messengers = [Tup|FM]}};
        false ->
            {ok, Pid} = dispatch_messenger:start(Messenger),
            _ = erlang:monitor(process, Pid),
            Tup = {Messenger, Pid},
            {reply, ok, State#state{messengers = [Tup|M],
                                    free_messengers = [Tup|FM]}}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({process, Orders}, State = #state{free_messengers = FM,
                                              active_messengers = AM}) ->
    OrderMessengerPairs = zip(Orders, FM),
    lists:foreach(fun({OrderID, {MessengerID, MessengerPid}}) ->
                          ok = orders:assign(OrderID, MessengerID),
                          ok = dispatch_messenger:work(MessengerPid, OrderID)
                  end, OrderMessengerPairs),
    BusyMessengers = [M || {_Order, M} <- OrderMessengerPairs],
    NewState = State#state{
                 active_messengers = BusyMessengers ++ AM,
                 free_messengers   = subtract(FM, BusyMessengers)
                 },
    {noreply, NewState};
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

subtract(L, []) ->
    L;
subtract(L, [X|Rest]) ->
    subtract(lists:delete(X, L), Rest).

zip(A, B) -> zip(A, B, []).

zip([], _, Acc) -> Acc;
zip(_, [], Acc) -> Acc;
zip([X|RestA], [Y|RestB], Acc) ->
    zip(RestA, RestB, [{X,Y} | Acc]).

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

zip_test_() ->
    [
     ?_assertMatch([],              zip([], not_used))
     , ?_assertMatch([],            zip(not_used, []))
     , ?_assertMatch([{1,1}],       zip([1], [1]))
     , ?_assertMatch([{1,2},{2,1}], zip([2,1], [1,2]))
    ].

-endif.

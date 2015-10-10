-module(overseer_wrkr).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          start_time = erlang:system_time(milli_seconds)                   :: non_neg_integer(),
          order_id   = integer_to_binary(erlang:system_time(nano_seconds)) :: binary(),
          fsm_state  = start :: start | polling_order | reporting,
          pickup  :: map:vertex() | undefined,
          dropoff :: map:vertex() | undefined
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    [To, From | _] = shuffle_lst(map:verticies()),
    State = #state{
               pickup = To,
               dropoff = From
              },
    {ok, State, 0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(report, State = #state{start_time = ST, fsm_state=reporting}) ->
    Now = erlang:system_time(milli_seconds),
    Diff = (Now - ST) / 1000,
    io:format(user, "DONE! Latency (sec): ~p~n", [Diff]),
    {stop, normal, State};
handle_info(timeout, State = #state{fsm_state = polling_order, order_id = OrderID}) ->
    NewState = case orders:status(OrderID) of
                   {ok, delivered} ->
                       {ok, _} = timer:send_after(0, report),
                       State#state{fsm_state = reporting};
                   {ok, waiting_assignment} ->
                       {ok, _} = timer:send_after(100, timeout),
                       State;
                   {ok, {assigned, _}} ->
                       {ok, _} = timer:send_after(100, timeout),
                       State
               end,
    {noreply, NewState};
handle_info(timeout, State = #state{fsm_state = start, order_id = OrderID, pickup = P, dropoff = D}) ->
    orders:submit(OrderID, P, D, 1000),
    {ok, _} = timer:send_after(100, timeout),
    {noreply, State#state{fsm_state = polling_order}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

shuffle_lst(L) ->
    [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- L])].

%%%===================================================================
%%% Internal Functions
%%%===================================================================

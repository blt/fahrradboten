-module(overseer_srv).

-behaviour(gen_server).

%% API
-export([start_link/0, start_workers/1, sustain_workers/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          sustain       = 0 :: non_neg_integer(),
          total_workers = 0 :: non_neg_integer()
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    Messengers = application:get_env(overseer, messengers, []),
    lists:foreach(fun(M) -> dispatch:clock_in(M) end, Messengers),

    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_workers(N) when is_integer(N) ->
    gen_server:call(?MODULE, {create, 0, N}, timer:seconds(5)).

sustain_workers(N) when is_integer(N) ->
    gen_server:call(?MODULE, {create, N, N}, timer:seconds(5)).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({create, Sus, N}, _From, State = #state{sustain = S, total_workers = TW}) ->
    create_workers(N),
    {reply, ok, State#state{sustain = S + Sus, total_workers = TW+N}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, normal},
            State = #state{sustain = S, total_workers=TW}) ->
    NewTW = TW-1,
    NewState = case NewTW < S of
                   true ->
                       create_workers(S - NewTW),
                       State#state{total_workers = S};
                   false ->
                       State#state{total_workers = NewTW}
               end,
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_workers(N) when N > 0 ->
    lists:foreach(fun(_) ->
                          {ok, _Pid} = overseer_wrkr:start_link()
                  end, lists:seq(1, N)).

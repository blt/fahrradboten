-module(overseer_srv).

-behaviour(gen_server).

%% API
-export([start_link/0, start_workers/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
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
    gen_server:call(?MODULE, {create, N}, timer:seconds(5)).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({create, N}, _From, State = #state{total_workers = TW}) ->
    lists:foreach(fun(_) ->
                          {ok, _Pid} = overseer_wrkr:start_link()
                  end, lists:seq(1, N)),
    {reply, ok, State#state{total_workers = TW+N}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, _Pid, work_completed}, State = #state{total_workers=TW}) ->
    {noreply, State#state{total_workers = TW-1}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

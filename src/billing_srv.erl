-module(billing_srv).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([
         open/1, credit/2, debit/2, balance/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

-define(TBL, billing_srv_tbl).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

open(Account) ->
    true = ets:insert(?TBL, [{account, Account, 0}]),
    ok.

credit(Account, Amount) ->
    try
        Amnt = ets:update_counter(?TBL, Account, {3, Amount}),
        {ok, Amnt}
    catch
        _:badarg ->
            {error, unknown_account}
    end.

debit(Account, Amount) ->
    case ets:lookup(?TBL, Account) of
        [{account, Account, Amnt}] ->
            case (Amnt - Amount) < 0 of
                true  -> {error, insufficient_balance};
                false -> {ok, ets:update_counter(?TBL, Account, {3,  (-1 * Amount)})}
            end;
        [] ->
            {error, unknown_account}
    end.

balance(Account) ->
    case ets:lookup(?TBL, Account) of
        [{account, Account, Amnt}] ->
            {ok, Amnt};
        [] ->
            {error, unknown_account}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ets:new(?TBL, [public, named_table, {keypos, 2}]),
    {ok, #state{}}.

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

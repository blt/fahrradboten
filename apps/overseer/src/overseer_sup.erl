-module(overseer_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    OverseerSrv = {overseer_srv,
                   {overseer_srv, start_link, []},
                   permanent, 5000, worker, dynamic},

    {ok, {{one_for_one, 1000, 3600}, [OverseerSrv]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

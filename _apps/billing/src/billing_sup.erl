%%%-------------------------------------------------------------------
%% @doc billing top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('billing_sup').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    BillingSrv = {billing_srv,
                  {billing_srv, start_link, []},
                  permanent, 5000, worker, dynamic},

    {ok, { {one_for_all, 100, 1}, [BillingSrv]} }.

%%====================================================================
%% Internal functions
%%====================================================================

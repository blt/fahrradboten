-module(fahrradboten_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    BillingSrv = {billing_srv,
                  {billing_srv, start_link, []},
                  permanent, 5000, worker, dynamic},

    OrdersSrv = {orders_srv,
                 {orders_srv, start_link, []},
                 permanent, 5000, worker, dynamic},

    DispatchSrv = {dispatch_srv,
                   {dispatch_srv, start_link, []},
                   permanent, 5000, worker, dynamic},

    MapSrv = {map_srv,
              {map_srv, start_link, [graph_config()]},
              permanent, 5000, worker, dynamic},

    Services = [MapSrv, BillingSrv, OrdersSrv, DispatchSrv],
    {ok, {{one_for_one, 100, 1}, Services}}.

%%====================================================================
%% Internal functions
%%====================================================================

graph_config() ->
    case application:get_env(fahrradboten, graph) of
        {ok, Config} -> Config;
        undefined -> default_graph_config()
    end.

default_graph_config() ->
    [
     {verticies, [a, b, c]},
     {edges, [{{a,c},1}, {{b,c},1}]},
     {headquarters, a}
    ].

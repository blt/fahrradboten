%%%-------------------------------------------------------------------
%% @doc map top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('map_sup').

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
    {ok, GraphConfig} = application:get_env(map, graph, {ok, [{verticies, [a, b,c]},
                                                              {edges, [
                                                                       {a,c},
                                                                       {b,c},
                                                                       {c, a}
                                                                      ]}
                                                              ]}),
    MapSrv = {map_srv,
              {map_srv, start_link, [GraphConfig]},
              permanent, 5000, worker, dynamic},

    {ok, { {one_for_all, 100, 1}, [MapSrv]} }.

%%====================================================================
%% Internal functions
%%====================================================================

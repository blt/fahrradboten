%%%-------------------------------------------------------------------
%% @doc fahrradboten top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('fahrradboten_sup').

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
    Services = [map(), billing(), orders()],
    {ok, {{one_for_all, 100, 1}, Services}}.

%%====================================================================
%% Internal functions
%%====================================================================

map() ->
    {map_srv,
     {map_srv, start_link, [graph_config()]},
     permanent, 5000, worker, dynamic}.

graph_config() ->
    case application:get_env(fahrradboten, graph) of
        {ok, Config} -> Config;
        undefined -> default_graph_config()
    end.

default_graph_config() ->
    [{verticies, [a, b, c]},
     {edges, [{{a,c},1}, {{b,c},1}]},
     {headquarters, a}].

billing() ->
    {billing_srv,
     {billing_srv, start_link, []},
     permanent, 5000, worker, dynamic}.

orders() ->
    {orders_srv,
     {orders_srv, start_link, []},
     permanent, 5000, worker, dynamic}.

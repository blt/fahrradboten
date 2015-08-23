-module(map_srv).

-behaviour(gen_server).

%% API
-export([start_link/1, path/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          graph = digraph:new([]) :: digraph:graph()
         }).

-opaque vertex() :: digraph:vertex().
-type path() :: [vertex()].

-export_type([vertex/0, path/0]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(GraphConfig) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [GraphConfig], []).

-spec path(A :: vertex(),
           B :: vertex()) -> {ok, path()} | {error, no_path}.
path(A, B) ->
    case gen_server:call(?MODULE, {path, A, B}, timer:seconds(5)) of
        {ok, Path} -> {ok, Path};
        false -> {error, no_path}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([GraphConfig]) ->
    State = #state{},
    interp(State#state.graph, GraphConfig),
    {ok, State}.

handle_call({path, A, B}, _From, State) ->
    {reply, {ok, digraph:get_path(State#state.graph, A, B)}, State};
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

interp(Graph, [{edges, Edges}, {verticies, Verticies}]) ->
    interp(Graph, [{verticies, Verticies}, {edges, Edges}]);
interp(Graph, [{verticies, Verticies}, {edges, Edges}]) ->
    lists:foreach(fun(Vertex) -> digraph:add_vertex(Graph, Vertex) end, Verticies),
    lists:foreach(fun({To, From}) ->
                          digraph:add_edge(Graph, To, From),
                          digraph:add_edge(Graph, From, To)
                  end, Edges).


%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

interp_test_() ->
    Graph = digraph:new([protected]),
    GraphConfig = [
                   {verticies, [a, b, c, d, e]},
                   {edges, [
                            {a, b},
                            {b, c},
                            {a, d},
                            {d, e},
                            {b, e}
                           ]}
                  ],

    ok = interp(Graph, GraphConfig),

    [
     ?_assertMatch([a,b,c,d,e], lists:sort(digraph:vertices(Graph)))

    , ?_assertMatch([b, d], lists:sort(digraph:out_neighbours(Graph, a)))
    , ?_assertMatch([a, c, e], lists:sort(digraph:out_neighbours(Graph, b)))
    , ?_assertMatch([b], lists:sort(digraph:out_neighbours(Graph, c)))
    , ?_assertMatch([a, e], lists:sort(digraph:out_neighbours(Graph, d)))
    , ?_assertMatch([b, d], lists:sort(digraph:out_neighbours(Graph, e)))

    , ?_assertMatch([a, d, e, b], digraph:get_path(Graph, a, b))
    , ?_assertMatch([a, d, e, b, c], digraph:get_path(Graph, a, c))
    , ?_assertMatch([a, d], digraph:get_path(Graph, a, d))
    , ?_assertMatch([a, d, e], digraph:get_path(Graph, a, e))

    , ?_assertMatch([b, e, d, a], digraph:get_path(Graph, b, a))
    , ?_assertMatch([b, c], digraph:get_path(Graph, b, c))
    , ?_assertMatch([b, e, d], digraph:get_path(Graph, b, d))
    , ?_assertMatch([b, e], digraph:get_path(Graph, b, e))

    , ?_assertMatch([c, b, e, d, a], digraph:get_path(Graph, c, a))
    , ?_assertMatch([c, b], digraph:get_path(Graph, c, b))
    , ?_assertMatch([c, b, e, d], digraph:get_path(Graph, c, d))
    , ?_assertMatch([c, b, e], digraph:get_path(Graph, c, e))

    , ?_assertMatch([d, e, b, a], digraph:get_path(Graph, d, a))
    , ?_assertMatch([d, e, b], digraph:get_path(Graph, d, b))
    , ?_assertMatch([d, e, b, c], digraph:get_path(Graph, d, c))
    , ?_assertMatch([d, e], digraph:get_path(Graph, d, e))

    , ?_assertMatch([e, b, a], digraph:get_path(Graph, e, a))
    , ?_assertMatch([e, b], digraph:get_path(Graph, e, b))
    , ?_assertMatch([e, b, c], digraph:get_path(Graph, e, c))
    , ?_assertMatch([e, b, a, d], digraph:get_path(Graph, e, d))
    ].




-endif.

-module(map_srv).

-behaviour(gen_server).

%% API
-export([start_link/1, path/2, distance/2, headquarters/0, verticies/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          headquarters :: map:vertex(),
          graph = digraph:new([]) :: digraph:graph()
         }).

-type vertex() :: digraph:vertex().
-type path() :: [vertex()].
-type distance() :: non_neg_integer().

-export_type([vertex/0, path/0, distance/0]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(GraphConfig) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [GraphConfig], []).

-spec path(A :: vertex(),
           B :: vertex()) -> {ok, {path(), distance()}} | {error, no_path}.
path(A, B) ->
    case gen_server:call(?MODULE, {path, A, B}, timer:seconds(5)) of
        {ok, Path} -> {ok, Path};
        false -> {error, no_path}
    end.

-spec distance(A :: vertex(),
               B :: vertex()) -> {ok, distance()} | {error, no_direct_connection}.
distance(A, B) ->
    case gen_server:call(?MODULE, {distance, A, B}, timer:seconds(5)) of
        {ok, Path} -> {ok, Path};
        false -> {error, no_direct_connection}
    end.

headquarters() ->
    {ok, Headquarters} = gen_server:call(?MODULE, headquarters, timer:seconds(5)),
    Headquarters.

verticies() ->
    {ok, Verticies} = gen_server:call(?MODULE, verticies, timer:seconds(5)),
    Verticies.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([GraphConfig]) ->
    {headquarters, HQ} = lists:keyfind(headquarters, 1, GraphConfig),
    State = #state{headquarters = HQ},
    interp(State#state.graph, GraphConfig),
    {ok, State}.

handle_call(verticies, _From, State) ->
    Graph = State#state.graph,
    {reply, {ok, digraph:vertices(Graph)}, State};
handle_call(headquarters, _From, State = #state{headquarters = HQ}) ->
    {reply, {ok, HQ}, State};
handle_call({path, A, A}, _From, State) ->
    Resp = {ok, {[A], 0}},
    {reply, Resp, State};
handle_call({path, A, B}, _From, State) ->
    Graph = State#state.graph,
    Resp = case digraph:get_path(Graph, A, B) of
               false -> {error, no_path};
               Path  -> {ok, {Path, distance(Graph, Path, 0)}}
           end,
    {reply, Resp, State};
handle_call({distance, A, B}, _From, State) ->
    Graph = State#state.graph,
    Repl = case digraph:edge(Graph, {A,B}) of
               {{A,B}, A, B, Weight} ->
                   {ok, Weight};
               false ->
                   false
           end,
    {reply, Repl, State};
handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.

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

distance(G, [A,B | _] = Pth, Acc) ->
    {{A,B}, A, B, Weight} = digraph:edge(G, {A,B}),
    distance(G, lists:nthtail(1, Pth), Acc+Weight);
distance(_G, [_], Acc) ->
    Acc;
distance(_G, [], Acc) ->
    Acc.

interp(Graph, [{verticies, Verticies}, {edges, Edges}, {headquarters, _HQ}]) ->
    lists:foreach(fun(Vertex) -> digraph:add_vertex(Graph, Vertex) end, Verticies),
    lists:foreach(fun({{To, From}, Weight}) ->
                          digraph:add_edge(Graph, {To, From}, To, From, Weight),
                          digraph:add_edge(Graph, {From, To}, From, To, Weight)
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
                            {{a, b}, 1},
                            {{b, c}, 1},
                            {{a, d}, 1},
                            {{d, e}, 1},
                            {{b, e}, 1}
                           ]},
                   {headquarters, a}
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

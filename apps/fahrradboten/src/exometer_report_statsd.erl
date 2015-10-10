-module(exometer_report_statsd).
-behaviour(exometer_report).

-include_lib("kernel/include/inet.hrl").

%% gen_server callbacks
-export(
   [
    exometer_init/1,
    exometer_info/2,
    exometer_cast/2,
    exometer_call/3,
    exometer_report/5,
    exometer_subscribe/5,
    exometer_unsubscribe/4,
    exometer_newentry/2,
    exometer_setopts/4,
    exometer_terminate/2
   ]).

-define(DEFAULT_HOST, "localhost").
-define(DEFAULT_PORT, 8125).

-record(st, {socket  :: inet:socket(),
             address :: inet:ip_address(),
             port    :: inet:port_number(),
             type_map :: [{list(atom()), atom()}]}).

%%%===================================================================
%%% Probe callbacks
%%%===================================================================

exometer_init(Opts) ->
    {ok, Host} = inet:gethostbyname(get_opt(hostname, Opts, ?DEFAULT_HOST)),
    [IP|_]     = Host#hostent.h_addr_list,
    AddrType   = Host#hostent.h_addrtype,
    Port       = get_opt(port, Opts, ?DEFAULT_PORT),
    TypeMap    = get_opt(type_map, Opts, []),

    case gen_udp:open(0, [AddrType]) of
        {ok, Sock} ->
            {ok, #st{socket=Sock, address=IP, port=Port, type_map=TypeMap}};
        {error, _} = Error ->
            Error
    end.


exometer_report(_, _, _, undefined, St) ->
    {ok, St};

exometer_report(Metric, DataPoint, _Extra, Value, #st{} = St) ->
    Name = name(Metric, DataPoint),
    Type = type(gauge),
    Line = line(Name, Value, Type, tags(Metric)),
    _ = gen_udp:send(St#st.socket, St#st.address, St#st.port, Line),
    {ok, St}.

exometer_subscribe(_Metric, _DataPoint, _Extra, _Interval, St) ->
    {ok, St}.

exometer_unsubscribe(_Metric, _DataPoint, _Extra, St) ->
    {ok, St}.

exometer_call(_Unknown, _From, St) ->
    {ok, St}.

exometer_cast(_Unknown, St) ->
    {ok, St}.

exometer_info(_Unknown, St) ->
    {ok, St}.

exometer_newentry(_Entry, St) ->
    {ok, St}.

exometer_setopts(_Metric, _Options, _Status, St) ->
    {ok, St}.

exometer_terminate(_, _) ->
    ignore.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

line(Name, Value, Type, [])->
    [Name, ":", value(Value), "|", type(Type)];
line(Name, Value, Type, Tags)->
    [Name, ":", value(Value), "|", type(Type), "|#", Tags].


get_opt(K, Opts, Def) ->
    exometer_util:get_opt(K, Opts, Def).

type(gauge) -> "g";
type(Other) -> Other.

ets_key(Metric, DataPoint) -> Metric ++ [ DataPoint ].

name(Metric, DataPoint) ->
    Metric0 = lists:takewhile(fun(Elem)-> not is_tuple(Elem) end, Metric),
    intersperse(".", lists:map(fun value/1, ets_key(Metric0, DataPoint))).

tags(Metric)->
    Tags = lists:dropwhile(fun(Elem)-> not is_tuple(Elem) end, Metric),
    intersperse(",", [intersperse(":", lists:map(fun value/1, [K,V])) || {K,V} <- Tags]).

value(X) when is_atom(X)    -> atom_to_list(X);
value(X) when is_integer(X) -> integer_to_list(X);
value(X) when is_float(X)   -> io_lib:format("~.6f", [X]);
value(X) when is_binary(X)  -> X;
value(X) when is_list(X)    -> X.

intersperse(_, [])         -> [];
intersperse(_, [X])        -> [X];
intersperse(Sep, [X | Xs]) -> [X, Sep | intersperse(Sep, Xs)].

-module(dispatch).

-export([location/1, clock_in/1, delivered/2, headquarters/0]).

-type messenger() :: binary().

-export_type([messenger/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec clock_in(Messenger :: messenger()) -> ok.
clock_in(Messenger) ->
    dispatch_srv:clock_in(Messenger).

-spec location(Messenger :: messenger()) -> {ok,
                                             {in_transit, {map:vertex(),
                                                           map:vertex()}} |
                                             {stationary, map:vertex()}} |
                                            {error, unknown}.
location(Messenger) ->
    dispatch_srv:location(Messenger).

-spec delivered(Messenger :: messenger(),
                OrderID :: orders:order_id()) -> ok.
delivered(Messenger, OrderID) ->
    dispatch_srv:delivered(Messenger, OrderID).

-spec headquarters() -> map:vertex().
headquarters() ->
    map:headquarters().

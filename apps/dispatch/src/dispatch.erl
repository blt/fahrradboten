-module(dispatch).

-export([location/1]).

-type messenger() :: binary().

-export_type([messenger/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec location(Messenger :: messenger()) ->
                      {ok,
                       {in_transit, {map:vertex(),
                                     map:vertex()}}
                       | {stationary, map:vertex()}} | {error, unknown}.
location(_Messenger) ->
    {error, unknown}.

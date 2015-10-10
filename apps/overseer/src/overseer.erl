-module(overseer).

-export([start_workers/1, sustain_workers/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_workers(N :: non_neg_integer()) -> ok.
start_workers(N) -> overseer_srv:start_workers(N).

-spec sustain_workers(N :: non_neg_integer()) -> ok.
sustain_workers(N) -> overseer_srv:sustain_workers(N).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

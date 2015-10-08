-module(dispatch_messenger).

-behaviour(gen_fsm).

%% API
-export([start/1]).

%% gen_fsm callbacks
-export([init/1, location/1,
         idle/3, work/2, traveling_to_pickup/2, delivering_package/2,
         handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {
          order_id :: orders:order_id() | undefined,
          id = <<>> :: dispatch:messenger(),
          current_location = dispatch:headquarters() :: in_transit | map:vertex(),
          route = [] :: map:path(),
          package_path = [] :: map:path()
         }).

%%%===================================================================
%%% API
%%%===================================================================

start(MessengerID) ->
    gen_fsm:start(?MODULE, [MessengerID], []).

location(Pid) ->
    gen_fsm:sync_send_event(Pid, location, timer:seconds(5)).

work(Pid, OrderID) ->
    gen_fsm:sync_send_event(Pid, {work, OrderID}, timer:seconds(5)).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([MessengerID]) ->
    {ok, idle, #state{id = MessengerID}}.

idle({work, OrderID}, From, State) ->
    {ok, {PickupLocation, DropOffLocation, _Worth}} = orders:details(OrderID),
    {ok, {PackagePath, _TotalDistance}} = map:path(PickupLocation, DropOffLocation),
    CurrentLocation = State#state.current_location,
    {ok, {Route, _RouteDistance}} = map:path(CurrentLocation, PickupLocation),
    {TravelTime, NewState} = case Route of
                                 [CurrentLocation, NextLocation | _Rest] ->
                                     TT = travel_time_between(CurrentLocation, NextLocation),
                                     {TT, State#state{
                                            order_id         = OrderID,
                                            current_location = in_transit,
                                            package_path     = PackagePath,
                                            route            = lists:nthtail(1, Route)
                                           }};
                                 [CurrentLocation] ->
                                     {0, State#state{
                                           order_id         = OrderID,
                                           current_location = in_transit,
                                           package_path     = PackagePath,
                                           route            = [CurrentLocation]
                                          }}
                             end,
    gen_fsm:reply(From, ok),
    {next_state, traveling_to_pickup, NewState, TravelTime}.

traveling_to_pickup(timeout, State) ->
    in_transit = State#state.current_location,
    case State#state.route of
        [_] ->
            [CurrentLocation, NextLocation | Rest] = State#state.package_path,
            TravelTime = travel_time_between(CurrentLocation, NextLocation),
            NewState = State#state{
                         route = [NextLocation | Rest]
                        },
            {next_state, delivering_package, NewState, TravelTime};
        [CurrentLocation, NextLocation | Rest] ->
            TravelTime = travel_time_between(CurrentLocation, NextLocation),
            NewState = State#state{
                         route = [NextLocation | Rest]
                        },
            {next_state, traveling_to_pickup, NewState, TravelTime}
    end.

delivering_package(timeout, State) ->
    in_transit = State#state.current_location,
    case State#state.route of
        [CurrentLocation] ->
            NewState = State#state{
                         current_location = CurrentLocation,
                         route = [],
                         package_path = [],
                         order_id = undefined
                        },
            dispatch:delivered(State#state.id, State#state.order_id),
            {next_state, idle, NewState};
        [CurrentLocation, NextLocation | Rest] ->
            TravelTime = travel_time_between(CurrentLocation, NextLocation),
            NewState = State#state{
                         route = [NextLocation | Rest]
                        },
            {next_state, delivering_package, NewState, TravelTime}
    end.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec travel_time_between(A :: map:vertex(),
                          B :: map:vertex()) -> non_neg_integer().
travel_time_between(A, A) -> 0;
travel_time_between(A, B) ->
    {ok, Distance} = map:distance(A, B),
    erlang:convert_time_unit(Distance, seconds, milli_seconds).

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

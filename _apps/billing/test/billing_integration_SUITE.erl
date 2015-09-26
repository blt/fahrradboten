-module(billing_integration_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([
         all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2,
         %% group: boot
         test_boot_billing/1,
         %% group: logic
         test_open/1, test_credit/1, test_debit/1
        ]).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() -> [
          {group, boot},
          {group, logic}
         ].

groups() -> [
             {boot, [],
              [
               test_boot_billing
              ]
             },
             {logic, [],
              [
               test_open,
               test_credit,
               test_debit
              ]
             }
            ].


init_per_suite(Config) ->
    stop_system(),
    start_system(),
    timer:sleep(1000),
    Config.

end_per_suite(_Config) ->
    stop_system(),
    ok.

init_per_testcase(_Suite, Config) ->
    Config.

end_per_testcase(_Suite, _Config) ->
    ok.

%%%===================================================================
%%% TESTS
%%%===================================================================

%%--------------------------------------------------------------------
%% Group : boot
%%--------------------------------------------------------------------

test_boot_billing(_Confif) ->
    ok.

%%--------------------------------------------------------------------
%% Group : logic
%%--------------------------------------------------------------------

test_open(_Config) ->
    Account = <<"test_open_account">>,

    %% new account, blank balance
    ?assertMatch(ok, billing:open(Account)),
    ?assertMatch({ok, 0}, billing:balance(Account)).

test_credit(_Config) ->
    Account = <<"test_credit_account">>,

    %% new account, blank balance
    ?assertMatch(ok, billing:open(Account)),
    ?assertMatch({ok, 0}, billing:balance(Account)),

    %% credit account, update balance
    ?assertMatch({ok, 10}, billing:credit(Account, 10)),
    ?assertMatch({ok, 10}, billing:balance(Account)),

    %% account that does not exist
    ?assertMatch({error, unknown_account}, billing:credit(<<"dne">>, 100)).

test_debit(_Config) ->
    Account = <<"test_debit_account">>,

    %% new account, blank balance
    ?assertMatch(ok, billing:open(Account)),
    ?assertMatch({ok, 0}, billing:balance(Account)),

    %% debit account, error
    ?assertMatch({error, insufficient_balance}, billing:debit(Account, 10)),

    %% credit account, update balance
    ?assertMatch({ok, 10}, billing:credit(Account, 10)),
    ?assertMatch({ok, 10}, billing:balance(Account)),

    %% debit account, success
    ?assertMatch({ok, 0}, billing:debit(Account, 10)),

    %% account that does not exist
    ?assertMatch({error, unknown_account}, billing:credit(<<"dne">>, 100)).


%%%===================================================================
%%% Internal Functions
%%%===================================================================

start_system() ->
    ok = application:start(billing).

stop_system() ->
    _ = application:stop(billing).

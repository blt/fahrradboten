-module(billing).

-export([open/1, credit/2, debit/2, balance/1]).

-type account_name() :: binary().
-type worth() :: non_neg_integer().

-export_type([account_name/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% Customers 'charge' their account, use that to pay for messangers. We skim 1%
%% off each transaction.
%%
%% Account 'fahrradboten' is company account.
%%
%% All money values are in USD cents.

-spec open(Account :: account_name()) -> ok.
open(Account) ->
    billing_srv:open(Account).

-spec credit(Account :: account_name(),
             Amount :: worth()) -> {ok, worth()} |
                                   {error, unknown_account}.
credit(Account, Amount) ->
    billing_srv:credit(Account, Amount).

-spec debit(Account :: account_name(),
            Amount :: worth()) -> {ok, worth()} |
                                  {error, insufficient_balance} |
                                  {error, unknown_account}.
debit(Account, Amount) ->
    billing_srv:debit(Account, Amount).

-spec balance(Account :: account_name()) -> {ok, worth()} |
                                            {error, unknown_account}.
balance(Account) ->
    billing_srv:balance(Account).

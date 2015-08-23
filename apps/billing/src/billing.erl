-module(billing).

-export([credit/2, debit/2, balance/1]).

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

-spec credit(Account :: account_name(),
             Amount :: worth()) -> ok |
                                   {error, unknown_account}.
credit(_Account, _Amount) ->
    ok.

-spec debit(Account :: account_name(),
            Amount :: worth()) -> ok |
                                  {error, insufficient_balance} |
                                  {error, unknown_account}.
debit(_Account, _Amount) ->
    ok.

-spec balance(Account :: account_name()) -> {ok, worth()} |
                                            {error, unknown_account}.
balance(_Account) ->
    {error, unknown_account}.

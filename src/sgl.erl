%%% @doc
%%% An implementation of the simple grant language: https://evernym.github.io/sgl/.
%%%
%%% Given a rule and a list of prinicipals it will determine if the prinicpals
%%% meet the requirements of the rule.
%%% @end
-module(sgl).

-export([evaluate/2]).

-type id_condition() :: {id, Id :: binary()}.

-type role_condition() ::
    {role, Role :: binary()} | {role, N :: pos_integer(), Role :: binary()}.

-type all_condition() :: {
    allcond,
    [id_condition() | role_condition() | any_condition() | all_condition()]
}.

-type any_condition() ::
    {
        anycond,
        [id_condition() | role_condition() | any_condition() | all_condition()]
    }
    | {
        anycond,
        N :: pos_integer(),
        [id_condition() | role_condition() | any_condition() | all_condition()]
    }.

-type rule() :: {
    {grant, [Permission :: binary()]},
    {condition, id_condition() | role_condition() | any_condition() | all_condition()}
}.

-type principal() :: {principal, {id, Id :: binary()}, {roles, [binary()]}}.

%% @doc Check the a given set of principals meet the requirements of the given grant rule.
-spec evaluate(Rule :: rule(), Principals :: [principal()]) ->
    {ok, PrinicipalIds :: [Id :: binary()]} | {error, PrinicipalIds :: [Id :: binary()]}.
evaluate({{grant, _}, {condition, Conditions}}, Principals) ->
    {Result, Used, _} = check(Conditions, {[], Principals, []}),
    %% Filter down to just the principal IDs
    Ids = lists:foldl(
        fun({principal, {id, Id}, _}, Acc) ->
            [Id | Acc]
        end,
        [],
        Used
    ),
    {Result, Ids}.

%%%
%%% Private stuff below
%%%

%% Check an Id condition
check({id, Value}, Input) ->
    do_check_id(Value, Input);
%% Check role match with default of 1 required
check({role, Value}, Input) ->
    check({role, 1, Value}, Input);
%% Check role match with N required
check({role, N, Value}, Input) ->
    do_check_roles(Value, N, Input);
%% Check 'any' match with default of 1
check({anycond, Rules}, Input) ->
    check({anycond, 1, Rules}, Input);
%% Check 'any' match with N required
check({anycond, N, Rules}, Input) ->
    do_check_any(Rules, N, Input);
%% Check 'all' rule
check({allcond, Rules}, Input) ->
    do_check_all(Rules, Input).

%% Id check logic
do_check_id(_, {Used, [], Unused}) ->
    %% Exhausted the list. No matches
    %%Principals = lists:flatten([Used, Unused]),
    Principals = Used ++ Unused,
    {error, [], Principals};
%% Ids match
do_check_id(Id, {Used, [{principal, {id, Id}, _} = H | Tail], Unused}) ->
    {ok, [H | Used], Tail ++ Unused};
do_check_id(Id, {Used, [H | T], Unused}) ->
    % No match so far. Add the current principal (H) to unused and continue checking
    do_check_id(Id, {Used, T, [H | Unused]}).

%% Role check logic
%% We hit the 'count' of required roles.
do_check_roles(_, 0, {Used, Rest, Unused}) ->
    {ok, Used, Rest ++ Unused};
%% No matches
do_check_roles(_, _, {Used, [], Unused}) ->
    Principals = Used ++ Unused,
    {error, [], Principals};
do_check_roles(
    Role,
    RequiredCount,
    {Used, [{principal, _, {roles, Roles}} = H | Tail], Unused}
) ->
    case lists:any(fun(E) -> E =:= Role end, Roles) of
        true ->
            %% Found a role match. Add the ID to Used and decrement the count
            do_check_roles(Role, RequiredCount - 1, {[H | Used], Tail, Unused});
        _ ->
            %% Not found (so far), keep going
            do_check_roles(Role, RequiredCount, {Used, Tail, [H | Unused]})
    end.

%% All check logic
do_check_all([], {[], _, Unused}) ->
    %% Hit the end of rules and found no matches
    {error, [], Unused};
do_check_all([], {Used, _, Unused}) ->
    %% Hit the end of rules, no more to check, and didn't fail along the way. We're good!
    {ok, Used, Unused};
do_check_all([Rule | Rest], {Used, Remaining, Unused}) ->
    case check(Rule, {Used, Remaining, Unused}) of
        {ok, Used1, UnusedPrincipals} ->
            %% Found a match with one of the conditions
            do_check_all(Rest, {Used1, UnusedPrincipals, Unused});
        {error, U, P} ->
            %% Return on the first fail
            {error, U, P}
    end.

%% Any check logic
do_check_any([], N, {Used, _, Unused}) when N > 0 ->
    %% We exhausted all the rules and didn't meet the requirements
    {error, Used, Unused};
do_check_any(_, 0, {Used, _, Unused}) ->
    %% We hit the required amount
    {ok, Used, Unused};
do_check_any([Rule | Rest], N, {Used, Remaining, Unused}) ->
    case check(Rule, {Used, Remaining, Unused}) of
        {ok, Used1, UnusedPrincipals} ->
            %% Found a match with one of the conditions. Decrement and continue
            do_check_any(Rest, N - 1, {Used1, UnusedPrincipals, Unused});
        {error, Used1, UnusedPrincipals} ->
            %% We hit and error, but continue as we're looking for 'any' matches
            do_check_any(Rest, N, {Used1, UnusedPrincipals, Unused})
    end.

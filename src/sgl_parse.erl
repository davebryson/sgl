%%% @doc
%%% Functions to convert the term based rules and prinicpals into the
%%% internal term format used by the sgl 'engine'.
%%% @end
-module(sgl_parse).

-export([
    transform_rule/1,
    transform_principals/1
]).

transform_rule({{grant, Permissions}, Conditions}) ->
    {
        {
            grant,
            into_binaries(Permissions, [])
        },
        {condition, parse_condition(Conditions)}
    }.

transform_principals(PrincipalList) ->
    [
        {principal, {id, list_to_binary(Id)}, {roles, into_binaries(Roles, [])}}
     || {Id, Roles} <- PrincipalList
    ].

parse_condition({id, Value}) ->
    {id, list_to_binary(Value)};
parse_condition({role, Value}) ->
    parse_condition({role, 1, Value});
parse_condition({role, N, Value}) ->
    {role, N, list_to_binary(Value)};
parse_condition({whenany, List}) ->
    parse_condition({whenany, 1, List});
parse_condition({whenany, N, List}) ->
    {anycond, N, parse_sublist(List)};
parse_condition({whenall, List}) ->
    {allcond, parse_sublist(List)}.

parse_sublist(List) ->
    lists:reverse([parse_condition(E) || E <- List]).

%% convert a list of lists or binaries into a list of binaries.
into_binaries([], Acc) ->
    lists:reverse(Acc);
into_binaries([H | T], Acc) when is_list(H) ->
    into_binaries(T, [list_to_binary(H) | Acc]);
into_binaries([H | T], Acc) when is_binary(H) ->
    into_binaries(T, [H | Acc]).

# Simple Grant Language (SGL)

This is an Erlang implementation of the [Simple Grant Language](https://evernym.github.io/sgl/)

> SGL is a simple but flexible DSL for granting and testing privileges (authorization). It is like XACML but simpler. You can use it to write rules about who should be able to do what, and then to compare circumstances to the rules to enforce custom logic. This lets you create your own Role-Based Access Control mechanisms, as well as authorizations based on other criteria.

This version use the Erlang terms for the language definition, but it could be easily adapted to other formats such as JSON.


## Example

Here's a simple rule that says `grant` the permission `backstage` to anyone with the role `ticketholder` OR `press`.
```erlang 
Rule = {
    {grant, ["backstage"]},
    {whenany, [
        {role, "ticketholder"},
        {role, "press"}
    ]).
}
```
You can then apply the rule to a set of principals to test if they can be granted the permission:
```erlang
%% Here are the Principals
Principals = [
    {"alice", ["stagehand"]},
    {"carl", ["press"]},
    {"jim", ["ticketholder"]}
],

%% 'Ids' is a list of the Prinicipal Ids that may fulfill the rule. 
%% In this example, that would be either carl OR jim.
{ok, Ids} = sgl:evaluate(Rule, Principals).
```

## Build
    $ rebar3 compile

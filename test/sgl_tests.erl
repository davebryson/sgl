-module(sgl_tests).

-include_lib("eunit/include/eunit.hrl").

convert_from_script_format(Grant, Principals) ->
    R = sgl_parse:transform_rule(Grant),
    P = sgl_parse:transform_principals(Principals),
    {R, P}.

will_pass(Grant, Principals, Size) ->
    {Rule, Plist} = convert_from_script_format(Grant, Principals),
    {ok, P} = sgl:evaluate(Rule, Plist),
    Size = length(P).

will_fail(Grant, Principals) ->
    {Rule, Plist} = convert_from_script_format(Grant, Principals),
    {error, _} = sgl:evaluate(Rule, Plist).

check_id_test() ->
    will_pass(
        {
            {grant, ["drive", "maintain"]},
            {id, "dave"}
        },
        [
            {"dave", ["a"]},
            {"bob", ["maintenance", "b"]}
        ],
        1
    ),

    will_fail(
        {
            {grant, ["drive", "maintain"]},
            {id, "carl"}
        },
        [
            {"dave", ["a"]},
            {"bob", ["maintenance", "b"]}
        ]
    ),

    will_pass(
        {
            {grant, ["drive", "maintain"]},
            {whenany, [
                {id, "alice"},
                {id, "bob"},
                {id, "carol"},
                {id, "jim"}
            ]}
        },
        [
            {"dave", ["a"]},
            {"bob", ["maintenance", "b"]}
        ],
        1
    ),

    will_fail(
        {
            {grant, ["drive", "maintain"]},
            {whenany, [
                {id, "alice"},
                {id, "bob"},
                {id, "carol"},
                {id, "jim"}
            ]}
        },
        []
    ),

    will_pass(
        {
            {grant, ["fullservice"]},
            {whenany, [
                {id, "bill"},
                {whenall, [
                    {id, "carl"},
                    {id, "jim"}
                ]}
            ]}
        },
        [
            {"dave", ["a"]},
            {"carl", ["maintenance", "b"]},
            {"jim", ["maintenance", "b"]}
        ],
        2
    ),

    will_pass(
        {
            {grant, ["fullservice"]},
            {whenany, [
                {id, "bill"},
                {whenall, [
                    {id, "carl"},
                    {id, "jim"}
                ]}
            ]}
        },
        [
            {"tim", ["a"]},
            {"bill", ["maintenance", "b"]},
            {"jim", ["maintenance", "b"]}
        ],
        1
    ),

    will_fail(
        {
            {grant, ["fullservice"]},
            {whenany, [
                {id, "bill"},
                {whenall, [
                    {id, "carl"},
                    {id, "jim"}
                ]}
            ]}
        },
        [
            {"tim", ["a"]},
            {"john", ["maintenance", "b"]},
            {"jim", ["maintenance", "b"]}
        ]
    ),
    ok.

check_role_test() ->
    %% Need 1 ticket holder. Return the first match
    will_pass(
        {
            {grant, ["backstage"]},
            {role, "ticket-holder"}
        },
        [
            {"dave", ["a"]},
            {"carl", ["press", "ticket-holder"]},
            {"jim", ["ticket-holder"]}
        ],
        1
    ),

    %% Need 2 ticket holders
    will_pass(
        {
            {grant, ["backstage"]},
            {role, 2, "ticket-holder"}
        },
        [
            {"dave", ["a"]},
            {"carl", ["press", "ticket-holder"]},
            {"jim", ["ticket-holder"]}
        ],
        2
    ),

    %% Not 2 ticket holders
    will_fail(
        {
            {grant, ["backstage"]},
            {role, 2, "ticket-holder"}
        },
        [
            {"dave", ["a"]},
            {"carl", ["press"]},
            {"jim", ["ticket-holder"]}
        ]
    ),
    ok.

check_any_test() ->
    will_pass(
        {
            {grant, ["fullservice"]},
            {whenany, [
                {id, "carl"},
                {whenall, [
                    {role, 2, "maintenance"}
                ]}
            ]}
        },
        [
            {"tim", ["a"]},
            {"bill", ["maintenance", "b"]},
            {"jim", ["maintenance", "b"]}
        ],
        2
    ),

    will_pass(
        {
            {grant, ["fullservice"]},
            {whenany, [
                {id, "carl"},
                {whenall, [
                    {role, 2, "maintenance"}
                ]}
            ]}
        },
        [
            {"tim", ["a"]},
            {"bill", ["maintenance", "b"]},
            {"carl", ["b"]}
        ],
        1
    ),

    will_pass(
        {
            {grant, ["guardian"]},
            {whenany, [
                {role, 2, "grandparent"},
                {role, "parent"},
                {id, "bob"}
            ]}
        },
        [
            {"tim", ["a"]},
            {"bill", ["grandparent"]},
            {"jim", ["cousin"]},
            {"carol", ["grandparent"]}
        ],
        2
    ),

    will_pass(
        {
            {grant, ["guardian"]},
            {whenany, [
                {role, 2, "grandparent"},
                {role, "parent"},
                {id, "bob"}
            ]}
        },
        [
            {"tim", ["a"]},
            {"bill", ["uncle"]},
            {"jim", ["parent"]},
            {"carol", ["grandparent"]}
        ],
        1
    ),

    will_pass(
        {
            {grant, ["guardian"]},
            {whenany, [
                {role, 2, "grandparent"},
                {role, "parent"},
                {id, "bob"}
            ]}
        },
        [
            {"tim", ["a"]},
            {"bob", ["uncle"]},
            {"jim", ["friend"]},
            {"carol", ["grandparent"]}
        ],
        1
    ),

    will_fail(
        {
            {grant, ["guardian"]},
            {whenany, [
                {role, 2, "grandparent"},
                {role, "parent"},
                {id, "bob"}
            ]}
        },
        [
            {"tim", ["a"]},
            {"dave", ["uncle"]},
            {"jim", ["friend"]},
            {"carol", ["grandparent"]}
        ]
    ),
    ok.

check_all_test() ->
    will_fail(
        {
            {grant, ["vote"]},
            {whenall, [
                {role, 2, "investor"},
                {role, 1, "board"}
            ]}
        },
        [
            {"tim", ["investor", "board"]},
            {"bob", ["investor", "board"]}
        ]
    ),

    will_pass(
        {
            {grant, ["vote"]},
            {whenall, [
                {role, 2, "investor"},
                {role, 1, "board"}
            ]}
        },
        [
            {"tim", ["investor", "board"]},
            {"bob", ["investor", "board"]},
            {"alice", ["investor"]}
        ],
        3
    ),

    will_pass(
        {
            {grant, ["vote"]},
            {whenall, [
                {role, 2, "investor"},
                {role, 1, "board"}
            ]}
        },
        [
            {"alice", ["board"]},
            {"tim", ["investor", "board"]},
            {"bob", ["investor", "board"]}
        ],
        3
    ),
    ok.

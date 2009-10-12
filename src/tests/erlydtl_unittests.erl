-module(erlydtl_unittests).

-export([run_tests/0]).

tests() ->
    [
        {"vars", [
                {"string",
                    <<"String value is: {{ var1 }}">>,
                    [{"var1", "foo"}], <<"String value is: foo">>},
                {"int",
                    <<"The magic number is: {{ var1 }}">>,
                    [{"var1", 42}], <<"The magic number is: 42">>},
                {"float",
                    <<"The price of milk is: {{ var1 }}">>,
                    [{"var1", 0.42}], <<"The price of milk is: 0.42">>},
                {"No spaces",
                    <<"{{var1}}">>,
                    [{"var1", "foo"}], <<"foo">>}
            ]},
        {"comment", [
                {"comment block is excised",
                    <<"bob {% comment %}(moron){% endcomment %} loblaw">>,
                    [], <<"bob  loblaw">>},
                {"inline comment is excised",
                    <<"you're {# not #} a very nice person">>,
                    [], <<"you're  a very nice person">>}
            ]},
        {"string literal", [
                {"Render literal",
                    <<"{{ \"foo\" }} is my name">>, [], <<"foo is my name">>},
                {"Newlines are escaped",
                    <<"{{ \"foo\\n\" }}">>, [], <<"foo\n">>}
            ]},
        {"cycle", [
                {"Cycling through quoted strings",
                    <<"{% for i in test %}{% cycle \"a\",\"b\" %}{{ i }},{% endfor %}">>,
                    [{"test", ["0", "1", "2", "3", "4"]}], <<"a0,b1,a2,b3,a4,">>},
                {"Cycling through normal variables",
                     <<"{% for i in test %}{% cycle aye, bee %}{{ i }},{% endfor %}">>,
                     [{"test", ["0", "1", "2", "3", "4"]}, {"aye", "a"}, {"bee", "b"}],
                     <<"a0,b1,a2,b3,a4,">>}
            ]},
        {"number literal", [
                {"Render integer",
                    <<"{{ 5 }}">>, [], <<"5">>},
                {"Render float",
                    <<"{{ 4.2 }}">>, [], <<"4.2">>}
            ]},
        {"variable", [
                {"Render variable",
                    <<"{{ var1 }} is my game">>, [{"var1", "bar"}], <<"bar is my game">>},
                {"Render variable with attribute",
                    <<"I enjoy {{ var1.game }}">>, [{"var1", [{"game", "Othello"}]}], <<"I enjoy Othello">>},
                {"Render variable in dict",
                    <<"{{ var1 }}">>, dict:store("var1", "bar", dict:new()), <<"bar">>},
                {"Render variable in gb_tree",
                    <<"{{ var1 }}">>, gb_trees:insert("var1", "bar", gb_trees:empty()), <<"bar">>},
                {"Render variable with attribute in dict",
                    <<"{{ var1.attr }}">>, [{"var1", dict:store("attr", "Othello", dict:new())}], <<"Othello">>},
                {"Render variable with attribute in gb_tree",
                    <<"{{ var1.attr }}">>, [{"var1", gb_trees:insert("attr", "Othello", gb_trees:empty())}], <<"Othello">>},
                {"Render variable in parameterized module",
                    <<"{{ var1.some_var }}">>, [{"var1", erlydtl_example_variable_storage:new("foo")}], <<"foo">>},
                {"Nested attributes",
                    <<"{{ person.city.state.country }}">>, [{"person", [{"city", [{"state", [{"country", "Italy"}]}]}]}],
                    <<"Italy">>}
            ]},
        {"if", [
                {"If false",
                    <<"{% if var1 %}boo{% endif %}">>, [{"var1", false}], <<>>},
                {"If true",
                    <<"{% if var1 %}boo{% endif %}">>, [{"var1", true}], <<"boo">>},
                {"If equals",
                    <<"{% if var1 == 42 %}boo{% endif %}">>, [{"var1", 42}], <<"boo">>},
                {"If undefined",
                    <<"{% if var1 %}boo{% endif %}">>, [], <<>>},
                {"If other atom",
                    <<"{% if var1 %}yay{% endif %}">>, [{"var1", foobar}], <<"yay">>},
                {"If string",
                    <<"{% if var1 %}yay{% endif %}">>, [{"var1", "hello"}], <<"yay">>},
                {"If proplist",
                    <<"{% if var1 %}yay{% endif %}">>, [{"var1", [{"foo", "bar"}]}], <<"yay">>}
            ]},
        {"for", [
                {"Simple loop",
                    <<"{% for x in list %}{{ x }}{% endfor %}">>, [{"list", ["1", "2", "3"]}],
                    <<"123">>},
                {"Expand list",
                    <<"{% for x, y in list %}{{ x }},{{ y }}\n{% endfor %}">>, [{"list", [["X", "1"], ["X", "2"]]}],
                    <<"X,1\nX,2\n">>},
                {"Expand tuple",
                    <<"{% for x, y in list %}{{ x }},{{ y }}\n{% endfor %}">>, [{"list", [{"X", "1"}, {"X", "2"}]}],
                    <<"X,1\nX,2\n">>},
                {"Resolve variable attribute",
                    <<"{% for number in person.numbers %}{{ number }}\n{% endfor %}">>, [{"person", [{"numbers", ["411", "911"]}]}],
                    <<"411\n911\n">>},
                {"Resolve nested variable attribute",
                    <<"{% for number in person.home.numbers %}{{ number }}\n{% endfor %}">>, [{"person", [{"home", [{"numbers", ["411", "911"]}]}]}],
                    <<"411\n911\n">>},
                {"Counter0",
                    <<"{% for number in numbers %}{{ forloop.counter0 }}. {{ number }}\n{% endfor %}">>, 
                    [{"numbers", ["Zero", "One", "Two"]}], <<"0. Zero\n1. One\n2. Two\n">>},
                {"Counter",
                    <<"{% for number in numbers %}{{ forloop.counter }}. {{ number }}\n{% endfor %}">>, 
                    [{"numbers", ["One", "Two", "Three"]}], <<"1. One\n2. Two\n3. Three\n">>},
                {"Reverse Counter0",
                    <<"{% for number in numbers %}{{ forloop.revcounter0 }}. {{ number }}\n{% endfor %}">>, 
                    [{"numbers", ["Two", "One", "Zero"]}], <<"2. Two\n1. One\n0. Zero\n">>},
                {"Reverse Counter",
                    <<"{% for number in numbers %}{{ forloop.revcounter }}. {{ number }}\n{% endfor %}">>, 
                    [{"numbers", ["Three", "Two", "One"]}], <<"3. Three\n2. Two\n1. One\n">>},
                {"Counter \"first\"",
                    <<"{% for number in numbers %}{% if forloop.first %}{{ number }}{% endif %}{% endfor %}">>,
                    [{"numbers", ["One", "Two", "Three"]}], <<"One">>},
                {"Counter \"last\"",
                    <<"{% for number in numbers %}{% if forloop.last %}{{ number }}{% endif %}{% endfor %}">>,
                    [{"numbers", ["One", "Two", "Three"]}], <<"Three">>},
                {"Nested for loop",
                    <<"{% for outer in list %}{% for inner in outer %}{{ inner }}\n{% endfor %}{% endfor %}">>,
                    [{"list", [["Al", "Albert"], ["Jo", "Joseph"]]}],
                    <<"Al\nAlbert\nJo\nJoseph\n">>},
                {"Access parent loop counters",
                    <<"{% for outer in list %}{% for inner in outer %}({{ forloop.parentloop.counter0 }}, {{ forloop.counter0 }})\n{% endfor %}{% endfor %}">>,
                    [{"list", [["One", "two"], ["One", "two"]]}], <<"(0, 0)\n(0, 1)\n(1, 0)\n(1, 1)\n">>}
            ]}
    ].

run_tests() ->
    io:format("Running unit tests...~n"),
    Failures = lists:foldl(fun run_test_group/2, [], tests()),

    case Failures of
        [] -> io:format("Unit tests passed~n~n");
        _ -> io:format("Unit test failures: ~p~n~n", [Failures]),
             halt(1)
    end.

run_test_group({Group, false, _Assertions}, GroupAcc) ->
    io:format(" Test group ~p skipped~n", [Group]),
    GroupAcc;

run_test_group({Group, Assertions}, GroupAcc) ->
    io:format(" Test group ~p...~n", [Group]),
    {Group, Acc} = lists:foldl(fun run_test/2, {Group, GroupAcc}, Assertions),
    Acc.

run_test({Name, DTL, Vars, Output}, {Group, Acc}) ->
    NewAcc = 
        case catch erlydtl:compile(DTL, erlydtl_running_test, [{write_erl_to, "/tmp/t"}]) of
            {ok, _} ->
                check_rendered(Group, Name, Vars, Output, Acc);
            Err ->
               [{Group, Name, Err} | Acc]
        end,
    {Group, NewAcc}.

check_rendered(Group, Name, Vars, Output, Acc) ->
    case catch erlydtl_running_test:render(Vars) of
        {ok, IOList} ->
                case catch erlydtl_running_test:render(vars_to_binary(Vars)) of
                    {ok, IOListBin} ->
                        case {iolist_to_binary(IOList), iolist_to_binary(IOListBin)} of
                            {Output, Output} ->
                                Acc;
                            {Output, Unexpected} ->
                                [{Group, Name, 'binary', Unexpected, Output} | Acc];
                            {Unexpected, Output} ->
                                [{Group, Name, 'list', Unexpected, Output} | Acc];
                            {Unexpected1, Unexpected2} ->
                                [{Group, Name, 'list', Unexpected1, Output}, 
                                 {Group, Name, 'binary', Unexpected2, Output} | Acc]
                        end;
                    Err -> [{Group, Name, Err} | Acc]
                end;
        Err -> [{Group, Name, Err} | Acc]
    end.

vars_to_binary(Vars) when is_list(Vars) ->
    lists:map(fun
            ({Key, [H|_] = Value}) when is_tuple(H) ->
                {Key, vars_to_binary(Value)};
            ({Key, [H|_] = Value}) when is_integer(H) ->
                {Key, list_to_binary(Value)};
            ({Key, Value}) ->
                {Key, Value}
        end, Vars);
vars_to_binary(Vars) ->
    Vars.

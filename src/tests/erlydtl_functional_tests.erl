%%%-------------------------------------------------------------------
%%% File:      erlydtl_tests.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc       ErlyDTL test suite
%%% @end
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%% @since 2008-02-11 by Roberto Saccon
%%%-------------------------------------------------------------------
-module(erlydtl_functional_tests).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').


%% API
-export([run_tests/0, run_test/1]).

test_list() ->
                                                % order is important.
    ["autoescape",
     "comment",
     "extends",
     "filters",
     "for",
     "for_list",
     "for_tuple",
     "for_records",
     "include",
     "if",
     "if_preset",
     "ifequal",
     "ifnotequal",
     "now",
     "var",
     "var_error",
     "cycle",
     %"custom_tag",
     %"custom_tag_error", 
     %"custom_call", 
     "include_template",
     "include_path",
     "extends_path",
     "extends_path2" ].

%% @spec (Name::string()) -> {CompileStatus::atom(), PresetVars::list(), 
%%     RenderStatus::atom(), RenderVars::list()} | skip
%% @doc
%% @end 
%%--------------------------------------------------------------------
setup("autoescape") ->
    RenderVars = [{var1, "<b>bold</b>"}],
    {ok, RenderVars};  
setup("extends") ->
    RenderVars = [{base_var, "base-barstring"}, {test_var, "test-barstring"}],
    {ok, RenderVars};
setup("filters") ->
    RenderVars = [
                  {date_var1, {1975,7,24}},
                  {datetime_var1, {{1975,7,24}, {7,13,1}}},
                  {'list', ["eins", "zwei", "drei"]}
                 ],
    {ok, RenderVars};
setup("for") ->
    RenderVars = [{fruit_list, ["apple", "banana", "coconut"]}],
    {ok, RenderVars};
setup("for_list") ->
    RenderVars = [{fruit_list, [["apple", "apples", "$1"], ["banana", "bananas", "$2"], ["coconut", "coconuts", "$500"]]}],
    {ok, RenderVars};
setup("for_tuple") ->
    RenderVars = [{fruit_list, [{"apple", "apples"}, {"banana", "bananas"}, {"coconut", "coconuts"}]}],
    {ok, RenderVars};
setup("for_records") ->
    Link1 = [{name, "Amazon"}, {url, "http://amazon.com"}],
    Link2 = [{name, "Google"}, {url, "http://google.com"}],
    Link3 = [{name, "Microsoft"}, {url, "http://microsoft.com"}],
    RenderVars = [{link_list, [Link1, Link2, Link3]}],
    {ok, RenderVars};  
setup("include") ->
    RenderVars = [{var1, "foostring1"}, {var2, "foostring2"}],
    {ok, RenderVars};
setup("if") ->
    RenderVars = [{var1, "something"}],
    {ok, RenderVars}; 
setup("ifequal") ->
    RenderVars = [{var1, "foo"}, {var2, "foo"}, {var3, "bar"}],
    {ok, RenderVars};      
setup("ifnotequal") ->
    RenderVars = [{var1, "foo"}, {var2, "foo"}, {var3, "bar"}],
    {ok, RenderVars};        
setup("var") ->
    RenderVars = [{var1, "foostring1"}, {var2, "foostring2"}, {var_not_used, "foostring3"}],
    {ok, RenderVars};
setup("var_error") ->
    RenderVars = [{var1, "foostring1"}],   
    {error, RenderVars};
setup("cycle") ->
    RenderVars = [{test, [integer_to_list(X) || X <- lists:seq(1, 20)]},
                  {a, "Apple"}, {b, "Banana"}, {c, "Cherry"}],
    {ok, RenderVars};
setup("include_template") ->
    RenderVars = [{base_var, "base-barstring"}, {test_var, "test-barstring"}],
    {ok, RenderVars};
setup("include_path") ->
    RenderVars = [{base_var, "base-barstring"}, {test_var, "test-barstring"}],
    {ok, RenderVars};
setup("extends_path") ->
    RenderVars = [{base_var, "base-barstring"}, {test_var, "test-barstring"}],
    {ok, RenderVars};
setup("extends_path2") ->
    RenderVars = [{base_var, "base-barstring"}, {test_var, "test-barstring"}],
    {ok, RenderVars};



%%--------------------------------------------------------------------       
%% Custom tags
%%--------------------------------------------------------------------
setup("custom_tag_error") ->
    RenderVars = [],
    {skip, RenderVars};        
setup("custom_call") ->
    RenderVars = [{var1, "something"}],
    {ok, RenderVars};    

setup(_) ->
    {ok, []}.


check_test_result("autoescape", Data) ->
    {match, [_, _]}
        = re:run(Data, "This is escaped: &lt;b&gt;bold&lt;/b&gt;", [global]),
    {match, [_]}
        = re:run(Data, "This is not escaped: <b>bold</b>", [global]),
    ok;

check_test_result("comment", Data) ->
    {match, [_]} = re:run(Data, "foo\\s*bar\\s*\\s*baz\\s*", [global, dotall]),
    ok;

check_test_result("extends", Data) ->
    {match, [_]} =
        re:run(Data, "base-barstring\\s*"
               "base template\\s*"
               "replacing the base title\\s*"
               "more of base template\\s*"
               "replacing the base content - variable: test-barstring after variable\\s*"
               "end of base template",
               [global,dotall]),
    ok;

check_test_result("filters", Data) ->
    {match, [_]} =
        re:run(Data, "Add: 2 \\+ 2 = 4\\s*"
               "Capfirst: Capitalized\\s*"

               "Centered:\\s*<pre>\\s"
               "\\s{7}center\\s{7}\\s"
               "</pre>\\s*"

               "Date format:\\s+Thu, 24 Jul 1975 00:00:00 .{5}\\s*" % Time zone not checked
               "DateTime format: Thu, 24 Jul 1975 07:13:01 .{5}\\s*"  % Time zone not checked

               "Escape JS: \\\\\" \\\\'\\s*"
               "First letter: f\\s*"
               "Fix ampersands: &amp;\\s*"

               "Force_escape: &lt;b&gt;&lt;/b&gt;\\s*"
               "Joined: eins, zwei, drei\\s*"
               "Last: t\\s*"
               "Length: 3\\s*"
               "Length is 2\\?: false\\s*"

               "Left adjust:\\s*<pre>\\s"
               "left\\s{16}\\s"
               "</pre>\\s*"

               "Line breaks: Line 1<br />Line 2<br />Line 3\\s*"
               "Lowercase: lowercase\\s*"

               "Right adjust:\\s*<pre>\\s"
               "\\s{15}right\\s"
               "</pre>\\s*"
               
               "Uppercase: UPPERCASE\\s*"
               "URL Encode: Let%27s\\+go%21"
               ,
               [global,dotall]),
    ok;

check_test_result("for", Data) ->
    {match, [_]} =
        re:run(Data, 
               "before\\s*"
               "<ul>\\s*"
               "<li>1\\. apple</li>\\s*"
               "<li>2\\. banana</li>\\s*"
               "<li>3\\. coconut</li>\\s*"
               "</ul>\\s*"
               "after"
               ,
               [global,dotall]),
    ok;

check_test_result("for_list", Data) ->
    {match, [_]} =
        re:run(Data, 
               "More than one apple is called \"apples\". Only \\$1 each!\\s*"
               "More than one banana is called \"bananas\". Only \\$2 each!\\s*"
               "More than one coconut is called \"coconuts\". Only \\$500 each!\\s*"
               ,
               [global,dotall]),
    ok;

check_test_result(_Name, _Data) ->
    ok.

run_tests() ->
    io:format("Running functional tests...~n"),
    case fold_tests() of
        {N, []}->
            Msg = lists:concat(["All ", N, " functional tests passed~n~n"]),
            io:format(Msg),
            {ok, Msg};
        {_, Errs} ->
            io:format("Errors: ~p~n~n",[Errs]),
            halt(1),
            failed
    end.


run_test(Name) ->
    test_compile_render(filename:join([templates_docroot(), Name])).


%%====================================================================
%% Internal functions
%%====================================================================

fold_tests() ->
    lists:foldl(fun(Name, {AccCount, AccErrs}) ->
                        case catch test_compile_render(Name) of
                            ok -> 
                                {AccCount + 1, AccErrs};
                            {error, Reason} -> 
                                {AccCount + 1, [{Name, Reason} | AccErrs]};
                            {'EXIT', Reason} ->
                                {AccCount + 1, [{Name, Reason} | AccErrs]}
                        end
                end, {0, []}, test_list()
               ).

test_compile_render(Name) ->  
    File = filename:join([templates_docroot(), Name]),
    ModuleName = "example_" ++ Name,
    Module = case ModuleName of
                 M when is_atom(M) -> M;
                 S -> list_to_atom(S)
             end,
    Options = [debug, {doc_root, templates_docroot()}, {erl_out_dir, erl_outdir()}],
    {RenderStatus, Vars} = setup(Name),
    io:format(" Template: ~p, ... compiling and rendering ... ", [Name]),
    case catch erlydtl_renderer:render(Name, Module, Vars, Options) of
        {ok, Data} ->
            case check_test_result(Name, iolist_to_binary(Data)) of
                ok -> 
                    io:format("ok~n"),
                    OutFile = filename:join([templates_outdir(), filename:basename(atom_to_list(Module))]),
                    filelib:ensure_dir(OutFile),
                    case file:open(OutFile, [write]) of
                        {ok, IoDev} ->
                            file:write(IoDev, Data),
                            file:close(IoDev),
                            ok;    
                        Err ->
                            Err
                    end;
                _ ->
                    io:format("error~n"),
                    {error, "render should have failed :" ++ File}
            end;
        {error, Err} ->
            case RenderStatus of
                error ->
                    io:format("ok~n"),
                    ok;
                _ ->
                    io:format("~nCompile error: ~p~n",[Err]), 
                    Err
            end;
        {'EXIT', Reason} ->
            io:format("~n"),
            
            io:format("failed invoking render method in ~p: ~p~n", [Module, Reason]),
            
            {error, "failed invoking render method in " ++ atom_to_list(Module)};
        Other ->
            {error, io_lib:format("Other result: ~p~n", [Other])}
    end.

templates_docroot() ->
    filename:join([erlydtl_deps:get_base_dir(), "examples", "docroot"]).

templates_outdir() ->   
    filename:join([erlydtl_deps:get_base_dir(), "examples", "rendered_output"]).

erl_outdir() ->   
    filename:join([erlydtl_deps:get_base_dir(), "examples", "erl_output"]).

%%%-------------------------------------------------------------------
%%% File:      erlydtl_compiler.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc  
%%% ErlyDTL template compiler
%%% @end  
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon, Evan Miller
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
%%% @since 2007-12-16 by Roberto Saccon, Evan Miller
%%%-------------------------------------------------------------------
-module(erlydtl_compiler).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').
-author('alex.borovsky@gmail.com').

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------
-export([compile/2, compile/3]).

-record(dtl_context, {
          local_scopes = [], 
          auto_escape = off, 
          parse_trail = [],
          debug = false,
          reader = {file, read_file},
          module = [],
          renderer_module = erlydtl_renderer,
          renderer_params,
          compiler_options = [verbose, report_errors],
          write_erl_to = none}).

compile(Binary, Module) when is_binary(Binary) ->
    compile(Binary, Module, []);

compile(File, Module) ->
    compile(File, Module, []).

compile(Binary, Module, Options) when is_binary(Binary) ->
    File = "",
    case parse(Binary) of
        {ok, DjangoParseTree} ->
            case compile_to_binary(File, DjangoParseTree, 
                    init_dtl_context(File, Module, Options)) of
                {ok, Module1, _} ->
                    {ok, Module1};
                Err ->
                    Err
            end;
        Err ->
            Err
    end;
    
compile(File, Module, Options) ->  
    Context = init_dtl_context(File, Module, Options),
    case parse(File, Context) of  
        {ok, ParseTree} ->
            case compile_to_binary(File, ParseTree, Context) of
                {ok, Module1, Bin} ->
                    case proplists:get_value(out_dir, Options) of
                        undefined ->
                            ok;
                        OutDir ->
                            BeamFile = filename:join([OutDir, atom_to_list(Module1) ++ ".beam"]),
                            case file:write_file(BeamFile, Bin) of
                                ok ->
                                    ok;
                                {error, Reason} ->
                                    {error, lists:concat(["beam generation failed (", Reason, "): ", BeamFile])}
                            end
                    end;
                Err ->
                    Err
            end;
        Err ->
            Err
    end.
    

%%====================================================================
%% Internal functions
%%====================================================================

compile_to_binary(File, DjangoParseTree, Context) ->
    try body_ast(DjangoParseTree, Context) of
        Ast ->
            %io:format("Ast for ~p: ~p~n", [Context#dtl_context.module, Ast]),
            Forms = forms(Context#dtl_context.module, Ast),
            case Context#dtl_context.write_erl_to of
                none -> none;
                ErlFile -> 
                    file:write_file(ErlFile ++ ".dump", io_lib:format("~p~n", [Forms])),
                    Res = lists:map(fun(Par) -> [erl_prettypr:format(Par), "\n"] end, Forms),
                    file:write_file(ErlFile, Res)
            end,
            case compile:forms(Forms, 
                    Context#dtl_context.compiler_options) of
                {ok, Module1, Bin} -> 
                    case code:load_binary(Module1, atom_to_list(Module1) ++ ".erl", Bin) of
                        {module, _} -> {ok, Module1, Bin};
                        _ -> {error, lists:concat(["code reload failed: ", Module1])}
                    end;
                error ->
                    {error, lists:concat(["compilation failed: ", File])};
                OtherError ->
                    OtherError
            end
    catch 
        throw:Error -> Error
    end.
                
init_dtl_context(File, Module, Options) when is_list(Module) ->
    init_dtl_context(File, list_to_atom(Module), Options);
init_dtl_context(File, Module, Options) ->
    Ctx = #dtl_context{},
    #dtl_context{
        parse_trail = [File], 
        module = Module,
        debug = proplists:get_value(debug, Options, Ctx#dtl_context.debug), 
        reader = proplists:get_value(reader, Options, Ctx#dtl_context.reader),
        compiler_options = proplists:get_value(compiler_options, Options, Ctx#dtl_context.compiler_options),
        renderer_params = proplists:get_value(renderer_params, Options, none), 
        renderer_module = proplists:get_value(renderer_module, Options, erlydtl_renderer), 
        write_erl_to = proplists:get_value(write_erl_to, Options, Ctx#dtl_context.write_erl_to)}.


parse(File, Context) ->  
    {M, F} = Context#dtl_context.reader,
    case catch M:F(File) of
        {ok, Data} ->
            case parse(Data) of
                {error, Msg} when is_list(Msg) ->
                    {error, File ++ ": " ++ Msg};
                Result ->
                    Result
            end;
        _ ->
            {error, "reading " ++ File ++ " failed "}
    end.
        
parse(Data) ->
    case erlydtl_scanner:scan(binary_to_list(Data)) of
        {ok, Tokens} ->
            erlydtl_parser:parse(Tokens);
        Err ->
            Err
    end.        
  
forms(Module, BodyAst) ->
    Render0FunctionAst = erl_syntax:function(erl_syntax:atom(render),
        [erl_syntax:clause([], none, [erl_syntax:application(none, 
                        erl_syntax:atom(render), [erl_syntax:list([])])])]),
    Function2 = erl_syntax:application(none, erl_syntax:atom(render2), 
        [erl_syntax:variable("Variables")]),
    ClauseOk = erl_syntax:clause([erl_syntax:variable("Val")], none,
        [erl_syntax:tuple([erl_syntax:atom(ok), erl_syntax:variable("Val")])]),     
    ClauseCatch = erl_syntax:clause([erl_syntax:variable("Err")], none,
        [erl_syntax:tuple([erl_syntax:atom(error), erl_syntax:variable("Err")])]),            
    Render1FunctionAst = erl_syntax:function(erl_syntax:atom(render),
        [erl_syntax:clause([erl_syntax:variable("Variables")], none, 
            [erl_syntax:try_expr([Function2], [ClauseOk], [ClauseCatch])])]),  

    RenderInternalFunctionAst = erl_syntax:function(
        erl_syntax:atom(render2), 
            [             
             erl_syntax:clause([erl_syntax:variable("Variables")], none, 
                [BodyAst])]),   
    
    ModuleAst  = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),
    
    ExportAst = erl_syntax:attribute(erl_syntax:atom(export),
        [erl_syntax:list([erl_syntax:arity_qualifier(erl_syntax:atom(render), erl_syntax:integer(0)),
                    erl_syntax:arity_qualifier(erl_syntax:atom(render), erl_syntax:integer(1))])]),


    [erl_syntax:revert(X) || X <- [ModuleAst, ExportAst, Render0FunctionAst,
            Render1FunctionAst, RenderInternalFunctionAst]].    

        
% child templates should only consist of blocks at the top level
body_ast([{extends, File} | ThisParseTree], Context) ->
    AddBlocksAsts = lists:foldl(
                      fun
                          ({block, {identifier, _, BlockName}, Contents}, Ast) ->
                              erl_syntax:application(erl_syntax:atom(Context#dtl_context.renderer_module),
                                           erl_syntax:atom(set_block),
                                                      [erl_syntax:string(BlockName), 
                                           body_ast(Contents, Context), Ast]);
                          (_, Ast) ->
                              Ast
                      end, erl_syntax:variable("Variables"), ThisParseTree),
    
    FileAst = resolve_expression(File, Context),
    include_ast(FileAst, AddBlocksAsts, Context);
 
    
body_ast(ParseTree, Context) ->
    AstInfoList = lists:map(
        fun(Ast) -> process_body_ast(Ast, Context) end, ParseTree),   
    erl_syntax:list(AstInfoList).

process_body_ast({'block', {identifier, _, Name}, Contents}, Context) ->
    block_ast(Name, Contents, Context);

process_body_ast({'comment', _Contents}, _Context) ->
    empty_ast();

process_body_ast({'text', _Pos, String}, Context) -> 
    string_ast(String, Context);

process_body_ast({'expression', _, _} = Expression, Context) ->
    Ast = resolve_expression(Expression, Context),
    format(Ast, Context);              

process_body_ast({'include', Path}, Context) ->
    PathAst = resolve_expression(Path, Context),
    include_ast(PathAst, Context);

process_body_ast({'if', Expression, Contents}, Context) ->
    IfAstInfo = body_ast(Contents, Context),
    ElseAstInfo = empty_ast(),
    ifelse_ast(Expression, IfAstInfo, ElseAstInfo, Context);

process_body_ast({'ifelse', Variable, IfContents, ElseContents}, Context) ->
    IfAstInfo = body_ast(IfContents, Context),
    ElseAstInfo = body_ast(ElseContents, Context),
    ifelse_ast(Variable, IfAstInfo, ElseAstInfo, Context);

process_body_ast({'for', {'in', IteratorList, Expression}, Contents}, Context) ->
    for_loop_ast(IteratorList, Expression, Contents, Context);

process_body_ast({'cycle', Names}, Context) ->
    cycle_ast(Names, Context).


block_ast(Name, Contents, Context) ->
    NameAst = erl_syntax:string(Name),
    ContextAst = erl_syntax:variable("Variables"),
    RenderModuleAst = erl_syntax:atom(Context#dtl_context.renderer_module),

    BlockCheckAst = erl_syntax:application(RenderModuleAst,
                                           erl_syntax:atom(have_child_block), 
                                           [NameAst, ContextAst]),
    TrueAst = erl_syntax:clause([erl_syntax:atom(true)], none, 
                                [erl_syntax:application(
                                   RenderModuleAst,
                                   erl_syntax:atom(render_block), 
                                   [NameAst, ContextAst])]),

    NewContextAst = erl_syntax:variable("NewContext"),
    FalseCaseAst = [erl_syntax:match_expr(NewContextAst,
                    erl_syntax:application(RenderModuleAst,
                                           erl_syntax:atom(set_block),
                                           [NameAst, 
                                            body_ast(Contents, Context), ContextAst])),
                    erl_syntax:application(
                      RenderModuleAst,
                      erl_syntax:atom(render_block), 
                      [NameAst, NewContextAst])
                   ],
    FalseAst = erl_syntax:clause([erl_syntax:atom(false)], none, FalseCaseAst),
    erl_syntax:case_expr(BlockCheckAst, [TrueAst, FalseAst]).

empty_ast() ->
    erl_syntax:list([]).


string_ast(String, Context) ->
    case Context#dtl_context.debug of
        true ->
            erl_syntax:string(String); %% less verbose AST, better for development and debugging
        _ ->
            erl_syntax:abstract(list_to_binary(String))
    end.

resolve_scoped_variable_ast(VarName, Context) ->
    lists:foldl(fun(Scope, Value) ->
                case Value of
                    undefined -> proplists:get_value(VarName, Scope);
                    _ -> Value
                end
        end, undefined, Context#dtl_context.local_scopes).

format(Ast, Context) ->
    auto_escape(format_number_ast(Ast), Context).


format_number_ast(Ast) ->
    erl_syntax:application(erl_syntax:atom(erlydtl_filters), erl_syntax:atom(format_number),
        [Ast]).


auto_escape(Value, Context) ->
    case Context#dtl_context.auto_escape of
        on ->
            erl_syntax:application(erl_syntax:atom(erlydtl_filters), erl_syntax:atom(force_escape),
                [Value]);
        _ ->
            Value
    end.


ifelse_ast(Expression, IfContentsAst, ElseContentsAst, Context) ->
    Ast = resolve_expression(Expression, Context),
    
    erl_syntax:case_expr(erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(is_true), [Ast]),
        [erl_syntax:clause([erl_syntax:atom(false)], none, 
                [ElseContentsAst]),
            erl_syntax:clause([erl_syntax:underscore()], none,
                [IfContentsAst])
        ]).

for_loop_ast(IteratorList, Expression, Contents, Context) ->
    Vars = lists:map(fun({identifier, _, Iterator}) -> 
                    erl_syntax:variable("Var_" ++ Iterator) 
            end, IteratorList),
    InnerAst = body_ast(Contents,
        Context#dtl_context{local_scopes = [
                [{"forloop", erl_syntax:variable("Counters")} | lists:map(
                    fun({identifier, _, Iterator}) ->
                            {Iterator, erl_syntax:variable("Var_" ++ Iterator)} 
                    end, IteratorList)] | Context#dtl_context.local_scopes]}),
    CounterAst = erl_syntax:application(erl_syntax:atom(erlydtl_runtime), 
        erl_syntax:atom(increment_counter_stats), [erl_syntax:variable("Counters")]),
    ListAst = resolve_expression(Expression, Context),
    CounterVars0 = case resolve_scoped_variable_ast("forloop", Context) of
        undefined ->
            erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(init_counter_stats), [ListAst]);
        Value ->
            erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(init_counter_stats), [ListAst, Value])
    end,
    erl_syntax:application(
            erl_syntax:atom('erlang'), erl_syntax:atom('element'),
            [erl_syntax:integer(1), erl_syntax:application(
                    erl_syntax:atom('lists'), erl_syntax:atom('mapfoldl'),
                    [erl_syntax:fun_expr([
                                erl_syntax:clause([erl_syntax:tuple(Vars), erl_syntax:variable("Counters")], none, 
                                    [erl_syntax:tuple([InnerAst, CounterAst])]),
                                erl_syntax:clause(case Vars of [H] -> [H, erl_syntax:variable("Counters")];
                                        _ -> [erl_syntax:list(Vars), erl_syntax:variable("Counters")] end, none, 
                                    [erl_syntax:tuple([InnerAst, CounterAst])])
                            ]),
                        CounterVars0, ListAst])]).

cycle_ast(Names, Context) ->
    NamesAst = resolve_expression(Names, Context),
    erl_syntax:application(
        erl_syntax:atom('erlydtl_runtime'), erl_syntax:atom('cycle'),
        [NamesAst, erl_syntax:variable("Counters")]).


resolve_expression({expression, Pos, Expression}, Context) ->
    case erlang_el:compile(Expression, Pos, context_ast(Context)) of
        {ok, Ast} -> Ast;
        {error, _} = Error -> throw(Error)
    end.

context_ast(Context) ->
    Extension = 
        lists:foldl(fun(Scope, AccScope) ->
                            lists:foldl(fun({Key, Val}, Acc) ->
                                            [erl_syntax:tuple([erl_syntax:string(Key), Val])| Acc]
                                        end, AccScope, Scope)
                    end, [], Context#dtl_context.local_scopes), 
    erl_syntax:application(erl_syntax:atom(erlydtl_runtime),
                           erl_syntax:atom(extend_dict),
                           [erl_syntax:variable("Variables"), 
                            erl_syntax:list(Extension)]).

include_ast(Path, Context) ->
    include_ast(Path, context_ast(Context), Context).

include_ast(Path, NewContextAst, Context) ->
    AppAst = erl_syntax:application(
               erl_syntax:atom(Context#dtl_context.renderer_module),
               erl_syntax:atom(render),
               [Path, NewContextAst, erl_syntax:abstract(Context#dtl_context.renderer_params)]),
    Ext = case erlang:now() of
              {MegaSec, Sec, MicroSec} -> 
                   "_" ++integer_to_list(MegaSec) ++
                   "_" ++integer_to_list(Sec) ++
                   "_" ++integer_to_list(MicroSec)
          end,
    RenderedAst = erl_syntax:variable("Rendered" ++ Ext),
    OkAst = erl_syntax:clause(
              [erl_syntax:tuple([erl_syntax:atom(ok), RenderedAst])], 
              none,
              [RenderedAst]),
    ReasonAst = erl_syntax:variable("Reason" ++ Ext),
    ErrStrAst = erl_syntax:application(
                  erl_syntax:atom(io_lib),
                  erl_syntax:atom(format),
                  [erl_syntax:string("error: ~p"), erl_syntax:list([ReasonAst])]),
    ErrorAst = erl_syntax:clause(
                 [erl_syntax:tuple([erl_syntax:atom(error), ReasonAst])], 
                 none,
                 [ErrStrAst]),
    erl_syntax:case_expr(AppAst, [OkAst, ErrorAst]).

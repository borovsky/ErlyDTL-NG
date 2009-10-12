
%%% File:      erlydtl_scanner.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @author    Alexander Borovsky <alex.borovsky@gmail.com>
%%% @copyright 2008, 2009 Roberto Saccon, Evan Miller, Alexander Borovsky
%%% @doc 
%%% Template language scanner
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
%%% @since 2007-11-11 by Roberto Saccon, Evan Miller
%%%-------------------------------------------------------------------
-module(erlydtl_scanner).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').
-author('alex.borovsky@gmail.com').

-export([scan/1]). 


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec scan(T::template()) -> {ok, S::tokens()} | {error, Reason}
%% @type template() = string() | binary(). Template to parse
%% @type tokens() = [tuple()].
%% @doc Scan the template string T and return the a token list or
%% an error.
%% @end
%%--------------------------------------------------------------------
scan(Template) ->
    scan(Template, [], {1, 1}, in_text).

scan([], Scanned, _, in_text) ->
    {ok, lists:reverse(lists:map(fun post_process/1, Scanned))};

scan([], _Scanned, _, {in_comment, _}) ->
    {error, "Reached end of file inside a comment."};

scan([], _Scanned, _, _) ->
    {error, "Reached end of file inside a code block."};


% Comments traversal
scan("<!--{#" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, Scanned, {Row, Column + length("<!--{#")}, {in_comment, "#}-->"});

scan("{#" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, Scanned, {Row, Column + 2}, {in_comment, "#}"});

scan("#}-->" ++ T, Scanned, {Row, Column}, {in_comment, "#}-->"}) ->
    scan(T, Scanned, {Row, Column + length("#}-->")}, in_text);

scan("#}" ++ T, Scanned, {Row, Column}, {in_comment, "#}"}) ->
    scan(T, Scanned, {Row, Column + 2}, in_text);

scan([_ | T], Scanned, {Row, Column}, {in_comment, Closer}) ->
    scan(T, Scanned, {Row, Column + 1}, {in_comment, Closer});



% Code tags scanning
scan("<!--{{" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [{open_var, {Row, Column}, "<!--{{"} | Scanned], {Row, Column + length("<!--{{")}, {in_code, "}}-->"});

scan("{{" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [{open_var, {Row, Column}, "{{"} | Scanned], {Row, Column + 2}, {in_code, "}}"});

scan("<!--{%" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [{open_tag, {Row, Column}, lists:reverse("<!--{%")} | Scanned], 
        {Row, Column + length("<!--{%")}, {in_code, "%}-->"});

scan("{%" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [{open_tag, {Row, Column}, lists:reverse("{%")} | Scanned], 
        {Row, Column + 2}, {in_code, "%}"});

scan("}}-->" ++ T, Scanned, {Row, Column}, {_, "}}-->"}) ->
    scan(T, [{close_var, {Row, Column}, lists:reverse("}}-->")} | Scanned], 
        {Row, Column + 2}, in_text);

scan("}}" ++ T, Scanned, {Row, Column}, {_, "}}"}) ->
    scan(T, [{close_var, {Row, Column}, "}}"} | Scanned], {Row, Column + 2}, in_text);

scan("%}-->" ++ T, Scanned, {Row, Column}, {_, "%}-->"}) ->
    scan(T, [{close_tag, {Row, Column}, lists:reverse("%}-->")} | Scanned], 
        {Row, Column + 2}, in_text);

scan("%}" ++ T, Scanned, {Row, Column}, {_, "%}"}) ->
    scan(T, [{close_tag, {Row, Column}, lists:reverse("%}")} | Scanned], 
        {Row, Column + 2}, in_text);

% Tag name scanning
scan(" " ++ T, Scanned, {Row, Column}, {in_code, Closer}) ->
    scan(T, Scanned,  {Row, Column + 1}, {in_code, Closer});

scan(" " ++ T, [H | Scanned], {Row, Column}, {in_identifier, Closer}) ->
    scan(T, [process_tag_name(H) | Scanned],  {Row, Column + 1}, {in_expression, Closer});

scan([H | T], Scanned, {Row, Column}, {in_code, Closer}) ->
    case char_type(H) of
        letter ->
            scan(T, [{identifier, {Row, Column}, [H]} | Scanned], {Row, Column + 1}, {in_identifier, Closer});
        _ ->
            scan(T, [{expression, {Row, Column}, [H]} | Scanned], {Row, Column + 1}, {in_expression, Closer})
    end;

scan([H | T], [{identifier, Pos, Identifier}| Scanned], {Row, Column}, {in_identifier, Closer}) ->
    case char_type(H) of
        letter ->
            scan(T, [{identifier, Pos, [H | Identifier]} | Scanned], {Row, Column + 1}, {in_identifier, Closer});
        _ ->
            scan(T, [{expression, Pos, [H | Identifier]} | Scanned], {Row, Column + 1}, {in_expression, Closer})
    end;

scan(Input, [{for_keyword, _, _} | _] = Scanned, Pos, {in_expression, Closer}) ->
    scan(Input, Scanned, Pos, {in_for_list, Closer});

scan(Input, [{block_keyword, _, _} | _] = Scanned, Pos, {in_expression, Closer}) ->
    scan(Input, Scanned, Pos, {in_block, Closer});

% For tag scanning
scan(" " ++ T, Scanned, {Row, Column}, {in_for_list, Closer}) ->
    scan(T, Scanned, {Row, Column + 1}, {in_for_list, Closer});

scan(" " ++ T, Scanned, {Row, Column}, {in_for_list_identifier, Closer}) ->
    scan(T, Scanned, {Row, Column + 1}, {in_for_list, Closer});

scan("," ++ T, Scanned, {Row, Column}, {in_for_list_identifier, Closer}) ->
    scan(T, [{comma, {Row, Column}, ","} | Scanned], {Row, Column + 1}, {in_for_list, Closer});

scan("in " ++ T, Scanned, {Row, Column}, {in_for_list_identifier, Closer}) ->
    scan(T, [{in_keyword, {Row, Column}, lists:reverse("in ")} | Scanned], {Row, Column + 3}, {in_expression, Closer});

scan("in " ++ T, Scanned, {Row, Column}, {in_for_list, Closer}) ->
    scan(T, [{in_keyword, {Row, Column}, lists:reverse("in ")} | Scanned], {Row, Column + 3}, {in_expression, Closer});

scan([H | T], Scanned, {Row, Column}, {in_for_list, Closer}) ->
    case char_type(H) of
        Type when (Type == letter) or (Type == underscore) ->
            scan(T, [{identifier, {Row, Column}, [H]} | Scanned], {Row, Column + 1}, {in_for_list_identifier, Closer});
        _ ->
            {error, io:format("Error while parsing for list at ~p", [{Row, Column}])}
    end;

scan([H | T], [{identifier, Pos, String} | Scanned], {Row, Column}, {in_for_list_identifier, Closer}) ->
    case char_type(H) of
        Type when ((Type == letter) or (Type == underscore) or (Type == digit)) ->
            scan(T, [{identifier, Pos, [H | String]} | Scanned], {Row, Column + 1}, {in_for_list_identifier, Closer});
        _ ->
            {error, io:format("Error while parsing for list at ~p", [{Row, Column}])}
    end;

% Block scanning
scan(" " ++ T, Scanned, {Row, Column}, {in_block_identifier, Closer}) ->
    scan(T, Scanned, {Row, Column + 1}, {in_block, Closer});

scan([H | T], Scanned, {Row, Column}, {in_block, Closer}) ->
    case char_type(H) of
        Type when (Type == letter) or (Type == underscore) ->
            scan(T, [{identifier, {Row, Column}, [H]} | Scanned], {Row, Column + 1}, {in_block_identifier, Closer});
        _ ->
            {error, io:format("Error while parsing for list at ~p", [{Row, Column}])}
    end;

scan([H | T], [{identifier, Pos, String} | Scanned], {Row, Column}, {in_block_identifier, Closer}) ->
    case char_type(H) of
        Type when ((Type == letter) or (Type == underscore) or (Type == digit)) ->
            scan(T, [{identifier, Pos, [H | String]} | Scanned], {Row, Column + 1}, {in_block_identifier, Closer});
        _ ->
            {error, io:format("Error while parsing for list at ~p", [{Row, Column}])}
    end;


% Text scanning
scan("\n" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, append_text_char(Scanned, {Row, Column}, $\n), {Row + 1, 1}, in_text);

scan([H | T], Scanned, {Row, Column}, in_text) ->
    scan(T, append_text_char(Scanned, {Row, Column}, H), {Row, Column + 1}, in_text);


% Expression scanning
scan("\"" ++ T, Scanned, {Row, Column}, {in_expression, Closer}) ->
    scan(T, append_expression_char(Scanned, {Row, Column}, $"), {Row, Column + 1}, {in_double_quote, Closer});

scan("\'" ++ T, Scanned, {Row, Column}, {in_expression, Closer}) ->
    scan(T, append_expression_char(Scanned, {Row, Column}, $'), {Row, Column + 1}, {in_single_quote, Closer});

scan([$\\ | T], Scanned, {Row, Column}, {in_double_quote, Closer}) ->
    scan(T, append_char(Scanned, $\\), {Row, Column + 1}, {in_double_quote_slash, Closer});

scan([H | T], Scanned, {Row, Column}, {in_double_quote_slash, Closer}) ->
    scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_double_quote, Closer});

scan([$\\ | T], Scanned, {Row, Column}, {in_single_quote, Closer}) ->
    scan(T, append_char(Scanned, $\\), {Row, Column + 1}, {in_single_quote_slash, Closer});

scan([H | T], Scanned, {Row, Column}, {in_single_quote_slash, Closer}) ->
    scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_single_quote, Closer});

scan("\"" ++ T, Scanned, {Row, Column}, {in_double_quote, Closer}) ->
    scan(T, append_char(Scanned, $"), {Row, Column + 1}, {in_expression, Closer});

scan("\'" ++ T, Scanned, {Row, Column}, {in_single_quote, Closer}) ->
    scan(T, append_char(Scanned, $'), {Row, Column + 1}, {in_expression, Closer});

scan([H | T], Scanned, {Row, Column}, {in_double_quote, Closer}) ->
    scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_double_quote, Closer});

scan([H | T], Scanned, {Row, Column}, {in_single_quote, Closer}) ->
    scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_single_quote, Closer});

scan([H | T], Scanned, {Row, Column}, {in_expression, Closer}) ->
    scan(T, append_expression_char(Scanned, {Row, Column}, H), {Row, Column + 1}, {in_expression, Closer}).

% internal functions

append_char([{Type, Pos, String} | Scanned], Char) ->
    [{Type, Pos, [Char | String]} | Scanned].

append_text_char([], Pos, Char) ->
    [{text, Pos, [Char]}];

append_text_char([{text, Pos, String} | Scanned1], _, Char) ->
    [{text, Pos, [Char | String]} | Scanned1];

append_text_char(Scanned, Pos, Char) ->
    [{text, Pos, [Char]} | Scanned].

append_expression_char([{expression, Pos, String} | Scanned], _, Char) ->
    [{expression, Pos, [Char | String]} | Scanned];

append_expression_char(Scanned, Pos, Char) ->
    [{expression, Pos, [Char]} | Scanned].

char_type(Char) ->
    case Char of 
        C when ((C >= $a) and (C =< $z)) or ((C >= $A) and (C =< $Z)) ->
            letter;
        $_ -> underscore;
        C when ((C >= $0) and (C =< $9)) ->
            digit;
        _ ->
            undefined
    end.

process_tag_name({identifier, Pos, String}) ->
    RevString = lists:reverse(String),
    TagNames = ["for",
                "endfor",
                "include",
                "block",
                "endblock",
                "extends",
                "if",
                "else",
                "endif",
                "comment",
                "endcomment",
                "cycle"
               ],
    Type = case lists:member(RevString, TagNames) of
               true ->
                   list_to_atom(RevString ++ "_keyword");
               _ ->
                   expression
           end,
    {Type, Pos, String}.

post_process({Type, Pos, String}) ->
    {Type, Pos, lists:reverse(String)}.

%%%-------------------------------------------------------------------
%%% File:      erlydtl_parser.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @author    Alexander Borovsky <alex.borovsky@gmail.com>
%%% @copyright 2007 - 2009 Roberto Saccon, Evan Miller, Alexander Borovsky
%%% @doc Template language grammar
%%% @reference  See <a href="http://erlydtl.googlecode.com" target="_top">http://erlydtl.googlecode.com</a> for more information
%%% @end  
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007-2009 Roberto Saccon, Evan Miller, Alexander Borovsky
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

Nonterminals
    Elements
    Expression

    ValueBraced

    ExtendsTag
    IncludeTag

    BlockBlock
    BlockBraced
    EndBlockBraced

    CommentBlock
    CommentBraced
    EndCommentBraced

    CycleTag

    ForBlock
    ForBraced
    EndForBraced
    ForExpression
    ForGroup

    IfBlock
    IfBraced
    ElseBraced
    EndIfBraced.

Terminals
    block_keyword
    close_tag
    close_var
    comma
    comment_keyword
    cycle_keyword
    else_keyword
    endblock_keyword
    endcomment_keyword
    endfor_keyword
    endif_keyword
    expression
    extends_keyword
    for_keyword
    identifier
    if_keyword
    in_keyword
    include_keyword
    open_tag
    open_var
    text.

Rootsymbol
    Elements.

Elements -> '$empty' : [].
Elements -> Elements text : '$1' ++ ['$2'].
Elements -> Elements ValueBraced : '$1' ++ ['$2'].
Elements -> Elements ExtendsTag : '$1' ++ ['$2'].
Elements -> Elements IncludeTag : '$1' ++ ['$2'].
Elements -> Elements CycleTag : '$1' ++ ['$2'].
Elements -> Elements BlockBlock : '$1' ++ ['$2'].
Elements -> Elements ForBlock : '$1' ++ ['$2'].
Elements -> Elements IfBlock : '$1' ++ ['$2'].
Elements -> Elements CommentBlock : '$1' ++ ['$2'].

ValueBraced -> open_var Expression close_var : '$2'.

Expression -> expression : '$1'.

ExtendsTag -> open_tag extends_keyword Expression close_tag : {extends, '$3'}.
IncludeTag -> open_tag include_keyword Expression close_tag : {include, '$3'}.

BlockBlock -> BlockBraced Elements EndBlockBraced : {block, '$1', '$2'}.
BlockBraced -> open_tag block_keyword identifier close_tag : '$3'.
EndBlockBraced -> open_tag endblock_keyword close_tag.

CommentBlock -> CommentBraced Elements EndCommentBraced : {comment, '$2'}.
CommentBraced -> open_tag comment_keyword close_tag.
EndCommentBraced -> open_tag endcomment_keyword close_tag.

CycleTag -> open_tag cycle_keyword Expression close_tag : {cycle, '$3'}.

ForBlock -> ForBraced Elements EndForBraced : {for, '$1', '$2'}.
ForBraced -> open_tag for_keyword ForExpression close_tag : '$3'.
EndForBraced -> open_tag endfor_keyword close_tag.
ForExpression -> ForGroup in_keyword Expression : {'in', '$1', '$3'}.
ForGroup -> identifier : ['$1'].
ForGroup -> ForGroup comma identifier : '$1' ++ ['$3'].

IfBlock -> IfBraced Elements ElseBraced Elements EndIfBraced : {ifelse, '$1', '$2', '$4'}.
IfBlock -> IfBraced Elements EndIfBraced : {'if', '$1', '$2'}.
IfBraced -> open_tag if_keyword Expression close_tag : '$3'.

ElseBraced -> open_tag else_keyword close_tag.
EndIfBraced -> open_tag endif_keyword close_tag.


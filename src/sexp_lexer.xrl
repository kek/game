Definitions.

Whitespace  = [\000-\s]
Chars       = [a-z!?_]+
LeftParen   = \(
RightParen  = \)
Number      = [0-9]+
Operator    = [+\-/*%$&=\\]
%% DoubleQuote = \"
%% Text        = [^\"]*

Rules.

{Whitespace}  : skip_token.
{LeftParen}   : {token, {'(', TokenLine}}.
{RightParen}  : {token, {')', TokenLine}}.
{Chars}       : {token, {atom, TokenLine, TokenChars}}.
{Number}      : {token, {number, TokenLine, TokenChars}}.
{Operator}    : {token, {operator, TokenLine, TokenChars}}.
%% {DoubleQuote} : {token, {double_quote, TokenLine, TokenChars}}.
%% {Text}        : {token, {text, TokenLine, TokenChars}}.

Erlang code.

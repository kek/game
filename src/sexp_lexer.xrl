Definitions.

Operator   = [+\-]
Number     = [0-9]+
Whitespace = [\000-\s]
Symbol     = [a-z]+
LeftParen  = \(
RightParen = \)

Rules.

{Whitespace} : skip_token.
{Operator}   : {token, {operator, TokenLine, TokenChars}}.
{LeftParen}  : {token, {open, TokenLine, TokenChars}}.
{RightParen} : {token, {close, TokenLine, TokenChars}}.
{Number}     : {token, {digit, TokenLine, TokenChars}}.
{Symbol}     : {token, {symbol, TokenLine, TokenChars}}.

Erlang code.

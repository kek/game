Nonterminals list elements element.
Terminals atom '(' ')' number operator.
Rootsymbol list.

list -> '(' elements ')' : '$2'.
elements -> element elements : ['$1'] ++ '$2'.
elements -> '$empty' : [].
element -> atom : '$1'.
element -> list : '$1'.
element -> number : to_integer('$1').
element -> operator : '$1'.

%% string -> double_quote text double_quote : {string, '$3'}.

Erlang code.

to_integer({number, TokenLine, TokenChars}) -> {number, TokenLine, list_to_integer(TokenChars)}.

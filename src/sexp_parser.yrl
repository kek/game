Nonterminals
expr
arg_list
item
.

Terminals
operator
digit
symbol
open
close
.

Rootsymbol expr.

expr -> open operator arg_list close : {expr, value_of('$2'), '$3'}.
expr -> open symbol arg_list close : {expr, '$2', '$3'}.
expr -> open operator close : {expr, '$2'}.
expr -> open symbol close : {expr, '$2'}.

arg_list -> expr : ['$1'].
arg_list -> expr arg_list : ['$1'] ++ '$2'.
arg_list -> symbol : ['$1'].
arg_list -> symbol arg_list : ['$1'] ++ '$2'.
arg_list -> digit : [{digit, ?l2i(value_of('$1'))}].
arg_list -> digit arg_list : [{digit, ?l2i(value_of('$1'))}] ++ '$2'.

Erlang code.

-define(l2i(L), list_to_integer(L)).
value_of({_,_,V}) -> V.


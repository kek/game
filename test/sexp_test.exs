defmodule SexpTest do
  use ExUnit.Case

  test "lexer" do
    assert {:ok,
            [
              {:"(", _, _},
              {:operator, _, '+'},
              {:digit, _, '1'},
              {:digit, _, '2'},
              {:digit, _, '3'},
              {:")", _, _}
            ], _} = :sexp_lexer.string('(+ 1 2 3)')
  end

  test "parser" do
    {:ok, tokens, _} = :sexp_lexer.string('(+ 1 2 (- 3))')

    assert {:ok, {:expr, '+', [{:digit, 1}, {:digit, 2}, {:expr, '-', [{:digit, 3}]}]}} =
             :sexp_parser.parse(tokens)
  end

  test "parse symbols" do
    assert {:ok, tokens, _} = :sexp_lexer.string('(f x)')
  end
end

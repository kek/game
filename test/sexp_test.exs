defmodule SexpTest do
  use ExUnit.Case

  test "lexer" do
    assert {:ok,
            [
              {:open, _, _},
              {:operator, _, '+'},
              {:digit, _, '1'},
              {:digit, _, '2'},
              {:digit, _, '3'},
              {:close, _, _}
            ], _} = :sexp_lexer.string('(+ 1 2 3)')
  end

  test "parser" do
    {:ok, tokens, _} = :sexp_lexer.string('(+ 1 2 (- 3))')

    assert {:ok, {:expr, '+', [{:digit, 1}, {:digit, 2}, {:expr, '-', [{:digit, 3}]}]}} =
             :sexp_parser.parse(tokens)
  end

  test "parse operator without argument" do
    assert {:ok, tokens, _} = :sexp_lexer.string('(+)')
    assert {:ok, _} = :sexp_parser.parse(tokens)
  end

  test "parse symbols" do
    assert {:ok, tokens, _} = :sexp_lexer.string('(f)')
    assert {:ok, _} = :sexp_parser.parse(tokens)
  end

  test "parse symbols with digit arguments" do
    assert {:ok, tokens, _} = :sexp_lexer.string('(f 1)')
    assert {:ok, _} = :sexp_parser.parse(tokens)
  end

  test "parse symbols as arguments" do
    assert {:ok, tokens, _} = :sexp_lexer.string('(f x 1 y)')
    assert {:ok, _} = :sexp_parser.parse(tokens)
  end

  test "parse nested list" do
    assert {:ok, tokens, _} = :sexp_lexer.string('(a (b) c)')
    assert {:ok, _} = :sexp_parser.parse(tokens)
  end
end

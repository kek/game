defmodule Game.Sexp do
  @doc """
  Parses a symbolic expression.

  Examples:

  iex> parse("(+ 1 2)")
  {:expr, '+', [digit: 1, digit: 2]}

  iex> parse("(+)")
  {:expr, {:operator, 1, '+'}}
  iex> parse("(f)")
  {:expr, {:symbol, 1, 'f'}}
  iex> parse("(f 1)")
  {:expr, {:symbol, 1, 'f'}, [digit: 1]}
  iex> parse("(f x 1 y)")
  {:expr, {:symbol, 1, 'f'},
          [{:symbol, 1, 'x'},
           {:digit, 1},
           {:symbol, 1, 'y'}]}
  iex> parse("(a (b) c)")
  {:expr, {:symbol, 1, 'a'},
          [{:expr, {:symbol, 1, 'b'}},
           {:symbol, 1, 'c'}]}
  """
  def parse(code) do
    with {:ok, tokens, _} <- code |> String.to_charlist() |> :sexp_lexer.string(),
         {:ok, tree} <- :sexp_parser.parse(tokens) do
      tree
    end
  end
end

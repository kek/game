defmodule Game.Sexp do
  @doc """
  Parses a symbolic expression.

  Examples:

  iex> parse("(steve!)")
  [{:atom, 1, 'steve!'}]
  iex> parse("(steve! steve!)")
  [{:atom, 1, 'steve!'}, {:atom, 1, 'steve!'}]
  iex> parse("(steve! (steve!))")
  [{:atom, 1, 'steve!'}, [{:atom, 1, 'steve!'}]]
  iex> parse("(steve! (steve! steve!))")
  [{:atom, 1, 'steve!'}, [{:atom, 1, 'steve!'}, {:atom, 1, 'steve!'}]]
  iex> parse("(steve! (steve! steve!) steve!)")
  [{:atom, 1, 'steve!'}, [{:atom, 1, 'steve!'}, {:atom, 1, 'steve!'}], {:atom, 1, 'steve!'}]
  iex> parse("(42)")
  [{:number, 1, 42}]
  iex> parse("(+ - / * % $ & = \\\\)")
  [{:operator, 1, '+'}, {:operator, 1, '-'}, {:operator, 1, '/'}, {:operator, 1, '*'}, {:operator, 1, '%'}, {:operator, 1, '$'}, {:operator, 1, '&'}, {:operator, 1, '='}, {:operator, 1, '\\\\'}]
  iex> parse("(snake_man)")
  [{:atom, 1, 'snake_man'}]
  """
  def parse(code) do
    with {:ok, tokens, _} <- code |> String.to_charlist() |> :sexp_lexer.string(),
         {:ok, tree} <- :sexp_parser.parse(tokens) do
      tree
    end
  end
end

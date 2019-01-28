defmodule Game.Language do
  def run(source) do
    {:ok, code} = parse(source)
    {:ok, ast} = compile(code)
    eval(ast)
  end

  @doc """
  Parses a source string.

  Examples:
  iex> parse("()")
  {:ok, []}
  iex> parse("(f x)")
  {:ok, [:f, :x]}
  iex> parse(")")
  {:error, {1, :lfe_parse, {:illegal, :")"}}}
  """
  def parse(source) do
    source
    |> String.to_charlist()
    |> :lfe_io.read_string()
  end

  def eval(ast) do
    {result, _} = Code.eval_quoted(ast)
    result
  end

  @doc """
  Compiles a symbolic expression to Elixir AST.

  Examples:
  iex> compile([:+, 1, 2])
  {:ok, {:+, [], [1, 2]}}
  """
  def compile([:+ | params]) do
    ast(:+, params)
  end

  defp ast(function, params) do
    {:ok, {function, [], params}}
  end
end

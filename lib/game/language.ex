defmodule Game.Language do
  def run(source) do
    {:ok, code} = parse(source)
    eval(code)
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

  def eval([function | params]) do
    code = {function, [context: Elixir, import: Kernel], params}
    IO.inspect(code)
    {result, _} = Code.eval_quoted(code)
    result
  end
end

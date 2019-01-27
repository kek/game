defmodule Game.Language do
  def run(source) do
    source
    |> parse()
    |> eval()
  end

  @doc """
  Parses a source string.

  Examples:
  iex> parse("()")
  {:ok, []}
  """
  def parse(source) do
    source
    |> String.to_charlist()
    |> :lfe_io.read_string()
  end

  def eval(code) do
    code
    |> IO.inspect()
    |> Code.eval_quoted()
  end
end

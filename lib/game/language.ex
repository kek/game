defmodule Game.Language do
  def run(source) do
    source
    |> parse()
    |> eval()
  end

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

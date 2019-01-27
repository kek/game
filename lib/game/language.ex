defmodule Game.Language do
  def run(source) do
    source
    |> parse()
    |> eval()
  end

  def parse(source) do
    []
  end

  def eval(code) do
    code
    |> IO.inspect()
    |> Code.eval_quoted()
  end
end

defmodule Game.Commands do
  use Symbelix.Library
  require Logger

  def load() do
    :ok
  end

  def say() do
    "say something"
  end

  def say(word) do
    Logger.info("someone says #{word}!")
    "you say #{inspect(word)}?"
  end

  def who() do
    Game.World.players()
    |> Enum.map(&Game.Player.name/1)
  end

  def foreach(list, function) do
    list
    |> Enum.map(fn item ->
      {:ok, ast} = generate_ast([function, item])
      Logger.info(inspect(ast))
      {result, _binding} = Code.eval_quoted(ast)
      result
    end)
  end
end

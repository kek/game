defmodule Game.Commands do
  use Symbelix.Library
  require Logger

  def load() do
    :ok
  end

  def say() do
    "say something"
  end

  def say(saying) do
    Logger.info("someone says #{saying}!")

    Game.World.players()
    |> Enum.map(fn player ->
      Logger.info(inspect({player, self()}))

      if player == self() do
        Logger.info("it's me!")
      else
        Logger.info(
          "CALL: #{inspect(self())} Player notify #{inspect({:saying, saying})} to #{
            inspect(player)
          }- commands.ex say"
        )

        Game.Player.notify(player, {:saying, self(), saying})
      end
    end)

    "You say #{saying}."
  end

  def who() do
    Game.World.players()
    |> Enum.map(fn player ->
      if player == self() do
        "Me"
      else
        Logger.info("Asking name of #{inspect(player)}")
        Game.Player.name(player)
      end
    end)
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

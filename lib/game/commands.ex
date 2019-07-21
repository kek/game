defmodule Game.Commands do
  use Symbelix.Library
  require Logger
  alias Game.{Player, World}

  def load() do
    :ok
  end

  def say() do
    "say something"
  end

  def say(words) do
    saying = Enum.join(words, " ")
    Logger.info("someone says #{saying}!")

    World.players()
    |> Enum.map(fn player ->
      Player.notify(player, {:saying, self(), saying})
    end)

    "You say #{inspect(saying)}."
  end

  def who([]) do
    World.players()
    |> Enum.map(fn player ->
      if player == self() do
        "Me"
      else
        Logger.info("Asking name of #{inspect(player)}")
        Player.name(player)
      end
    end)
  end

  def edit([name]) do
    Player.edit(self(), name)
  end

  def edit(_) do
    Player.notify(self(), "Usage: edit <object>")
  end

  def foreach([list, function]) do
    list
    |> Enum.map(fn item ->
      {:ok, ast} = generate_ast([function, item])
      Logger.debug(inspect(ast))
      {result, _binding} = Code.eval_quoted(ast)
      result
    end)
  end

  def quit([]) do
    Logger.debug("Quitting #{inspect(self())}!")
    Player.log_off(self())
    :quit
  end
end

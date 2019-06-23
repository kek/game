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

  def mode([mode]) do
    Player.change_mode(self(), mode)
  end

  def edit([]) do
    Player.change_mode(self(), Game.Conversation.Editor)
  end

  def foreach([list, function]) do
    list
    |> Enum.map(fn item ->
      {:ok, ast} = generate_ast([function, item])
      Logger.info(inspect(ast))
      {result, _binding} = Code.eval_quoted(ast)
      result
    end)
  end
end

defmodule Game.Commands do
  use Symbelix.Library
  require Logger
  alias Game.{Player, World, Object}

  def load() do
    :ok
  end

  def help([]) do
    output("Help!")

    output(
      "Available commands: say <text>, who, edit <object>, look, look <object>, run <object>, quit"
    )
  end

  def say(words) do
    Logger.debug("say #{inspect(words)}")
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
    |> Enum.each(fn player ->
      if player == self() do
        output("You")
      else
        Logger.info("Asking name of #{inspect(player)}")
        output(Player.name(player))
      end
    end)
  end

  def edit([]) do
    output("Usage: edit <object>")
  end

  def edit(name_words) do
    object_name = Enum.join(name_words, " ")
    Player.edit(self(), object_name)
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

  def l(x), do: look(x)

  def look([]) do
    case World.objects() do
      [] ->
        output("There is nothing here.")

      objects ->
        objects
        |> Enum.map(&Object.name/1)
        |> Enum.each(&output/1)
    end
  end

  def look(object_name_words) do
    object_name = Enum.join(object_name_words, " ")

    object_name
    |> World.lookup_object()
    |> case do
      nil ->
        output("There is no #{object_name}.")

      something ->
        something
        |> Object.get_state()
        |> Map.get(:code)
        |> Enum.each(&output/1)
    end
  end

  def run([]), do: output("Usage: run <object>")

  def run(object_name_words) do
    object_name_words
    |> Enum.join(" ")
    |> World.lookup_object()
    |> Object.run()
    |> inspect()
  end

  def bg(object_name_words) do
    object_name_words
    |> Enum.join(" ")
    |> World.lookup_object()
    |> Object.bg()
    |> inspect()
  end

  def delete(object_name_words) do
    object_name_words
    |> Enum.join(" ")
    |> World.delete_object()
  end

  def restart(_) do
    output("Restarting, please log in again.")

    Game.World
    |> Process.whereis()
    |> Process.exit(:kill)

    Player.log_off(self())
  end

  def reload(_) do
    Game.World.reload()
  end

  defp output(text) do
    Player.notify(self(), text)
  end
end

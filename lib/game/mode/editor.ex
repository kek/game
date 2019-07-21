defmodule Game.Mode.Editor do
  require Logger
  alias Game.{Player, Mode.Normal}

  def perform(player, ".") do
    lines = Process.get(:lines)
    text = Enum.join(lines, "\r\n")
    Logger.debug("Exiting editor")
    Player.change_mode(player, Normal)
    Player.notify(player, text)
  end

  def perform(_me, line) do
    lines = Process.get(:lines)
    Process.put(:lines, lines ++ [line])
  end

  def intro() do
    Process.put(:lines, [])
    "Edit mode. \".\" to end input"
  end

  def prompt() do
    ": "
  end
end

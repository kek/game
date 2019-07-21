defmodule Game.Mode.Editor do
  require Logger
  alias Game.Player

  def perform(player, ".") do
    lines = Process.get(:lines)
    Logger.debug("Exiting editor")
    Player.done_editing(player, lines)
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
    "| "
  end
end

defmodule Game.Mode.Editor do
  require Logger
  alias Game.{Player, Mode.Normal}

  def perform(player, ".") do
    Logger.debug("Exiting editor")
    Player.change_mode(player, Normal)
  end

  def perform(me, line) do
    Player.notify(me, {:saying, me, "ok #{line}"})
  end

  def intro() do
    "Edit mode"
  end

  def prompt() do
    ":"
  end
end

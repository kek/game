defmodule Game.Mode.Normal do
  require Logger
  alias Game.{Player}

  def perform(me, input) do
    if input =~ ~r/^\s*$/ do
      Logger.debug("no input")
    else
      input =
        if input =~ ~r/^\(.*\)$/ do
          input
        else
          "(#{input})"
        end

      Logger.debug("Performing #{inspect(input)} for #{inspect(me)}")
      Player.perform(me, input)
    end
  end

  def intro() do
    "Command mode"
  end

  def prompt() do
    "> "
  end
end

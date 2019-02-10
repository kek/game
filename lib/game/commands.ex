defmodule Game.Commands do
  use Symbelix.Library

  def load() do
    :ok
  end

  def say() do
    "say something"
  end

  def say(word) do
    "you say #{inspect(word)}?"
  end
end

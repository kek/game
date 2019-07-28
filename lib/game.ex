defmodule Game do
  @moduledoc """
  Documentation for Game.
  """

  @doc """
  Hello world.

  ## Examples

      iex> Game.hello()
      :world

  """
  def hello do
    :world
  end

  def test_luerl_sandbox_timeout do
    st = :luerl_sandbox.init()

    say = fn [message], state ->
      Game.World.players()
      |> Enum.each(&Game.Player.notify(&1, {:saying, "test", message}))

      {[message], state}
    end

    st = :luerl.set_table([:say], say, st)

    {result, state} =
      :luerl_sandbox.run("x = 0\nwhile true do\nx = x + 1\nsay(x)\nend", st, 100_000)

    IO.puts("done")
    {result, state}
  end
end

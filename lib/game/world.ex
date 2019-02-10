defmodule Game.World do
  use GenServer
  require Logger

  def start_link([]) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    Logger.info("#{__MODULE__} started at #{inspect(self())}")
    {:ok, []}
  end
end

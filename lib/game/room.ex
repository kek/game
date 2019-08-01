defmodule Game.Room do
  use GenServer

  defstruct name: nil, x: nil, y: nil

  def start_link([], [name, x, y]) do
    GenServer.start_link(__MODULE__, [name, x, y])
  end

  @impl true
  def init([name, x, y]) do
    state = %__MODULE__{name: name, x: x, y: y}
    {:ok, state}
  end
end

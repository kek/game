defmodule Game.World do
  use GenServer
  require Logger
  alias Game.Player

  defstruct players: []

  def start_link([]) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    Logger.info("#{__MODULE__} started at #{inspect(self())}")
    {:ok, %__MODULE__{}}
  end

  def create_player() do
    GenServer.call(__MODULE__, {:create_player})
  end

  def handle_call({:create_player}, _from, state) do
    {:ok, player} = Player.start_link()
    state = %{state | players: [player | state.players]}
    {:reply, player, state}
  end
end

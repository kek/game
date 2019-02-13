defmodule Game.World do
  use GenServer
  require Logger
  alias Game.Player

  defstruct players: []

  def start_link([]) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__, debug: [:trace])
  end

  def init([]) do
    Logger.info("#{__MODULE__} started at #{inspect(self())}")
    {:ok, %__MODULE__{}}
  end

  def create_player(connection) do
    GenServer.call(__MODULE__, {:create_player, connection})
  end

  def players() do
    GenServer.call(__MODULE__, {:players})
  end

  def handle_call({:create_player, connection}, _from, state) do
    {:ok, player} = Player.start_link(connection)
    state = %{state | players: [player | state.players]}
    {:reply, player, state}
  end

  def handle_call({:players}, _from, state) do
    {:reply, state.players, state}
  end
end

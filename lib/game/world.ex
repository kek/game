defmodule Game.World do
  use GenServer
  require Logger
  alias Game.Player

  defstruct players: []

  @gen_server_options Application.get_env(:game, :gen_server_options) || []

  def start_link([]) do
    GenServer.start_link(__MODULE__, [], Keyword.merge(@gen_server_options, name: __MODULE__))
  end

  def init([]) do
    Logger.info("#{__MODULE__} started at #{inspect(self())}")
    {:ok, %__MODULE__{}}
  end

  def create_player(conversation) do
    GenServer.call(__MODULE__, {:create_player, conversation})
  end

  def players() do
    GenServer.call(__MODULE__, {:players})
  end

  def handle_call({:create_player, conversation}, _from, state) do
    {:ok, player} = Player.start_link(conversation)
    state = %{state | players: [player | state.players]}
    {:reply, player, state}
  end

  def handle_call({:players}, _from, state) do
    {:reply, state.players, state}
  end
end

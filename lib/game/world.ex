defmodule Game.World do
  use GenServer
  require Logger
  alias Game.{Player, Object}

  defstruct players: [], objects: %{}

  @gen_server_options Application.get_env(:game, :gen_server_options) || []

  def start_link([]) do
    GenServer.start_link(__MODULE__, [], Keyword.merge(@gen_server_options, name: __MODULE__))
  end

  def init([]) do
    Logger.info("#{__MODULE__} started at #{inspect(self())}")
    {:ok, %__MODULE__{}}
  end

  ### Public interface

  def create_player(conversation) do
    GenServer.call(__MODULE__, {:create_player, conversation})
  end

  def create_object(name, contents) do
    GenServer.call(__MODULE__, {:create_object, name, contents})
  end

  def players() do
    GenServer.call(__MODULE__, {:players})
  end

  def objects do
    GenServer.call(__MODULE__, {:objects})
  end

  def lookup_object(object_name) do
    GenServer.call(__MODULE__, {:lookup_object, object_name})
  end

  ### Callbacks

  def handle_call({:create_player, conversation}, _from, state) do
    {:ok, player} = Player.start_link(conversation)
    state = %{state | players: [player | state.players]}
    {:reply, player, state}
  end

  def handle_call({:create_object, name, contents}, {creator, _}, state) do
    Logger.debug("World creating object #{name}: #{inspect(contents)}")

    if Map.has_key?(state.objects, name) do
      state.objects
      |> Map.get(name)
      |> Object.update_code(contents)

      {:reply, :ok, state}
    else
      {:ok, object} = Object.start_link(name, contents, creator)
      {:reply, :ok, %{state | objects: Map.put(state.objects, name, object)}}
    end
  end

  def handle_call({:players}, _from, state) do
    players = Enum.filter(state.players, &Process.alive?/1)
    {:reply, players, %{state | players: players}}
  end

  def handle_call({:objects}, _from, state) do
    object_pids = Map.values(state.objects)
    {:reply, object_pids, state}
  end

  def handle_call({:lookup_object, object_name}, _from, state) do
    object = Map.get(state.objects, object_name)
    Logger.debug("Getting object with name #{object_name}: #{inspect(object)}")
    {:reply, object, state}
  end
end

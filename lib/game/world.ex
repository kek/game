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

    state = %__MODULE__{}
    state = create_default_objects(state)

    {:ok, state}
  end

  ### Public interface

  def create_player(conversation), do: GenServer.call(__MODULE__, {:create_player, conversation})

  def create_object(name, contents),
    do: GenServer.call(__MODULE__, {:create_object, name, contents})

  def players(), do: GenServer.call(__MODULE__, {:players})

  def objects, do: GenServer.call(__MODULE__, {:objects})

  def lookup_object(object_name), do: GenServer.call(__MODULE__, {:lookup_object, object_name})

  def delete_object(object_name), do: GenServer.call(__MODULE__, {:delete_object, object_name})

  def reload(), do: GenServer.call(__MODULE__, {:reload})

  def name(thing), do: GenServer.call(__MODULE__, {:name, thing})

  ### Callbacks

  def handle_call({:create_player, conversation}, _from, state) do
    {:ok, player} = Player.start_link(conversation)
    state = %{state | players: [player | state.players]}
    {:reply, player, state}
  end

  def handle_call({:create_object, name, contents}, {creator, _}, state) do
    state = do_create_object(state, name, contents, creator)
    {:reply, :ok, state}
  end

  def handle_call({:players}, _from, state) do
    players = Enum.filter(state.players, &Process.alive?/1)
    {:reply, players, %{state | players: players}}
  end

  def handle_call({:objects}, _from, state) do
    object_pids =
      state.objects
      |> Map.values()
      |> Enum.filter(&Process.alive?/1)
    {:reply, object_pids, state}
  end

  def handle_call({:lookup_object, object_name}, _from, state) do
    object = Map.get(state.objects, object_name)
    Logger.debug("Getting object with name #{object_name}: #{inspect(object)}")
    {:reply, object, state}
  end

  def handle_call({:delete_object, object_name}, {_, _}, state) do
    object = Map.get(state.objects, object_name)
    :ok = Object.stop(object)
    {:reply, :ok, %{state | objects: Map.delete(state.objects, object_name)}}
  end

  def handle_call({:reload}, _from, state) do
    state = state |> create_default_objects()
    {:reply, :ok, state}
  end

  def handle_call({:name, thing}, _from, state) do
    {:reply,
      cond do
        thing == self() -> "the world"
        thing in state.players -> Player.name(thing)
        thing in Map.values(state.objects) -> Object.name(thing)
        true -> "an unknown entity"
      end,
      state}
  end

  def handle_cast({:notify, message}, state) do
    Logger.debug("Notification: #{message}")
    {:noreply, state}
  end

  ### Helpers

  defp do_create_object(state, name, contents, creator \\ self())

  defp do_create_object(state, name, contents, creator) when is_binary(contents) do
    do_create_object(state, name, String.split(contents, "\n"), creator)
  end

  defp do_create_object(state, name, contents, creator) do
    if Map.has_key?(state.objects, name) do
      object = Map.get(state.objects, name)
      Object.update_code(object, contents)
      Logger.debug("World updated object #{name} (#{inspect(object)}): #{inspect(contents)}")
      state
    else
      {:ok, object} = Object.start_link(name, contents, creator)
      Logger.debug("World created object #{name} (#{inspect(object)}): #{inspect(contents)}")
      %{state | objects: Map.put(state.objects, name, object)}
    end
  end

  defp create_default_objects(state) do
    path = "priv/objects"
    ending = ~r/\.lua/

    state =
      File.ls!(path)
      |> Enum.filter(fn filename ->
        filename =~ ending
      end)
      |> Enum.map(fn filename ->
        object_name = Regex.replace(ending, filename, "")
        code = File.read!("#{path}/#{filename}")
        {object_name, code}
      end)
      |> Enum.reduce(state, fn {name, code}, state ->
        do_create_object(state, name, code)
      end)

    state
  end
end

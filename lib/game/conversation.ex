defmodule Game.Conversation do
  use GenServer
  alias Game.{Player, World, Mode.Normal}
  require Logger

  @gen_server_options Application.get_env(:game, :gen_server_options) || []

  defstruct me: nil, socket: nil, mode: Normal

  def init([socket]) do
    Logger.info("#{__MODULE__} started at #{inspect(self())}")
    player = World.create_player(self())
    Process.link(player)
    state = %__MODULE__{socket: socket, me: player}
    player_name = Player.name(player)
    Logger.info("#{inspect(state)} logged in: #{player_name}")
    do_output(socket, "You are now known as #{player_name}.")
    Player.prompt(player)
    {:ok, state}
  end

  def start(socket) do
    GenServer.start(__MODULE__, [socket], @gen_server_options)
  end

  def output(conversation, string, options \\ [newline: true]) do
    GenServer.cast(conversation, {:output, string, options})
  end

  def change_mode(conversation, mode) do
    GenServer.call(conversation, {:change_mode, mode})
  end

  def prompt(conversation) do
    GenServer.call(conversation, {:prompt})
  end

  def handle_call({:prompt}, _from, state) do
    {:reply, state.mode.prompt(), state}
  end

  def handle_call({:change_mode, mode}, _from, state) when is_atom(mode) do
    {:reply, :ok, %{state | mode: mode}}
  end

  def handle_call({:change_mode, mode}, _from, state) do
    try do
      mode = String.to_existing_atom(mode)
      do_output(state.socket, "Switching to mode #{inspect(mode)}")
      {:reply, :ok, %{state | mode: mode}}
    rescue
      error ->
        do_output(state.socket, "#{inspect(mode)} not found")
        {:reply, {:error, error}, state}
    end
  end

  def handle_cast({:output, string, options}, state) do
    do_output(state.socket, string, options)
    {:noreply, state}
  end

  defp do_output(socket, string, options \\ [newline: true]) do
    :ok = :gen_tcp.send(socket, String.to_charlist(string))

    if options[:newline] == true do
      :gen_tcp.send(socket, '\n')
    end
  end

  def handle_info({:tcp, socket, text}, state) do
    string = text |> List.to_string() |> String.trim()
    perform(state, socket, string)
    {:noreply, state}
  end

  def handle_info({:tcp_closed, socket}, state) do
    Logger.info("Closed #{inspect(socket)}")
    {:stop, :normal, state}
  end

  defp perform(state, socket, input) do
    Logger.info("got #{input} from #{inspect(socket)}")

    Logger.debug("Performing #{input} for #{inspect(state.me)}")
    state.mode.perform(state.me, String.trim(input))
    Player.prompt(state.me)
  end
end

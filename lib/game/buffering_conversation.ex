defmodule Game.BufferingConversation do
  use GenServer
  alias Game.{Player, World, Telnet}
  alias Game.Mode.Normal
  require Logger

  @gen_server_options Application.get_env(:game, :gen_server_options) || []

  @control_codes Telnet.control_codes()
  @commands Telnet.commands()
  @reverse_commands Telnet.reverse_commands()
  @options Telnet.options()
  @reverse_options Telnet.reverse_options()

  defstruct me: nil, socket: nil, mode: Normal, buffer: [], prompt: "> "

  def init([socket]) do
    Logger.info("#{__MODULE__} started at #{inspect(self())}")
    player = World.create_player(self())
    Process.link(player)
    state = %__MODULE__{socket: socket, me: player}
    player_name = Player.name(player)
    Logger.info("#{inspect(state)} logged in: #{player_name}")
    set_character_at_a_time_mode(state)
    :gen_tcp.send(state.socket, "You are now known as #{player_name}.\r\n")
    :gen_tcp.send(state.socket, state.prompt)
    {:ok, state}
  end

  def start(socket) do
    GenServer.start(__MODULE__, [socket], @gen_server_options)
  end

  def output(conversation, string, options \\ [newline: true]) do
    GenServer.call(conversation, {:output, string, options})
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
    state = %{state | mode: mode, prompt: mode.prompt()}
    do_output(state, mode.intro())
    {:reply, :ok, state}
  end

  def handle_call({:change_mode, mode}, _from, state) do
    try do
      mode = String.to_existing_atom(mode)
      state = %{state | mode: mode, prompt: mode.prompt()}
      do_output(state, "Switching to mode #{inspect(mode)}")
      {:reply, :ok, state}
    rescue
      error ->
        do_output(state, "#{inspect(mode)} not found")
        {:reply, {:error, error}, state}
    end
  end

  def handle_call({:output, string, options}, _from, state) do
    do_output(state, string, options)
    {:reply, :ok, state}
  end

  defp do_output(state, string, options \\ [newline: true]) do
    Logger.debug("Outputting #{string} with #{inspect(state)}")

    state.buffer
    |> Enum.each(fn _ ->
      Logger.debug("Backing up")
      :gen_tcp.send(state.socket, [8])
    end)

    :gen_tcp.send(state.socket, [8])
    :gen_tcp.send(state.socket, [8])

    :ok = :gen_tcp.send(state.socket, String.to_charlist(string))

    if options[:newline] == true do
      :gen_tcp.send(state.socket, '\r\n')
    end

    :gen_tcp.send(state.socket, state.prompt)
    :gen_tcp.send(state.socket, state.buffer)
  end

  defp set_character_at_a_time_mode(state) do
    telnet_command = [
      @commands["IAC"],
      @commands["WILL"],
      @options["Echo"],
      @commands["IAC"],
      @commands["DONT"],
      @options["Echo"],
      @commands["IAC"],
      @commands["WILL"],
      @options["Suppress Go Ahead"],
      @commands["IAC"],
      @commands["DO"],
      @options["Suppress Go Ahead"]
    ]

    :gen_tcp.send(state.socket, telnet_command)
  end

  def handle_info({:tcp, socket, input}, state) do
    buffer = perform(state, socket, input)
    Logger.debug("Buffer: #{inspect(buffer)}")
    {:noreply, %{state | buffer: buffer}}
  end

  def handle_info({:tcp_closed, socket}, state) do
    Logger.info("Closed #{inspect(socket)}")
    {:stop, :normal, state}
  end

  defp perform(state, socket, [255, operation, option] ++ input) do
    Logger.debug("got [IAC, #{@reverse_commands[operation]}, #{@reverse_options[option]}]")
    state.buffer ++ perform(state, socket, input)
  end

  defp perform(state, _socket, [13, 0]) do
    Logger.debug("Got CR")
    :ok = :gen_tcp.send(state.socket, [@control_codes["CR"], @control_codes["LF"]])
    state.mode.perform(state.me, List.to_string(state.buffer))
    :gen_tcp.send(state.socket, state.mode.prompt())
    []
  end

  defp perform(state, socket, [input | rest]) do
    :gen_tcp.send(socket, [input])
    Logger.debug("Got #{[input]} (#{input}) from #{inspect(state.me)}")
    state.buffer ++ [input] ++ perform(state, socket, rest)
  end

  defp perform(_state, _socket, []) do
    []
  end
end

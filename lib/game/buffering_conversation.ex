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
    Process.flag(:trap_exit, true)
    state = %__MODULE__{socket: socket, me: player}
    player_name = Player.name(player)
    Logger.info("#{inspect(state)} logged in: #{player_name}")
    set_character_at_a_time_mode(state)

    :gen_tcp.send(
      state.socket,
      "You are now known as #{player_name}.\r\nType 'help' for help.\r\n"
    )

    :gen_tcp.send(state.socket, state.prompt)
    {:ok, state}
  end

  ### Public interface

  def start_link([], [socket]) do
    GenServer.start_link(__MODULE__, [socket], @gen_server_options)
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

  ### Callbacks

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

  def handle_cast({:output, string, options}, state) do
    do_output(state, string, options)
    {:noreply, state}
  end

  def handle_info({:tcp, socket, input}, state) do
    buffer = process_input(state.buffer, state, socket, input)
    {:noreply, %{state | buffer: buffer}}
  end

  def handle_info({:tcp_closed, socket}, state) do
    Logger.info("Closed #{inspect(socket)}")
    Player.log_off(state.me)
    {:stop, :normal, state}
  end

  def handle_info({:EXIT, pid, {:timeout, details}}, state) do
    Logger.debug("Timeout in #{inspect(pid)}: #{inspect(details)}")
    do_output(state, "Timeout in #{inspect(pid)}: #{inspect(details)}", prompt: false)
    {:noreply, state}
  end

  def handle_info({:EXIT, pid, :bye}, state) do
    Logger.debug("Player #{inspect(pid)} logged off. Conversation #{inspect(self())} terminating")
    do_output(state, "Bye", prompt: false)
    {:stop, :normal, state}
  end

  def handle_info({:EXIT, pid, :killed}, state) do
    Logger.debug("Player #{inspect(pid)} was killed. Conversation #{inspect(self())} terminating")
    do_output(state, "Bye!", prompt: false)
    {:stop, :normal, state}
  end

  def handle_info({:EXIT, pid, reason}, state) do
    Logger.error(
      "#{__MODULE__} #{inspect(self())} got unexpected exit message about #{inspect(pid)}: #{
        inspect(reason)
      }"
    )

    {:noreply, state}
  end

  ### Private helpers

  defp do_output(state, string, options \\ [])

  defp do_output(state, string, options) when is_binary(string) do
    default_options = [newline: true, prompt: true]
    options = Keyword.merge(default_options, options)

    state.buffer
    |> Enum.each(fn _ ->
      :gen_tcp.send(state.socket, [8])
    end)

    :gen_tcp.send(state.socket, [8])
    :gen_tcp.send(state.socket, [8])
    :gen_tcp.send(state.socket, '  ')
    :gen_tcp.send(state.socket, [8])
    :gen_tcp.send(state.socket, [8])

    case :gen_tcp.send(state.socket, String.to_charlist(string)) do
      :ok ->
        true

      {:error, reason} ->
        Logger.error(
          "Error in do_output: #{inspect(reason)}, socket #{inspect(state.socket)}, output: #{
            inspect(string)
          }"
        )
    end

    if options[:newline] == true do
      :gen_tcp.send(state.socket, '\r\n')
    end

    if options[:prompt] do
      :gen_tcp.send(state.socket, state.prompt)
      :gen_tcp.send(state.socket, state.buffer)
    end
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

  defp process_input(buffer, state, socket, [255, operation, option] ++ input) do
    Logger.debug("got [IAC, #{@reverse_commands[operation]}, #{@reverse_options[option]}]")
    buffer ++ process_input(buffer, state, socket, input)
  end

  defp process_input(buffer, state, _socket, [13, 0]) do
    Logger.debug("Got CR")
    :ok = :gen_tcp.send(state.socket, [@control_codes["CR"], @control_codes["LF"]])

    result = state.mode.perform(state.me, List.to_string(buffer))
    Logger.debug("Result: #{inspect(result)}")

    if result == :quit do
      :quit
    else
      :gen_tcp.send(state.socket, state.mode.prompt())
      []
    end
  end

  defp process_input(buffer, state, socket, [27, ?[, c1, c2, ?~ | rest]) do
    Logger.debug("Ignoring escape sequence <ESC>[#{[c1, c2]}~")
    process_input(buffer, state, socket, rest)
  end

  defp process_input(buffer, state, socket, [27, ?[, c, ?~ | rest]) do
    Logger.debug("Ignoring escape sequence <ESC>[#{[c]}~")
    process_input(buffer, state, socket, rest)
  end

  defp process_input(buffer, state, socket, [27, ?[, c | rest]) do
    Logger.debug("Ignoring escape sequence <ESC>[#{[c]}")
    process_input(buffer, state, socket, rest)
  end

  defp process_input(buffer, state, socket, [27, ?O, c | rest]) do
    Logger.debug("Ignoring function key <ESC>O#{[c]}")
    process_input(buffer, state, socket, rest)
  end

  defp process_input(buffer, state, socket, [127 | rest]) do
    Logger.debug("Got DEL from #{inspect(state.me)}. Rest: #{inspect(rest)}. Buffer: #{buffer}")

    if buffer != [] do
      :gen_tcp.send(socket, [8, ?\s, 8])

      buffer
      |> Enum.reverse()
      |> Enum.drop(1)
      |> Enum.reverse()
    else
      buffer
    end ++ process_input([], state, socket, rest)
  end

  defp process_input(buffer, state, socket, [input | rest]) when input >= ?\s and input <= ?~ do
    :gen_tcp.send(socket, [input])
    buffer ++ [input] ++ process_input([], state, socket, rest)
  end

  defp process_input(buffer, _state, _socket, []) do
    buffer
  end

  defp process_input(buffer, state, socket, [input | rest]) do
    Logger.debug("Ignoring #{inspect(input)}")
    buffer ++ process_input([], state, socket, rest)
  end
end

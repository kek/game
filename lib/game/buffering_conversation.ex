defmodule Game.BufferingConversation do
  use GenServer
  alias Game.{Player, World}
  alias Game.Mode.Normal
  require Logger

  @gen_server_options Application.get_env(:game, :gen_server_options) || []

  @control_codes %{
    # NULL. No operation
    "NUL" => 0,
    # Line Feed. Moves the printer to the next print line, keeping the same horizontal position.
    "LF" => 10,
    # Carriage Return. Moves the printer to the left margin of the current line.
    "CR" => 13,
    # BELL. Produces an audible or visible signal (which does NOT move the print head.
    "BEL" => 7,
    # Back Space. Moves the print head one character position towards the left margin. [On a printing devices this mechanism was commonly used to form composite characters by printing two basic characters on top of each other.]
    "BS" => 8,
    # Horizontal Tab. Moves the printer to the next horizontal tab stop. It remains unspecified how either party determines or establishes where such tab stops are located.
    "HT" => 9,
    # Vertical Tab. Moves the printer to the next vertical tab stop. It remains unspecified how either party determines or establishes where such tab stops are located.
    "VT" => 11,
    # Form Feed. Moves the printer to the top of the next page, keeping the same horizontal position. [On visual displays this commonly clears the screen and moves the cursor to the top left corner.]
    "FF" => 12
  }

  @commands %{
    # End of subnegotiation parameters.
    "SE" => 240,
    # No operation
    "NOP" => 241,
    # Data mark. Indicates the position of a Synch event within the data stream. This should always be accompanied by a TCP urgent notification.
    "DM" => 242,
    # Break. Indicates that the "break" or "attention" key was hit.
    "BRK" => 243,
    # Suspend, interrupt or abort the process to which the NVT is connected.
    "IP" => 244,
    # Abort output. Allows the current process to run to completion but do not send its output to the user.
    "AO" => 245,
    # Are you there. Send back to the NVT some visible evidence that the AYT was received.
    "AYT" => 246,
    # Erase character. The receiver should delete the last preceding undeleted character from the data stream.
    "EC" => 247,
    # Erase line. Delete characters from the data stream back to but not including the previous CRLF.
    "EL" => 248,
    # Go ahead. Used, under certain circumstances, to tell the other end that it can transmit.
    "GA" => 249,
    # Subnegotiation of the indicated option follows.
    "SB" => 250,
    # Indicates the desire to begin performing, or confirmation that you are now performing, the indicated option.
    "WILL" => 251,
    # Indicates the refusal to perform, or continue performing, the indicated option.
    "WONT" => 252,
    # Indicates the request that the other party perform, or confirmation that you are expecting the other party to perform, the indicated option.
    "DO" => 253,
    # Indicates the demand that the other party stop performing, or confirmation that you are no longer expecting the other party to perform, the indicated option.
    "DONT" => 254,
    # Interpret as command
    "IAC" => 255
  }

  @reverse_commands @commands
                    |> Enum.map(fn {key, value} -> {value, key} end)
                    |> Map.new()

  @options %{
    "Transmit Binary" => 0,
    "Echo" => 1,
    "Reconnection" => 2,
    "Suppress Go Ahead" => 3,
    "Approx Message Size Negotiation." => 4,
    "Status" => 5,
    "Timing Mark" => 6,
    "Remote Controlled Trans and Echo" => 7,
    "Output Line Width" => 8,
    "Output Page Size" => 9,
    "Negotiate About Output Carriage-Return Disposition" => 10,
    "Negotiate About Output Horizontal Tabstops" => 11,
    "NAOHTD, Negotiate About Output Horizontal Tab Disposition" => 12,
    "Negotiate About Output Formfeed Disposition" => 13,
    "Negotiate About Vertical Tabstops" => 14,
    "Negotiate About Output Vertcial Tab Disposition" => 15,
    "Negotiate About Output Linefeed Disposition" => 16,
    "Extended ASCII." => 17,
    "Logout." => 18,
    "Byte Macro" => 19,
    "Data Entry Terminal" => 20,
    "SUPDUP" => 21,
    "SUPDUP Output" => 22,
    "Send Location" => 23,
    "Terminal Type" => 24,
    "End of Record" => 25,
    "TACACS User Identification" => 26,
    "Output Marking" => 27,
    "TTYLOC, Terminal Location Number." => 28,
    "Telnet 3270 Regime" => 29,
    "X.3 PAD." => 30,
    "NAWS, Negotiate About Window Size." => 31,
    "Terminal Speed" => 32,
    "Remote Flow Control" => 33,
    "Linemode" => 34,
    "X Display Location." => 35,
    "Environment" => 36,
    "Authentication" => 37,
    "Encryption Option" => 38,
    "New Environment" => 39,
    "TN3270E" => 40,
    "XAUTH" => 41,
    "CHARSET" => 42,
    "RSP, Telnet Remote Serial Port" => 43,
    "Com Port Control" => 44,
    "Telnet Suppress Local Echo" => 45,
    "Telnet Start TLS" => 46,
    "KERMIT" => 47,
    "SEND-URL" => 48,
    "FORWARD_X" => 49,
    "TELOPT PRAGMA LOGON" => 138,
    "TELOPT SSPI LOGON" => 139,
    "TELOPT PRAGMA HEARTBEAT" => 140,
    "Extended-Options-List" => 255
  }

  @reverse_options @options
                   |> Enum.map(fn {key, value} -> {value, key} end)
                   |> Map.new()

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

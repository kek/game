defmodule Game.Conversation do
  use GenServer
  alias Game.{Commands, Player}
  require Logger

  defstruct me: nil, socket: nil

  def init([socket]) do
    Logger.info("#{__MODULE__} started at #{inspect(self())}")
    {:ok, player} = Player.start_link()
    state = %__MODULE__{socket: socket, me: player}
    player_name = Player.name(player)
    Logger.info("#{inspect(state)} logged in: #{player_name}")
    output(socket, "You are now known as #{player_name}.")
    {:ok, state}
  end

  def start(socket) do
    GenServer.start(__MODULE__, [socket])
  end

  def output(socket, string, options \\ [newline: true]) do
    :gen_tcp.send(socket, String.to_charlist(string))

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

  defp perform(state, session, message) do
    Logger.info("got #{message} from #{inspect(session)}")

    program =
      if message =~ ~r/^\(.*\)$/ do
        message
      else
        "(#{message})"
      end

    if message =~ ~r/^\W*$/ do
      output(session, "no input")
    else
      # ensure Commands is loaded for Symbelix.run
      Commands.load()

      result = Symbelix.run(program, Commands)

      output(session, "#{program} -> #{inspect(result)}")
    end
  end
end

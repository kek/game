defmodule Game.Conversation do
  use GenServer
  alias Game.Commands
  require Logger

  def init(args) do
    Logger.info("#{__MODULE__} started at #{inspect(self())}")
    {:ok, args}
  end

  def start do
    {:ok, pid} = GenServer.start(__MODULE__, [])
    pid
  end

  def output(port, string, options \\ [newline: true]) do
    :gen_tcp.send(port, String.to_charlist(string))

    if options[:newline] == true do
      :gen_tcp.send(port, '\n')
    end
  end

  def handle_info({:tcp, port, text}, state) do
    string = text |> List.to_string() |> String.trim()
    perform(port, string)
    {:noreply, state}
  end

  def handle_info({:tcp_closed, port}, state) do
    Logger.info("Closed #{inspect(port)}")
    {:stop, :normal, state}
  end

  defp perform(session, message) do
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

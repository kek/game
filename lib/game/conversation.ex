defmodule Game.Conversation do
  use GenServer
  alias Game.Session

  def init(args) do
    {:ok, args}
  end

  def start_link do
    {:ok, pid} = GenServer.start_link(__MODULE__, [])
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
    Session.handle(port, string)
    {:noreply, state}
  end

  def handle_info({:tcp_closed, port}, state) do
    IO.puts("Closed #{inspect(port)}")
    {:stop, :normal, state}
  end
end

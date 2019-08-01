defmodule Game.Server do
  alias Game.ConversationSupervisor
  require Logger
  use GenServer

  @gen_server_options Application.get_env(:game, :gen_server_options) || []

  defmodule State do
    defstruct nothing: nil
  end

  def init([port], suppress_waiting_message \\ false) do
    case :gen_tcp.listen(port, []) do
      {:ok, listening_socket} ->
        Logger.info("listening at socket #{inspect(listening_socket)}")
        {:ok, %State{}, {:continue, listening_socket}}

      {:error, :eaddrinuse} ->
        unless suppress_waiting_message do
          Logger.info("Port #{port} in use, waiting...")
        end

        Process.sleep(1000)
        init([port], true)
    end
  end

  def handle_continue(listening_socket, state = %State{}) do
    {:ok, socket} = :gen_tcp.accept(listening_socket)
    Logger.info("connected #{inspect(socket)}")
    {:ok, _conversation} = ConversationSupervisor.start_child(socket)

    {:noreply, state, {:continue, listening_socket}}
  end

  def start_link(port) do
    GenServer.start_link(__MODULE__, port, Keyword.merge(@gen_server_options, name: __MODULE__))
  end
end

defmodule Game.Server do
  alias Game.Conversation
  require Logger

  use GenServer

  defmodule State do
    defstruct nothing: nil
  end

  def init(port) do
    Logger.info("#{__MODULE__} started at #{inspect(self())}")

    case :gen_tcp.listen(port, []) do
      {:ok, listening_socket} ->
        Logger.info("listening at socket #{inspect(listening_socket)}")
        {:ok, %State{}, {:continue, listening_socket}}

      {:error, :eaddrinuse} ->
        Logger.info("Port #{port} in use, waiting...")
        Process.sleep(5000)
        init(port)
    end
  end

  def handle_continue(listening_socket, state = %State{}) do
    {:ok, socket} = :gen_tcp.accept(listening_socket)
    Logger.info("connected #{inspect(socket)}")
    {:ok, conversation} = Conversation.start(socket)
    :ok = :gen_tcp.controlling_process(socket, conversation)

    {:noreply, state, {:continue, listening_socket}}
  end

  def start_link(port) do
    GenServer.start_link(__MODULE__, port, name: __MODULE__, debug: [:trace])
  end
end

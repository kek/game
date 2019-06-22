defmodule Game.Player do
  use GenServer
  require Logger
  alias Game.{Conversation, Player, Commands}

  defstruct name: nil, socket: nil

  def start_link(socket) do
    GenServer.start_link(__MODULE__, [socket], debug: [:trace])
  end

  def init([socket]) do
    name = Faker.Name.name()
    Logger.info("Started #{inspect(self())}: #{name}")
    {:ok, %__MODULE__{socket: socket, name: name}}
  end

  def name(pid) do
    if pid == self() do
      "you"
    else
      GenServer.call(pid, {:name})
    end
  end

  def notify(pid, {:saying, from, saying}) do
    GenServer.cast(pid, {:notify, {:saying, from, saying}})
  end

  def perform(pid, program) do
    GenServer.call(pid, {:perform, program})
  end

  def handle_call({:perform, program}, _, state) do
    # ensure Commands is loaded for Symbelix.run
    message = run(program)

    Conversation.output(state.socket, "#{program} -> #{message}")

    {:reply, :ok, state}
  end

  def handle_call({:name}, _from, state) do
    {:reply, state.name, state}
  end

  def handle_cast({:notify, {:saying, from, saying}}, state) do
    player_name = Player.name(from)

    Conversation.output(state.socket, "#{player_name}: #{inspect(saying)}")
    {:noreply, state}
  end

  defp run(program) do
    Logger.info("Running #{inspect(program)}")

    Commands.load()

    try do
      case Symbelix.run(program, Commands) do
        {:error, message} ->
          "Error: #{inspect(program)} -> #{inspect(message)}"

        result ->
          "#{inspect(program)} -> #{inspect(result)}"
      end
    rescue
      error in MatchError ->
        %MatchError{term: {:error, message}} = error
        "Error: #{inspect(program)} -> #{inspect(message)}"

      error ->
        "Unexpected error: #{inspect(program)} -> #{inspect(error)}"
    end
  end
end

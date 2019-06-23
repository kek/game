defmodule Game.Player do
  use GenServer
  require Logger
  alias Game.{Conversation, Player, Commands}

  defstruct name: nil, conversation: nil

  def start_link(conversation) do
    GenServer.start_link(__MODULE__, [conversation], debug: [:trace])
  end

  def init([conversation]) do
    name = Faker.Name.name()
    Logger.info("Started #{inspect(self())}: #{name}")
    {:ok, %__MODULE__{conversation: conversation, name: name}}
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

  def perform(player, program) do
    GenServer.call(player, {:perform, program})
  end

  def handle_call({:perform, program}, _, state) do
    # ensure Commands is loaded for Symbelix.run
    message = run(program)

    Conversation.output(state.conversation, "#{program} -> #{message}")

    {:reply, :ok, state}
  end

  def handle_call({:name}, _from, state) do
    {:reply, state.name, state}
  end

  def handle_cast({:notify, {:saying, from, saying}}, state) do
    player_name = Player.name(from)

    Conversation.output(state.conversation, "#{player_name}: #{inspect(saying)}")
    {:noreply, state}
  end

  defp run(program) do
    Logger.info("Running #{inspect(program)}")

    Commands.load()

    try do
      case Symbelix.run(program, Commands) do
        {:error, message} ->
          "Error: #{message}"

        result when is_list(result) ->
          Enum.join(result, ", ")

        result ->
          result
      end
    rescue
      error in MatchError ->
        %MatchError{term: {:error, message}} = error
        "Match error: #{message}"

      error ->
        "Unexpected error: #{inspect(error)}"
    end
  end
end

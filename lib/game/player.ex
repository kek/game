defmodule Game.Player do
  use GenServer
  require Logger
  alias Game.{Conversation, Player, Commands}

  defstruct name: nil, socket: nil

  def start_link(socket) do
    GenServer.start_link(__MODULE__, [socket])
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
      Logger.info("CALL: #{inspect(self())} Player name #{inspect(pid)} - player.ex name")
      GenServer.call(pid, {:name})
    end
  end

  def notify(pid, {:saying, from, saying}) do
    if pid == self() do
      Logger.info("#{inspect(self())} tried to Player.notify itself \"#{saying}\"!")
    else
      GenServer.cast(pid, {:notify, {:saying, from, saying}})
    end
  end

  def perform(pid, program) do
    Logger.info(
      "CALL: #{inspect(self)} Player perform to #{inspect(pid)} #{program} - player.ex perform"
    )

    GenServer.call(pid, {:perform, program})
  end

  def handle_call({:perform, program}, _, state) do
    # ensure Commands is loaded for Symbelix.run
    Commands.load()

    result = Symbelix.run(program, Commands)

    Conversation.output(state.socket, "#{program} -> #{inspect(result)}")

    {:reply, :ok, state}
  end

  def handle_call({:name}, _from, state) do
    {:reply, state.name, state}
  end

  def handle_cast({:notify, {:saying, from, saying}}, state) do
    player_name = Player.name(from)

    Conversation.output(state.socket, "#{player_name}: #{saying}")
    {:noreply, state}
  end
end

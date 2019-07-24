defmodule Game.Player do
  use GenServer
  require Logger
  alias Game.{Player, Commands, Mode, World}

  defstruct name: nil, conversation: nil, edited_object: nil

  @gen_server_options Application.get_env(:game, :gen_server_options) || []
  @conversation Application.get_env(:game, :conversation)

  def start_link(conversation) do
    GenServer.start_link(__MODULE__, [conversation], @gen_server_options)
  end

  def init([conversation]) do
    name = Faker.Name.name()
    Logger.info("Started #{inspect(self())}: #{name}")
    {:ok, %__MODULE__{conversation: conversation, name: name}}
  end

  def terminate(reason, state) do
    Logger.debug("Terminating #{inspect(reason)} #{inspect(self())} state #{inspect(state)}")
    Process.exit(state.conversation, :bye)
  end

  def name(pid) do
    if pid == self() do
      "you"
    else
      GenServer.call(pid, {:name})
    end
  end

  ### Public interface

  def notify(player, message) do
    GenServer.cast(player, {:notify, message})
  end

  def perform(player, program) do
    GenServer.cast(player, {:perform, program})
  end

  def edit(player, name) do
    GenServer.cast(player, {:edit, name})
  end

  def done_editing(player, lines) do
    GenServer.cast(player, {:done_editing, lines})
  end

  def log_off(player) do
    GenServer.cast(player, {:log_off})
  end

  def write(player, object_name, lines) do
    GenServer.cast(player, {:write, object_name, lines})
  end

  ### Callbacks

  def handle_call({:name}, _from, state) do
    {:reply, state.name, state}
  end

  def handle_cast({:log_off}, state) do
    Logger.debug("Logging off #{inspect(self())}")
    {:stop, :normal, state}
  end

  def handle_cast({:edit, name}, state) do
    @conversation.change_mode(state.conversation, Mode.Editor)
    Logger.debug("Starting editing #{name}")
    {:noreply, %{state | edited_object: name}}
  end

  def handle_cast({:done_editing, lines}, state) do
    @conversation.change_mode(state.conversation, Mode.Normal)
    Logger.debug("Finalize editing. State: #{inspect(state)}. Lines: #{inspect(lines)}")
    Player.write(self(), state.edited_object, lines)
    {:noreply, %{state | edited_object: nil}}
  end

  def handle_cast({:write, object_name, lines}, state) do
    Logger.debug("#{inspect(self())} writing #{inspect(lines)} to #{object_name}")
    World.create_object(object_name, lines)
    {:noreply, state}
  end

  def handle_cast({:perform, program}, state) do
    Logger.debug("Performing #{inspect(program)} with state #{inspect(state)}")
    message = run_code(program)

    if message == :quit do
      Logger.debug("Quitting because run_code returned :quit #{inspect(self())}")
      {:stop, :normal, state}
    else
      @conversation.output(state.conversation, "#{program} -> #{inspect(message)}")
      {:noreply, state}
    end
  end

  def handle_cast({:notify, {:saying, from, saying}}, state) when is_pid(from) do
    player_name = Player.name(from)
    handle_cast({:notify, {:saying, player_name, saying}}, state)
  end

  def handle_cast({:notify, {:saying, player_name, saying}}, state) do
    @conversation.output(state.conversation, "#{player_name}: #{inspect(saying)}")
    {:noreply, state}
  end

  def handle_cast({:notify, text}, state) do
    @conversation.output(state.conversation, text)
    {:noreply, state}
  end

  ### Private helpers

  defp run_code(program) do
    Logger.info("Running #{inspect(program)}")

    Commands.load()

    try do
      case Symbelix.run(program, Commands) do
        {:error, message} ->
          "Error: #{message}"

        result when is_list(result) ->
          Logger.debug("Result in Player.run_code/1: #{inspect(result)}")
          Enum.join(result, ", ")

        result ->
          Logger.debug("Result in Player.run_code/1: #{inspect(result)}")
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

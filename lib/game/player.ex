defmodule Game.Player do
  use GenServer
  require Logger
  alias Game.{Player, Commands}

  defstruct name: nil, conversation: nil

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

  def name(pid) do
    if pid == self() do
      "you"
    else
      GenServer.call(pid, {:name})
    end
  end

  ### Public interface

  def notify(pid, {:saying, from, saying}) do
    GenServer.cast(pid, {:notify, {:saying, from, saying}})
  end

  # def notify(pid, text) do
  #   GenServer.cast(pid, {:notify, text})
  # end

  def perform(player, program) do
    GenServer.cast(player, {:perform, program})
  end

  def change_mode(player, mode) do
    GenServer.cast(player, {:change_mode, mode})
  end

  # def prompt(player) do
  #   GenServer.cast(player, {:prompt})
  # end

  def log_off(player) do
    Process.exit(player, :normal)
  end

  ### Callbacks

  def handle_call({:name}, _from, state) do
    {:reply, state.name, state}
  end

  def handle_cast({:change_mode, mode}, state) do
    @conversation.change_mode(state.conversation, mode)
    {:noreply, state}
  end

  def handle_cast({:perform, program}, state) do
    Logger.debug("Performing #{inspect(program)} with state #{inspect(state)}")
    message = run_code(program)
    @conversation.output(state.conversation, "#{program} -> #{inspect(message)}")
    {:noreply, state}
  end

  # def handle_cast({:prompt}, state) do
  #   prompt = @conversation.prompt(state.conversation)
  #   @conversation.output(state.conversation, prompt, newline: false)
  #   {:noreply, state}
  # end

  def handle_cast({:notify, {:saying, from, saying}}, state) do
    player_name = Player.name(from)
    @conversation.output(state.conversation, "#{player_name}: #{inspect(saying)}")
    {:noreply, state}
  end

  # def handle_cast({:notify, ""}, state) do
  #   {:noreply, state}
  # end

  # def handle_cast({:notify, text}, state) do
  #   Logger.debug("*** Notifying player of #{text}")
  #   @conversation.output(state.conversation, text)
  #   {:noreply, state}
  # end

  ### Private helpers

  defp run_code(program) do
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

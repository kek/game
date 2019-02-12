defmodule Game.Player do
  use GenServer
  require Logger

  defstruct name: nil

  def start_link do
    GenServer.start_link(__MODULE__, [])
  end

  def init([]) do
    name = Faker.Name.name()
    Logger.info("Started #{inspect(self())}: #{name}")
    {:ok, %__MODULE__{name: name}}
  end

  def name(pid) do
    GenServer.call(pid, {:name})
  end

  def handle_call({:name}, _from, state) do
    {:reply, state.name, state}
  end
end

defmodule Game.Object do
  use GenServer

  defstruct name: nil, code: [], creator: nil, st: nil
  @gen_server_options Application.get_env(:game, :gen_server_options) || []

  def start_link(name, code, creator) do
    GenServer.start_link(__MODULE__, [name, code, creator], @gen_server_options)
  end

  def init([name, code, creator]) do
    {:ok, %__MODULE__{name: name, code: code, creator: creator, st: :luerl_sandbox.init()}}
  end

  ### Public interface

  def name(object) do
    GenServer.call(object, {:name})
  end

  def get(object) do
    GenServer.call(object, {:get})
  end

  def run(object) do
    GenServer.call(object, {:run})
  end

  ### Callbacks

  def handle_call({:name}, _from, state) do
    {:reply, state.name, state}
  end

  def handle_call({:get}, _from, state) do
    {:reply, state, state}
  end

  def handle_call({:run}, _from, state) do
    code = Enum.join(state.code, "\n")
    {result, st} = :luerl_sandbox.run(code, state.st)
    {:reply, result, %{state | st: st}}
  end
end

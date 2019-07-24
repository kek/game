defmodule Game.Object do
  use GenServer
  require Logger
  alias Game.{World, Player}

  defstruct name: nil, code: [], creator: nil, lua: nil
  @gen_server_options Application.get_env(:game, :gen_server_options) || []

  def start_link(name, code, creator) do
    GenServer.start_link(__MODULE__, [name, code, creator], @gen_server_options)
  end

  def init([name, code, creator]) do
    lua = :luerl_sandbox.init()

    say = fn [message], state ->
      World.players()
      |> Enum.each(&Player.notify(&1, {:saying, name, message}))

      Logger.debug("say(#{inspect(message)}) from Lua!")
      {["yo"], state}
    end

    lua = :luerl.set_table([:say], say, lua)

    {:ok, %__MODULE__{name: name, code: code, creator: creator, lua: lua}}
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

  def update_code(object, code) do
    GenServer.call(object, {:update_code, code})
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
    {result, lua} = :luerl_sandbox.run(code, state.lua)
    {:reply, result, %{state | lua: lua}}
  end

  def handle_call({:update_code, code}, _from, state) do
    {:reply, :ok, %{state | code: code}}
  end
end

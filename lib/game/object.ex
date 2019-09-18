defmodule Game.Object do
  use GenServer
  require Logger
  alias Game.{World, Player}

  defstruct name: nil, code: [], creator: nil, lua: nil, food: 0
  @gen_server_options Application.get_env(:game, :gen_server_options) || []
  @max_reductions 20000

  def start_link(name, code, creator) do
    GenServer.start_link(__MODULE__, [name, code, creator], @gen_server_options)
  end

  def init([name, code, creator]) do
    lua = :luerl_sandbox.init()

    say = fn [message], lua_state ->
      World.players()
      |> Enum.each(&Player.notify(&1, {:saying, name, message}))

      Logger.debug("say(#{inspect(message)}) from Lua!")
      {[message], lua_state}
    end

    sleep = fn [time], lua_state ->
      time = trunc(time)
      Process.sleep(time)
      {["ok"], lua_state}
    end

    crash = fn _, lua_state ->
      _ = 1 / 0
      {["ok"], lua_state}
    end

    build = fn [_program], lua_state ->
      {["ok"], lua_state}
    end

    eat = fn [target_name], lua_state ->
      case World.lookup_object(target_name) do
        nil ->
          say.(["I can't find #{target_name}"], lua_state)

        target ->
          {_, _} = say.(["I eat #{target_name}."], lua_state)
          stop(target)
      end

      {["ok"], lua_state}
    end

    lua = :luerl.set_table([:say], say, lua)
    lua = :luerl.set_table([:sleep], sleep, lua)
    lua = :luerl.set_table([:crash], crash, lua)
    lua = :luerl.set_table([:build], build, lua)
    lua = :luerl.set_table([:eat], eat, lua)

    {:ok, %__MODULE__{name: name, code: code, creator: creator, lua: lua}}
  end

  ### Public interface
  def name(object), do: GenServer.call(object, {:name})
  def get_state(object), do: GenServer.call(object, {:get_state})

  def run(nil) do
    Player.notify(self(), "That doesn't exist.")
    :ok
  end

  def run(object), do: GenServer.call(object, {:run})

  def bg(nil) do
    Player.notify(self(), "That doesn't exist.")
    :ok
  end

  def bg(object), do: GenServer.cast(object, {:bg})

  def update_code(object, code), do: GenServer.call(object, {:update_code, code})

  def stop(object), do: GenServer.call(object, {:stop})

  ### Callbacks

  def handle_call({:name}, _from, state) do
    {:reply, state.name, state}
  end

  def handle_call({:get_state}, _from, state) do
    {:reply, state, state}
  end

  def handle_call({:run}, {caller, _}, state) do
    code = Enum.join(state.code, "\n")

    {result, lua} =
      case :luerl_sandbox.run(code, state.lua, @max_reductions) do
        {:error, reason} ->
          Player.notify(state.creator, "Error in #{state.name}: #{inspect(reason)}")
          Player.notify(caller, "Error in #{state.name}: #{inspect(reason)}")
          {reason, state.lua}

        {result, lua} ->
          Player.notify(caller, "#{state.name} -> #{inspect(result)}")
          {result, lua}
      end

    {:reply, result, %{state | lua: lua}}
  end

  def handle_call({:update_code, code}, _from, state) do
    {:reply, :ok, %{state | code: code}}
  end

  def handle_call({:stop}, _from, state) do
    World.players()
    |> Enum.each(&Player.notify(&1, {:saying, state.name, "Stops now"}))
    Logger.debug("Stopping #{inspect(self())}")
    {:stop, :normal, :ok, state}
  end

  def handle_cast({:bg}, state) do
    code = Enum.join(state.code, "\n")

    lua =
      case :luerl_sandbox.run(code, state.lua, @max_reductions) do
        {:error, reason} ->
          Player.notify(state.creator, "Error in #{state.name}: #{inspect(reason)}")
          Logger.debug("Error running code:\n#{code}\n#{inspect(reason)}")
          state.lua

        {result, lua} ->
          Logger.debug("OK running code:\n#{code}\nResult:\n#{inspect(result)}")
          lua
      end

    {:noreply, %{state | lua: lua}}
  end

  def handle_info({pid, luerl}, state) do
    Logger.warn(
      "#{__MODULE__} #{inspect(self())} got unexpected message about #{inspect(pid)}: #{
        inspect(luerl)
      }"
    )

    # Logger.warn("#{inspect(self())} got unexpected message about #{inspect(pid)}")

    # Logger.warn(is_pid(pid))

    {:noreply, state}
  end
end

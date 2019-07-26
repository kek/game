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
      {[message], state}
    end

    sleep = fn [time], state ->
      time = trunc(time)
      Process.sleep(time)
      {["ok"], state}
    end

    crash = fn _, state ->
      raise RuntimeError
      {["ok"], state}
    end

    lua = :luerl.set_table([:say], say, lua)
    lua = :luerl.set_table([:sleep], sleep, lua)
    lua = :luerl.set_table([:crash], crash, lua)

    {:ok, %__MODULE__{name: name, code: code, creator: creator, lua: lua}}
  end

  ### Public interface

  def name(object), do: GenServer.call(object, {:name})

  def get(object), do: GenServer.call(object, {:get})

  def run(nil), do: Player.notify(self(), "That doesn't exist.")
  def run(object), do: GenServer.call(object, {:run})

  def bg(nil), do: Player.notify(self(), "That doesn't exist.")
  def bg(object), do: GenServer.cast(object, {:bg})

  def update_code(object, code), do: GenServer.call(object, {:update_code, code})

  def stop(object), do: GenServer.call(object, {:stop})

  ### Callbacks

  def handle_call({:name}, _from, state) do
    {:reply, state.name, state}
  end

  def handle_call({:get}, _from, state) do
    {:reply, state, state}
  end

  def handle_call({:run}, {caller, _}, state) do
    code = Enum.join(state.code, "\n")

    {result, lua} =
      case :luerl_sandbox.run(code, state.lua) do
        {:error, reason} ->
          Player.notify(state.creator, "Error in #{state.name}: #{inspect(reason)}")
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
    Logger.debug("Stopping #{inspect(self())}")
    {:stop, :normal, :ok, state}
  end

  def handle_cast({:bg}, state) do
    code = Enum.join(state.code, "\n")

    lua =
      case :luerl_sandbox.run(code, state.lua) do
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

    {:noreply, state: state}
  end
end

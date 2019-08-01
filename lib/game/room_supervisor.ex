defmodule Game.RoomSupervisor do
  alias Game.Room
  use DynamicSupervisor

  def start_link(init_arg) do
    DynamicSupervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @impl true
  def init(init_arg) do
    DynamicSupervisor.init(strategy: :one_for_one, extra_arguments: [init_arg])
  end

  def start_child(name, x, y) do
    spec = {Room, [name, x, y]}
    DynamicSupervisor.start_child(__MODULE__, spec)
  end
end

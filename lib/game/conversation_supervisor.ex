defmodule Game.ConversationSupervisor do
  alias Game.BufferingConversation
  use DynamicSupervisor

  def start_link(init_arg) do
    DynamicSupervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @impl true
  def init(init_arg) do
    DynamicSupervisor.init(strategy: :one_for_one, extra_arguments: [init_arg])
  end

  def start_child(socket) do
    spec = {BufferingConversation, [socket]}
    {:ok, conversation} = DynamicSupervisor.start_child(__MODULE__, spec)
    :ok = :gen_tcp.controlling_process(socket, conversation)
    {:ok, conversation}
  end
end

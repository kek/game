defmodule GameTest do
  use ExUnit.Case
  doctest Game
  require Logger

  test "greets the world" do
    port = Application.get_env(:game, :port)
    {:ok, socket} = :gen_tcp.connect('localhost', port, [])
    Logger.info(socket, label: "socket")
    :ok = :gen_tcp.send(socket, 'BU!\r')

    receive do
      msg -> Logger.info(msg, label: "got")
    after
      1000 -> true
    end

    :ok = :gen_tcp.close(socket)
  end
end

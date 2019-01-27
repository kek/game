defmodule MudTest do
  use ExUnit.Case
  doctest Mud
  alias Mud.{TestServer}

  test "greets the world" do
    TestServer.start_link(5001)
    {:ok, socket} = :gen_tcp.connect('localhost', 5001, [])
    IO.inspect(socket, label: "socket")
    :ok = :gen_tcp.send(socket, 'BU!\r')

    receive do
      msg -> IO.inspect(msg, label: "got")
    after
      1000 -> true
    end

    :ok = :gen_tcp.close(socket)
  end
end

defmodule Game.ObjectTest do
  use ExUnit.Case
  alias Game.Object

  @tag :pending
  test "handles timeout failures" do
    code = ["say \"hi\"", "sleep(5000)", "say \"bye\""]
    {:ok, obj} = Object.start_link("object", code, self())
    assert Process.alive?(obj)
    assert Object.run(obj) == :timeout
    assert_receive {:"$gen_cast", {:notify, "Error in object: :timeout"}}
    assert Process.alive?(obj)
  end
end

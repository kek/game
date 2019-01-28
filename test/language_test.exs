defmodule Game.LanguageTest do
  use ExUnit.Case
  import Game.Language
  doctest Game.Language

  describe "run" do
    test "executes an expression" do
      assert run("(+ 1 2)") == 3
    end
  end
end

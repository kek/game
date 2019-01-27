defmodule Game.LanguageTest do
  use ExUnit.Case
  alias Game.Language

  @tag :pending
  describe "run" do
    test "executes an expression" do
      assert Language.run("(+ 1 2)") == 3
    end
  end

  describe "parse" do
    test "parses an empty list" do
      assert Language.parse("()") == []
    end
  end
end

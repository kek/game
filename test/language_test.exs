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
      assert Language.parse("()") == {:ok, []}
    end

    test "parses a list" do
      assert Language.parse("(f x)") == {:ok, [:f, :x]}
    end

    test "returns error when given incorrect source" do
      assert Language.parse(")") == {:error, {1, :lfe_parse, {:illegal, :")"}}}
    end
  end
end

defmodule Game.LanguageTest do
  use ExUnit.Case
  import Game.Language
  doctest Game.Language

  @tag :pending
  describe "run" do
    test "executes an expression" do
      assert run("(+ 1 2)") == 3
    end
  end

  describe "parse" do
    test "parses an empty list" do
      assert parse("()") == {:ok, []}
    end

    test "parses a list" do
      assert parse("(f x)") == {:ok, [:f, :x]}
    end

    test "returns error when given incorrect source" do
      assert parse(")") == {:error, {1, :lfe_parse, {:illegal, :")"}}}
    end
  end
end

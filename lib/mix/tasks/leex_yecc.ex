defmodule Mix.Tasks.LeexYecc do
  use Mix.Task

  def run([]) do
    IO.inspect(:leex.file('src/calx_lexer.xrl'))
    IO.inspect(:yecc.file('src/calx_parser.yrl'))
  end
end

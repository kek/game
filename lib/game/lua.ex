defmodule Game.Lua do
  def run(lines) do
    st = :luerl_sandbox.init()
    code = Enum.join(lines, "\n")
    {:ok, result} = :luerl.eval(code, st)
    result
  end
end

defmodule Mix.Tasks.Lferepl do
  use Mix.Task

  # ERL_LIBS=/home/ke/lfe::./deps/observer_cli:./deps/lutil:./deps/file_system:./deps/ltest:./deps/mix_lfe:./deps/color:./deps/lfe:./deps/mix_test_watch:./deps/recon:: exec erl -user lfe_init -extra
  # echo (for stuff in _build/**/ebin; echo -- -pa $stuff; end)

  def run(args) do
    try do
      :lfe_shell.server()
    rescue
      CaseClauseError -> run(args)
    end
  end
end

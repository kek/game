defmodule Game.Session do
  alias Game.{Conversation, Commands}
  require Logger

  def handle(session, message) do
    Logger.info("got #{message} from #{inspect(session)}")

    program =
      if message =~ ~r/^\(.*\)$/ do
        message
      else
        "(#{message})"
      end

    if message =~ ~r/^\W*$/ do
      Conversation.output(session, "no input")
    else
      Commands.load()

      result = Symbelix.run(program, Commands)

      Conversation.output(session, "#{program} -> #{inspect(result)}")
    end
  end
end

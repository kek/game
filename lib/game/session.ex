defmodule Game.Session do
  alias Game.Conversation

  def handle(session, message) do
    IO.puts("got #{message} from #{inspect(session)}")
    Conversation.output(session, "tack: ", newline: false)
    Conversation.output(session, message)
  end
end

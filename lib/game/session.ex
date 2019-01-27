defmodule Game.Session do
  alias Game.Conversation
  require Logger

  def handle(session, message) do
    Logger.info("got #{message} from #{inspect(session)}")
    Conversation.output(session, "tack: ", newline: false)
    Conversation.output(session, message)
  end
end

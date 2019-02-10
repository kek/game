defmodule Game.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    # List all child processes to be supervised
    children = [
      # Starts a worker by calling: Game.Worker.start_link(arg)
      # {Game.Worker, arg}
      {Game.Server, Application.get_env(:game, :port)},
      {Game.World, []}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Game.Supervisor]
    Supervisor.start_link(children, opts)
  end
end

defmodule Game.MixProject do
  use Mix.Project

  def project do
    [
      app: :game,
      version: "0.1.0",
      elixir: "~> 1.8",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger, :sasl],
      mod: {Game.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:observer_cli, "~> 1.4"},
      {:mix_test_watch, "~> 0.9.0", only: :dev, runtime: false},
      {:symbelix, github: "kek/symbelix"},
      {:faker, "~> 0.12.0"},
      {:luerl, "~> 0.4.0"},
      {:remix, "~> 0.0.2", only: :dev},
      {:dialyxir, "~> 0.5.1"}
    ]
  end
end

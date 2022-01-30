defmodule TfliteElixir.MixProject do
  use Mix.Project

  def project do
    [
      app: :tflite_elixir,
      version: "0.1.0",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:elixir_make, "~> 0.6"},
      {:ex_doc, "~> 0.27", only: :dev, runtime: false},
    ]
  end
end


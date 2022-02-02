defmodule TfliteElixir.MixProject do
  use Mix.Project
  require Logger

  @app :tflite_elixir
  @tflite_version "2.8.0"
  # only means compatible. need to write more tests
  @compatible_tflite_versions ["2.7.0", "2.8.0"]
  def project do
    [
      app: @app,
      version: "0.1.0",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      compilers: [:elixir_make] ++ Mix.compilers(),
      deps: deps(),
      source_url: "https://github.com/cocox-xu/tflite_elixir",
      description: description(),
      package: package(),
      test_coverage: [ignore_modules: [TFLite.Nif], tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test
      ],
      make_env: %{
        "TFLITE_VER" => tflite_versions(System.get_env("TFLITE_VER", @tflite_version)),
        "MAKE_BUILD_FLAGS" =>
          System.get_env("MAKE_BUILD_FLAGS", "-j#{System.schedulers_online()}")
      },
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:elixir_make, "~> 0.6"},
      {:excoveralls, "~> 0.10", only: :test},
      {:ex_doc, "~> 0.27", only: [:dev, :test], runtime: false},
      {:nx, "~> 0.1", optional: true}
    ]
  end

  defp description() do
    "TensorflowLite-Elixir bindings."
  end

  defp package() do
    [
      name: "tflite_elixir",
      # These are the default files included in the package
      files: ~w(lib c_src 3rd_party .formatter.exs mix.exs README* LICENSE*),
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => "https://github.com/cocoa-xu/tflite_elixir"}
    ]
  end

  def tflite_versions(version) do
    if Enum.member?(@compatible_tflite_versions, version) do
      version
    else
      Logger.warn(
        "Tensorflow Lite version #{version} is not in the compatible list, you may encounter compile errors"
      )

      Logger.warn(
        "Compatible Tensorflow Lite versions: " <>
          (@compatible_tflite_versions |> Enum.join(", "))
      )

      version
    end
  end
end

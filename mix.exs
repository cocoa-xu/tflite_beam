defmodule TfliteElixir.MixProject do
  use Mix.Project
  require Logger

  @app :tflite_elixir
  @tflite_version "2.9.1"
  # only means compatible. need to write more tests
  @compatible_tflite_versions [
    "2.7.0",
    "2.7.1",
    "2.7.2",
    "2.7.3",
    "2.8.0",
    "2.8.1",
    "2.8.2",
    "2.9.0",
    "2.9.1"
  ]

  # coral related
  @default_edgetpu_runtime "edgetpu_runtime_20220624"
  @default_edgetpu_libraries "native"
  @enable_coral_support_by_default "YES"

  def project do
    enable_coral_support =
      System.get_env("TFLITE_ELIXIR_CORAL_SUPPORT", @enable_coral_support_by_default)

    System.put_env("TFLITE_ELIXIR_CORAL_SUPPORT", enable_coral_support)

    if enable_coral_support == "YES" do
      edgetpu_runtime =
        System.get_env("TFLITE_ELIXIR_CORAL_LIBEDGETPU_RUNTIME", @default_edgetpu_runtime)

      edgetpu_libraries =
        System.get_env("TFLITE_ELIXIR_CORAL_LIBEDGETPU_LIBRARIES", @default_edgetpu_libraries)

      :ok = download_edgetpu_runtime(edgetpu_runtime)
      System.put_env("TFLITE_ELIXIR_CORAL_LIBEDGETPU_RUNTIME", edgetpu_runtime)
      System.put_env("TFLITE_ELIXIR_CORAL_LIBEDGETPU_LIBRARIES", edgetpu_libraries)
    end

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
      test_coverage: [ignore_modules: [TFLite.Nif, TFLiteElixir.Coral], tool: ExCoveralls],
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
      }
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:nx, "~> 0.2"},
      {:stb_image, "~> 0.5"},
      {:elixir_make, "~> 0.6", runtime: false},
      {:excoveralls, "~> 0.10", only: :test},
      {:ex_doc, "~> 0.27", only: [:dev, :test], runtime: false}
    ]
  end

  defp description() do
    "TensorFlowLite-Elixir bindings."
  end

  defp package() do
    [
      name: "tflite_elixir",
      # These are the default files included in the package
      files:
        ~w(lib c_src 3rd_party priv scripts CMakeLists.txt Makefile .gitmodules .formatter.exs mix.exs README* LICENSE*),
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

  defp cache_dir() do
    System.get_env("TFLITE_ELIXIR_CACHE_DIR", "./3rd_party/cache")
  end

  defp download_edgetpu_runtime(runtime) do
    # always download edgetpu runtime for linux
    linux_runtime = "#{runtime}_linux"
    filename = "#{linux_runtime}.zip"
    runtime_url = "https://github.com/cocoa-xu/libedgetpu/releases/download/grouper/#{filename}"
    unzip_to = Path.join([cache_dir(), linux_runtime])
    download_zip_file(filename, runtime_url, unzip_to)

    case :os.type() do
      {:unix, :darwin} ->
        macos_runtime = "#{runtime}_macos"
        filename = "#{macos_runtime}.zip"
        runtime_url = "https://github.com/cocoa-xu/libedgetpu/releases/download/grouper/#{filename}"
        unzip_to = Path.join([cache_dir(), macos_runtime])
        download_zip_file(filename, runtime_url, unzip_to)

      {:win32, :nt} ->
        windows_runtime = "#{runtime}_windows"
        filename = "#{windows_runtime}.zip"
        runtime_url = "https://github.com/cocoa-xu/libedgetpu/releases/download/grouper/#{filename}"
        unzip_to = Path.join([cache_dir(), windows_runtime])
        download_zip_file(filename, runtime_url, unzip_to)
    end
  end

  defp download_zip_file(filename, url, unzip_to) do
    if !File.exists?(unzip_to) do
      File.mkdir_p!(unzip_to)

      cache_location = Path.join([cache_dir(), filename])

      if !File.exists?(cache_location) do
        :ssl.start()
        :inets.start()
        :ok = download!(url, cache_location)
      end

      with {:ok, _} <-
             :zip.unzip(String.to_charlist(cache_location), [
               {:cwd, String.to_charlist(unzip_to)}
             ]) do
        :ok
      else
        _ -> {:error, "failed to unzip file #{filename}"}
      end
    else
      :ok
    end
  end

  defp download!(url, save_as, overwrite \\ false)

  defp download!(url, save_as, false) do
    unless File.exists?(save_as) do
      download!(url, save_as, true)
    end

    :ok
  end

  defp download!(url, save_as, true) do
    http_opts = []
    opts = [body_format: :binary]
    arg = {url, []}

    body =
      case :httpc.request(:get, arg, http_opts, opts) do
        {:ok, {{_, 200, _}, _, body}} ->
          body

        {:error, reason} ->
          raise inspect(reason)
      end

    File.write!(save_as, body)
  end
end

defmodule TfliteElixir.MixProject do
  use Mix.Project
  require Logger

  @app :tflite_elixir
  @tflite_version "2.8.0"
  # only means compatible. need to write more tests
  @compatible_tflite_versions ["2.7.0", "2.8.0"]

  # coral related
  @default_edgetpu_runtime "edgetpu_runtime_20220308"
  @default_edgetpu_libraries "native"
  @throttle_coral_usb "YES"

  def project do
    enable_coral_support =
      case Application.compile_env(@app, :enable_coral_support, false) do
        true -> "YES"
        _ -> "NO"
      end
    System.put_env("TFLITE_ELIXIR_CORAL_SUPPORT", enable_coral_support)
    unless enable_coral_support == "NO" do
      edgetpu_runtime = System.get_env("TFLITE_ELIXIR_CORAL_LIBEDGETPU_RUNTIME", @default_edgetpu_runtime)
      throttle_coral_usb = System.get_env("TFLITE_ELIXIR_CORAL_USB_THROTTLE", @throttle_coral_usb)
      edgetpu_libraries = System.get_env("TFLITE_ELIXIR_CORAL_LIBEDGETPU_LIBRARIES", @default_edgetpu_libraries)

      :ok = download_edgetpu_runtime(edgetpu_runtime)
      {:ok, _} = install_edgetpu_runtime(edgetpu_runtime, throttle_coral_usb, edgetpu_libraries)
      System.put_env("TFLITE_ELIXIR_CORAL_LIBEDGETPU_RUNTIME", edgetpu_runtime)
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
      {:elixir_make, "~> 0.6", runtime: false},
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

  defp cache_dir() do
    System.get_env("TFLITE_ELIXIR_CACHE_DIR", "./3rd_party/cache")
  end

  defp install_edgetpu_runtime(runtime, throttle, edgetpu_libraries) do
    copy_to = Path.join([Mix.Project.build_path(), "lib/tflite_elixir/priv/libedgetpu"])
    unless File.exists?(copy_to) do
      File.mkdir_p!(copy_to)

      runtime_dir =
        if throttle == "NO" do
          "direct"
        else
          "throttled"
        end
      unzipped_location = Path.join([cache_dir(), runtime, "edgetpu_runtime/libedgetpu", runtime_dir])

      edgetpu_libraries =
        case edgetpu_libraries do
          "native" ->
            case :os.type() do
              {:unix, :darwin} ->
                case :os.cmd('uname -p') do
                  'arm\n' ->
                    "darwin_arm64"
                  _ ->
                    "darwin_x86_64"
                end
              {:unix, _} ->
                case :os.cmd('uname -p') do
                  'aarch64\n' ->
                    "aarch64"
                  'x86_64\n' ->
                    "k8"
                  'armv7l\n' ->
                    "armv7a"
                  unsupported ->
                    raise RuntimeError, "#{inspect(unsupported)} is not supported"
                end
              {:win32, :nt} ->
                "x64_windows"
            end
          specific ->
            specific
        end

      File.cp_r!(Path.join([unzipped_location, edgetpu_libraries]), copy_to)
    end

    {:ok, copy_to}
  end

  defp download_edgetpu_runtime(runtime) do
    filename = "#{runtime}.zip"
    runtime_url = "https://github.com/google-coral/libedgetpu/releases/download/release-grouper/#{filename}"
    unzip_to = Path.join([cache_dir(), runtime])
    download_zip_file(filename, runtime_url, unzip_to)
  end

  defp download_zip_file(filename, url, unzip_to) do
    File.mkdir_p!(unzip_to)

    cache_location = Path.join([cache_dir(), filename])
    if !File.exists?(cache_location) do
      :ssl.start()
      :inets.start()
      :ok = download!(url, cache_location)
    end

    with {:ok, _} <- :zip.unzip(String.to_charlist(cache_location), [
      {:cwd, String.to_charlist(unzip_to)}
    ]) do
      :ok
    else
      _ -> {:error, "failed to unzip file #{filename}"}
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

defmodule TfliteElixir.MixProject do
  use Mix.Project
  require Logger

  @app :tflite_elixir
  @version "0.1.0"
  @tflite_version "2.9.1"
  @prefer_precompiled "YES"
  @github_url "https://github.com/cocoa-xu/tflite_elixir"
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
    prefer_precompiled = System.get_env("TFLITE_ELIXIR_PREFER_PRECOMPILED", @prefer_precompiled)
    {:ok, compilers} = use_precompiled(prefer_precompiled)

    [
      app: @app,
      version: @version,
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      compilers: compilers,
      deps: deps(),
      source_url: @github_url,
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

  defp use_precompiled("YES") do
    tflite_version = System.get_env("TFLITE_VER", @tflite_version)

    enable_coral_support =
      System.get_env("TFLITE_ELIXIR_CORAL_SUPPORT", @enable_coral_support_by_default)

    edgetpu_libraries =
      System.get_env("TFLITE_ELIXIR_CORAL_LIBEDGETPU_LIBRARIES", @default_edgetpu_libraries)

    {precompiled_available, url, filename} =
      has_precompiled_binaries(tflite_version, enable_coral_support, edgetpu_libraries)

    if precompiled_available do
      unarchive_to = Path.join([cache_dir(), "precompiled", filename])

      with :ok <- download_precompiled(filename, url, unarchive_to) do
        System.put_env(
          "TFLITE_ELIXIR_ONLY_COPY_PRIV",
          Path.join([unarchive_to, filename, "priv"])
        )

        {:ok, [:elixir_make] ++ Mix.compilers()}
      else
        _ ->
          use_precompiled("NO")
      end
    else
      use_precompiled("NO")
    end
  end

  defp use_precompiled("NO") do
    enable_coral_support =
      System.get_env("TFLITE_ELIXIR_CORAL_SUPPORT", @enable_coral_support_by_default)

    System.put_env("TFLITE_ELIXIR_CORAL_SUPPORT", enable_coral_support)

    if enable_coral_support == "YES" do
      edgetpu_runtime =
        System.get_env("TFLITE_ELIXIR_CORAL_LIBEDGETPU_RUNTIME", @default_edgetpu_runtime)

      edgetpu_libraries =
        System.get_env("TFLITE_ELIXIR_CORAL_LIBEDGETPU_LIBRARIES", @default_edgetpu_libraries)

      :ok = download_edgetpu_runtime(edgetpu_runtime, edgetpu_libraries)
      System.put_env("TFLITE_ELIXIR_CORAL_LIBEDGETPU_RUNTIME", edgetpu_runtime)
      System.put_env("TFLITE_ELIXIR_CORAL_LIBEDGETPU_LIBRARIES", edgetpu_libraries)
    end

    {:ok, [:elixir_make] ++ Mix.compilers()}
  end

  defp has_precompiled_binaries(
         tflite_version = @tflite_version,
         _enable_coral_support = "YES",
         edgetpu_libraries
       ) do
    case edgetpu_libraries do
      lib when lib in ["k8", "x86_64", "aarch64", "armv7a", "riscv64"] ->
        lib =
          if lib == "k8" do
            "x86_64"
          else
            lib
          end

        filename = "tflite_elixir-linux-#{lib}-v#{@version}"
        {true, "#{@github_url}/releases/download/v#{@version}/#{filename}.zip", filename}

      lib when lib in ["darwin_arm64", "darwin_x86_64"] ->
        filename =
          case lib do
            "darwin_arm64" -> "tflite_elixir-macos-arm64-v#{@version}"
            "darwin_x86_64" -> "tflite_elixir-macos-x86_64-v#{@version}"
          end

        {true, "#{@github_url}/releases/download/v#{@version}/#{filename}.zip", filename}

      _ ->
        {false, nil, nil}
    end
  end

  defp has_precompiled_binaries(_tflite_version, _enable_coral_support, _edgetpu_libraries) do
    false
  end

  defp download_precompiled(filename, url, unarchive_to) do
    download_archived_file(filename, url, unarchive_to, :zip)
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
    "TensorFlow Lite-Elixir binding with TPU support."
  end

  defp package() do
    [
      name: "tflite_elixir",
      # These are the default files included in the package
      files:
        ~w(lib c_src 3rd_party priv scripts CMakeLists.txt Makefile .gitmodules .formatter.exs mix.exs README* LICENSE*),
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => @github_url}
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

  defp download_edgetpu_runtime(runtime, edgetpu_libraries) do
    {filename, runtime_url, unzip_to} =
      case edgetpu_libraries do
        "native" ->
          case :os.type() do
            {:unix, :darwin} ->
              macos_runtime = "#{runtime}_macos"
              filename = "#{macos_runtime}.zip"

              runtime_url =
                "https://github.com/cocoa-xu/libedgetpu/releases/download/grouper/#{filename}"

              unzip_to = Path.join([cache_dir(), macos_runtime])
              {filename, runtime_url, unzip_to}

            {:unix, _} ->
              linux_runtime = "#{runtime}_linux"
              filename = "#{linux_runtime}.zip"

              runtime_url =
                "https://github.com/cocoa-xu/libedgetpu/releases/download/grouper/#{filename}"

              unzip_to = Path.join([cache_dir(), linux_runtime])
              {filename, runtime_url, unzip_to}

            {:win32, :nt} ->
              windows_runtime = "#{runtime}_windows"
              filename = "#{windows_runtime}.zip"

              runtime_url =
                "https://github.com/cocoa-xu/libedgetpu/releases/download/grouper/#{filename}"

              unzip_to = Path.join([cache_dir(), windows_runtime])
              {filename, runtime_url, unzip_to}
          end

        "x64_windows" ->
          windows_runtime = "#{runtime}_windows"
          filename = "#{windows_runtime}.zip"

          runtime_url =
            "https://github.com/cocoa-xu/libedgetpu/releases/download/grouper/#{filename}"

          unzip_to = Path.join([cache_dir(), windows_runtime])
          {filename, runtime_url, unzip_to}

        lib when lib in ["darwin_arm64", "darwin_x86_64"] ->
          macos_runtime = "#{runtime}_macos"
          filename = "#{macos_runtime}.zip"

          runtime_url =
            "https://github.com/cocoa-xu/libedgetpu/releases/download/grouper/#{filename}"

          unzip_to = Path.join([cache_dir(), macos_runtime])
          {filename, runtime_url, unzip_to}

        lib when lib in ["k8", "x86_64", "aarch64", "armv7a", "riscv64", "s390x", "ppc64el"] ->
          linux_runtime = "#{runtime}_linux"
          filename = "#{linux_runtime}.zip"

          runtime_url =
            "https://github.com/cocoa-xu/libedgetpu/releases/download/grouper/#{filename}"

          unzip_to = Path.join([cache_dir(), linux_runtime])
          {filename, runtime_url, unzip_to}
      end

    download_archived_file(filename, runtime_url, unzip_to, :zip)
  end

  defp download_archived_file(filename, url, unarchive_to, type) do
    if !File.exists?(unarchive_to) do
      File.mkdir_p!(unarchive_to)
    end

    cache_location = Path.join([cache_dir(), filename])

    if !File.exists?(cache_location) do
      :ssl.start()
      :inets.start()
      :ok = download!(url, cache_location)
    end

    case type do
      :zip -> unzip_file(cache_location, unarchive_to)
      _ -> :ok
    end
  end

  defp unzip_file(filepath, unzip_to) do
    with {:ok, _} <-
           :zip.unzip(String.to_charlist(filepath), [
             {:cwd, String.to_charlist(unzip_to)}
           ]) do
      :ok
    else
      _ -> {:error, "failed to unzip file #{filepath}"}
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

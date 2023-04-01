defmodule TFLiteBEAM.MixProject do
  use Mix.Project
  require Logger

  @app :tflite_beam
  @version "0.2.0"
  @tflite_version "2.11.0"
  @prefer_precompiled "YES"
  @github_url "https://github.com/cocoa-xu/tflite_beam"
  @libedgetpu_runtime_github_url "https://github.com/cocoa-xu/libedgetpu"
  @libedgetpu_runtime_version "0.1.5"
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
    "2.9.1",
    "2.9.2",
    "2.9.3",
    "2.10.0",
    "2.10.1",
    "2.11.0"
  ]

  @precompiled_triplets %{
    "x86_64" => [
      "x86_64-linux-gnu"
    ],
    "k8" => [
      "x86_64-linux-gnu"
    ],
    "aarch64" => [
      "aarch64-linux-gnu",
      "aarch64-linux-musl"
    ],
    "armv7l" => [
      "armv7l-linux-gnueabihf"
    ],
    "armv6" => [
      "armv6-linux-gnueabihf"
    ],
    "riscv64" => [
      "riscv64-linux-gnu",
      "riscv64-linux-musl"
    ]
  }

  # coral related
  @default_edgetpu_libraries "native"
  @enable_coral_support_by_default "YES"

  def project do
    prefer_precompiled = System.get_env("TFLITE_BEAM_PREFER_PRECOMPILED", @prefer_precompiled)

    prefer_precompiled =
      case prefer_precompiled do
        "YES" -> true
        _ -> false
      end

    {:ok, compilers} = use_precompiled(prefer_precompiled)

    [
      app: @app,
      version: @version,
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      compilers: compilers,
      deps: deps(),
      source_url: @github_url,
      description: description(),
      package: package(),
      test_coverage: [ignore_modules: [:tflite_beam_nif, TFLiteBEAM.Coral], tool: ExCoveralls],
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

  defp use_precompiled(true) do
    tflite_version = System.get_env("TFLITE_VER", @tflite_version)

    enable_coral_support =
      System.get_env("TFLITE_BEAM_CORAL_SUPPORT", @enable_coral_support_by_default)

    System.put_env("TFLITE_BEAM_CORAL_SUPPORT", enable_coral_support)

    edgetpu_libraries =
      System.get_env("TFLITE_BEAM_CORAL_LIBEDGETPU_LIBRARIES", @default_edgetpu_libraries)

    System.put_env("TFLITE_BEAM_CORAL_LIBEDGETPU_LIBRARIES", edgetpu_libraries)

    case has_precompiled_binaries(tflite_version, enable_coral_support, edgetpu_libraries) do
      {true, url, filename} ->
        unarchive_to = Path.join([cache_dir(), "precompiled", filename])

        with :ok <- download_precompiled("#{filename}.tar.gz", url, unarchive_to) do
          System.put_env(
            "TFLITE_BEAM_ONLY_COPY_PRIV",
            Path.join([unarchive_to, filename, "priv"])
          )

          {:ok, [:elixir_make] ++ Mix.compilers()}
        else
          _ ->
            use_precompiled(false)
        end

        {:ok, [:elixir_make] ++ Mix.compilers()}

      _ ->
        use_precompiled(false)
    end
  end

  defp use_precompiled(false) do
    System.put_env("TFLITE_BEAM_PREFER_PRECOMPILED", "NO")
    System.put_env("TFLITE_BEAM_ONLY_COPY_PRIV", "NO")

    enable_coral_support =
      System.get_env("TFLITE_BEAM_CORAL_SUPPORT", @enable_coral_support_by_default)

    System.put_env("TFLITE_BEAM_CORAL_SUPPORT", enable_coral_support)

    if enable_coral_support == "YES" do
      edgetpu_libraries =
        System.get_env("TFLITE_BEAM_CORAL_LIBEDGETPU_LIBRARIES", @default_edgetpu_libraries)

      with {:ok, url, filename, triplet} <- edgetpu_runtime_url(edgetpu_libraries) do
        System.put_env("TFLITE_BEAM_CORAL_LIBEDGETPU_URL", url)
        System.put_env("TFLITE_BEAM_CORAL_LIBEDGETPU_TRIPLET", triplet)
        System.put_env("TFLITE_BEAM_CORAL_LIBEDGETPU_RUNTIME", filename)
        {:ok, [:elixir_make] ++ Mix.compilers()}
      else
        {:error, error} ->
          Logger.warning(error)
          {:ok, Mix.compilers()}
      end
    else
      {:ok, [:elixir_make] ++ Mix.compilers()}
    end
  end

  defp get_triplet(edgetpu_libraries) do
    {edgetpu_libraries, target_os} =
      if edgetpu_libraries == "native" do
        {native_arch, native_os} =
          case :os.type() do
            {:unix, :darwin} ->
              {machine, 0} = System.cmd("uname", ["-m"])
              [machine | _] = String.split(machine, "\n")
              {"darwin_#{machine}", "apple"}

            {:unix, _} ->
              {machine, 0} = System.cmd("uname", ["-m"])
              [machine | _] = String.split(machine, "\n")

              case machine do
                "armv7" <> _ ->
                  {"armv7l", "linux"}

                "armv6" <> _ ->
                  {"armv6", "linux"}

                _ ->
                  {machine, "linux"}
              end

            _ ->
              {nil, nil}
          end

        {System.get_env("TARGET_ARCH", native_arch), System.get_env("TARGET_OS", native_os)}
      else
        {System.get_env("TARGET_ARCH", edgetpu_libraries), System.get_env("TARGET_OS", nil)}
      end

    case {edgetpu_libraries, target_os} do
      {lib, "linux"}
      when lib in ["k8", "x86_64", "aarch64", "armv7l", "arm", "armv6", "riscv64"] ->
        lib =
          case lib do
            "k8" ->
              "x86_64"

            "arm" ->
              case {System.get_env("TARGET_ABI"), System.get_env("TARGET_OS"),
                    System.get_env("TARGET_CPU")} do
                {"gnueabihf", "linux", "arm1176jzf_s"} ->
                  "armv6"

                {"gnueabihf", "linux", "cortex" <> _} ->
                  "armv7l"

                _ ->
                  "arm"
              end

            lib ->
              lib
          end

        get_triplet_if_possible(lib)

      {lib, "apple"}
      when lib in [
             "darwin_aarch64",
             "darwin_arm64",
             "darwin_x86_64",
             "aarch64",
             "arm64",
             "x86_64"
           ] ->
        case lib do
          lib when lib in ["arm64", "aarch64"] ->
            get_triplet_if_possible("darwin_arm64")

          "x86_64" ->
            get_triplet_if_possible("darwin_x86_64")

          _ ->
            get_triplet_if_possible(lib)
        end

      _ ->
        {:error, edgetpu_libraries, []}
    end
  end

  defp get_triplet_if_possible(requested_arch)
       when requested_arch in ["darwin_aarch64", "darwin_arm64", "darwin_x86_64"] do
    requested_os = System.get_env("TARGET_OS", "apple")
    requested_abi = System.get_env("TARGET_ABI", "darwin")
    "darwin_" <> target_arch = requested_arch
    requested_triplet = "#{target_arch}-#{requested_os}-#{requested_abi}"

    case requested_arch do
      "darwin_arm64" -> {:ok, "aarch64-apple-darwin"}
      "darwin_aarch64" -> {:ok, "aarch64-apple-darwin"}
      "darwin_x86_64" -> {:ok, "x86_64-apple-darwin"}
      _ -> {:error, requested_triplet, ["aarch64-apple-darwin", "x86_64-apple-darwin"]}
    end
  end

  defp get_triplet_if_possible(requested_arch)
       when requested_arch in ["k8", "x86_64", "aarch64", "riscv64", "armv7l", "arm", "armv6"] do
    requested_os = System.get_env("TARGET_OS", "linux")
    requested_abi = System.get_env("TARGET_ABI", "gnu")
    requested_triplet = "#{requested_arch}-#{requested_os}-#{requested_abi}"
    available_precompiled_binaries = Map.get(@precompiled_triplets, requested_arch, [])

    if requested_triplet in available_precompiled_binaries do
      {:ok, requested_triplet}
    else
      {:error, requested_triplet, available_precompiled_binaries}
    end
  end

  defp has_precompiled_binaries(
         _tflite_version = @tflite_version,
         _enable_coral_support = "YES",
         edgetpu_libraries
       ) do
    with {:ok, triplet} <- get_triplet(edgetpu_libraries) do
      filename = "tflite_beam-nif-#{:erlang.system_info(:nif_version)}-#{triplet}-v#{@version}"
      {true, "#{@github_url}/releases/download/v#{@version}/#{filename}.tar.gz", filename}
    else
      {:error, requested_triplet, _available_precompiled_triplets} ->
        Logger.warning(
          "No precompiled binaries for #{requested_triplet}, will try to build from source."
        )

        false
    end
  end

  defp has_precompiled_binaries(_tflite_version, _enable_coral_support, _edgetpu_libraries) do
    false
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:elixir_make, "~> 0.7", runtime: false},
      {:nx, "~> 0.5"},
      {:stb_image, "~> 0.6"},
      {:excoveralls, "~> 0.10", only: :test},
      {:ex_doc, "~> 0.27", only: :docs, runtime: false}
    ]
  end

  defp description() do
    "TensorFlow Lite BEAM binding with optional TPU support."
  end

  defp package() do
    [
      name: to_string(@app),
      # These are the default files included in the package
      files: ~w(
        c_src/*.h
        c_src/*.cpp
        c_src/*.hpp
        c_src/coral
        c_src/tflite
        cc_toolchain
        scripts
        patches
        unicodedata
        CMakeLists.txt
        Makefile
        .gitmodules
        lib
        src
        .formatter.exs
        mix.exs
        README*
        LICENSE*
        rebar.config
      ),
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => @github_url}
    ]
  end

  def tflite_versions(version) do
    if Enum.member?(@compatible_tflite_versions, version) do
      version
    else
      Logger.warning(
        "Tensorflow Lite version #{version} is not in the compatible list, you may encounter compile errors"
      )

      Logger.warning(
        "Compatible Tensorflow Lite versions: " <>
          (@compatible_tflite_versions |> Enum.join(", "))
      )

      version
    end
  end

  defp cache_dir() do
    System.get_env(
      "TFLITE_BEAM_CACHE_DIR",
      Path.join(Path.dirname(Path.expand(__ENV__.file)), "3rd_party/cache")
    )
  end

  defp edgetpu_runtime_url(edgetpu_libraries) do
    with {:ok, triplet} <- get_triplet(edgetpu_libraries) do
      filename = "edgetpu_runtime_#{triplet}_v#{@libedgetpu_runtime_version}"

      runtime_url =
        "#{@libedgetpu_runtime_github_url}/releases/download/v#{@libedgetpu_runtime_version}/#{filename}.tar.gz"

      {:ok, runtime_url, filename, triplet}
    else
      {:error, requested_triplet, _available_precompiled_triplets} ->
        msg = "No precompiled libedgetpu runtime binaries for #{requested_triplet}."
        Logger.warning(msg)
        {:error, msg}
    end
  end

  defp download_precompiled(filename, url, unarchive_to) do
    if !File.exists?(unarchive_to) do
      File.mkdir_p!(unarchive_to)
    end

    cache_location = Path.join([cache_dir(), filename])

    status =
      if !File.exists?(cache_location) do
        :ssl.start()
        :inets.start()
        download!(url, cache_location)
      else
        :ok
      end

    if status == :ok do
      unarchive_file(cache_location, unarchive_to)
    end
  end

  defp unarchive_file(filepath, unarchive_to) do
    case File.read(filepath) do
      {:ok, contents} ->
        File.rm_rf!(unarchive_to)

        case :erl_tar.extract({:binary, contents}, [:compressed, {:cwd, unarchive_to}]) do
          :ok ->
            :ok

          {:error, term} ->
            {:error, "cannot decompress precompiled #{inspect(filepath)}: #{inspect(term)}"}
        end

      {:error, reason} ->
        {:error,
         "precompiled #{inspect(filepath)} does not exist or cannot download: #{inspect(reason)}"}
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
    case download(url) do
      {:ok, body} ->
        File.write!(save_as, body)

      {:error, reason} ->
        Mix.raise(reason)
    end
  end

  def download(url) do
    url_charlist = String.to_charlist(url)

    {:ok, _} = Application.ensure_all_started(:inets)
    {:ok, _} = Application.ensure_all_started(:ssl)
    {:ok, _} = Application.ensure_all_started(:public_key)

    if proxy = System.get_env("HTTP_PROXY") || System.get_env("http_proxy") do
      Mix.shell().info("Using HTTP_PROXY: #{proxy}")
      %{host: host, port: port} = URI.parse(proxy)

      :httpc.set_options([{:proxy, {{String.to_charlist(host), port}, []}}])
    end

    if proxy = System.get_env("HTTPS_PROXY") || System.get_env("https_proxy") do
      Mix.shell().info("Using HTTPS_PROXY: #{proxy}")
      %{host: host, port: port} = URI.parse(proxy)
      :httpc.set_options([{:https_proxy, {{String.to_charlist(host), port}, []}}])
    end

    # https://erlef.github.io/security-wg/secure_coding_and_deployment_hardening/inets
    # TODO: This may no longer be necessary from Erlang/OTP 25.0 or later.
    https_options = [
      ssl:
        [
          verify: :verify_peer,
          customize_hostname_check: [
            match_fun: :public_key.pkix_verify_hostname_match_fun(:https)
          ]
        ] ++ cacerts_options()
    ]

    options = [body_format: :binary]

    case :httpc.request(:get, {url_charlist, []}, https_options, options) do
      {:ok, {{_, 200, _}, _headers, body}} ->
        {:ok, body}

      other ->
        {:error, "couldn't fetch file from #{url}: #{inspect(other)}"}
    end
  end

  defp cacerts_options do
    cond do
      path = System.get_env("ELIXIR_MAKE_CACERT") ->
        [cacertfile: path]

      Application.spec(:castore, :vsn) ->
        [cacertfile: Application.app_dir(:castore, "priv/cacerts.pem")]

      Application.spec(:certifi, :vsn) ->
        [cacertfile: Application.app_dir(:certifi, "priv/cacerts.pem")]

      certs = otp_cacerts() ->
        [cacerts: certs]

      path = cacerts_from_os() ->
        [cacertfile: path]

      true ->
        warn_no_cacerts()
        []
    end
  end

  defp otp_cacerts do
    if System.otp_release() >= "25" do
      # cacerts_get/0 raises if no certs found
      try do
        :public_key.cacerts_get()
      rescue
        _ ->
          nil
      end
    end
  end

  # https_opts and related code are taken from
  # https://github.com/elixir-cldr/cldr_utils/blob/v2.19.1/lib/cldr/http/http.ex
  @certificate_locations [
    # Debian/Ubuntu/Gentoo etc.
    "/etc/ssl/certs/ca-certificates.crt",

    # Fedora/RHEL 6
    "/etc/pki/tls/certs/ca-bundle.crt",

    # OpenSUSE
    "/etc/ssl/ca-bundle.pem",

    # OpenELEC
    "/etc/pki/tls/cacert.pem",

    # CentOS/RHEL 7
    "/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem",

    # Open SSL on MacOS
    "/usr/local/etc/openssl/cert.pem",

    # MacOS & Alpine Linux
    "/etc/ssl/cert.pem"
  ]

  defp cacerts_from_os do
    Enum.find(@certificate_locations, &File.exists?/1)
  end

  defp warn_no_cacerts do
    Mix.shell().error("""
    No certificate trust store was found.

    Tried looking for: #{inspect(@certificate_locations)}

    A certificate trust store is required in
    order to download locales for your configuration.
    Since elixir_make could not detect a system
    installed certificate trust store one of the
    following actions may be taken:

    1. Install the hex package `castore`. It will
       be automatically detected after recompilation.

    2. Install the hex package `certifi`. It will
       be automatically detected after recompilation.

    3. Specify the location of a certificate trust store
       by configuring it in environment variable:

         export ELIXIR_MAKE_CACERT="/path/to/cacerts.pem"

    4. Use OTP 25+ on an OS that has built-in certificate
       trust store.
    """)
  end
end

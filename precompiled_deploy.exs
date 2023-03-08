app_priv = "#{Mix.Project.app_path(Mix.Project.config())}/priv"
File.mkdir_p(app_priv)
precompiled_priv = System.get_env("TFLITE_ELIXIR_ONLY_COPY_PRIV", nil)
if precompiled_priv != nil do
  File.cp_r!(precompiled_priv, app_priv, fn _, _ ->
    false
  end)
end

libedgetpu_dir = Path.join([app_priv, "libedgetpu"])
symlinks =
  case :os.type() do
    {:unix, :darwin} ->
      [
        {"libedgetpu.1.0.dylib", "libedgetpu.1.dylib"},
        {"libedgetpu.1.dylib", "libedgetpu.dylib"},
      ]
    {:unix, _} ->
      [
        {"libedgetpu.so.1.0", "libedgetpu.so.1"},
        {"libedgetpu.so.1", "libedgetpu.so"},
      ]
  end

saved_cwd = File.cwd!()
File.cd!(libedgetpu_dir)

Enum.each(symlinks, fn {original, symlink} ->
  File.rm_rf!(symlink)
  File.ln_s!(original, symlink)
end)

File.cd!(saved_cwd)

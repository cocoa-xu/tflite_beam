app_priv = "#{Mix.Project.app_path(Mix.Project.config())}/priv"
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
        {"lib/libusb-1.0.0.dylib", "libusb-1.0.0.dylib"}
      ]
    {:unix, _} ->
      [
        {"libedgetpu.so.1.0", "libedgetpu.so.1"},
        {"libedgetpu.so.1", "libedgetpu.so"},
        {"libusb-1.0.so.0.3.0", "lib/libusb-1.0.so.0"},
        {"lib/libusb-1.0.so.0", "libusb-1.0.so.0"}
      ]
  end

saved_cwd = File.cwd!()
File.cd!(libedgetpu_dir)

Enum.each(symlinks, fn {original, symlink} ->
  File.rm_rf!(symlink)
  File.ln_s!(original, symlink)
end)

File.cd!(saved_cwd)

app_priv = "#{Mix.Project.app_path(Mix.Project.config())}/priv"
File.mkdir_p(app_priv)
precompiled_priv = System.get_env("TFLITE_ELIXIR_ONLY_COPY_PRIV", nil)
if precompiled_priv != nil do
  File.cp_r!(precompiled_priv, app_priv, fn _, _ ->
    false
  end)
end

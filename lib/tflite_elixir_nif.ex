defmodule TFLite.Nif do
  @moduledoc false
  @on_load :load_nif
  def load_nif do
    require Logger
    nif_file = '#{:code.priv_dir(:tflite_elixir)}/tflite_elixir'

    case :erlang.load_nif(nif_file, 0) do
      :ok -> :ok
      {:error, {:reload, _}} -> :ok
      {:error, reason} -> Logger.warn("Failed to load nif: #{inspect(reason)}")
    end
  end

  def flatBufferModel_buildFromFile(_filename), do: :erlang.nif_error(:not_loaded)
  def ops_builtin_builtinResolver_new(), do: :erlang.nif_error(:not_loaded)
  def interpreterBuilder_new(_model, _resolver), do: :erlang.nif_error(:not_loaded)
  def interpreterBuilder_build(_self, _builder), do: :erlang.nif_error(:not_loaded)
  def interpreter_new(), do: :erlang.nif_error(:not_loaded)
  def interpreter_allocateTensors(_self), do: :erlang.nif_error(:not_loaded)
  def tflite_printInterpreterState(_interpreter), do: :erlang.nif_error(:not_loaded)
end

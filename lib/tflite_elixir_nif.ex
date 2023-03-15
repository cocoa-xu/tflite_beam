defmodule TFLiteElixir.Nif do
  @moduledoc false
  @on_load :load_nif
  def load_nif do
    require Logger
    nif_file = ~c"#{:code.priv_dir(:tflite_elixir)}/tflite_elixir"

    case :erlang.load_nif(nif_file, 0) do
      :ok -> :ok
      {:error, {:reload, _}} -> :ok
      {:error, reason} -> Logger.warn("Failed to load nif: #{inspect(reason)}")
    end
  end

  def errorReporter_DefaultErrorReporter(), do: :erlang.nif_error(:not_loaded)

  def flatBufferModel_buildFromFile(_filename, _error_reporter), do: :erlang.nif_error(:not_loaded)
  def flatBufferModel_verifyAndBuildFromFile(_filename, _extra_verifier, _error_reporter), do: :erlang.nif_error(:not_loaded)
  def flatBufferModel_buildFromBuffer(_buffer), do: :erlang.nif_error(:not_loaded)
  def flatBufferModel_initialized(_self), do: :erlang.nif_error(:not_loaded)
  def flatBufferModel_getMinimumRuntime(_self), do: :erlang.nif_error(:not_loaded)
  def flatBufferModel_readAllMetadata(_self), do: :erlang.nif_error(:not_loaded)

  def ops_builtin_builtinResolver_new(), do: :erlang.nif_error(:not_loaded)

  def interpreterBuilder_new(_model, _resolver), do: :erlang.nif_error(:not_loaded)
  def interpreterBuilder_build(_self, _builder), do: :erlang.nif_error(:not_loaded)
  def interpreterBuilder_setNumThreads(_self, _num_threads), do: :erlang.nif_error(:not_loaded)

  def interpreter_new(), do: :erlang.nif_error(:not_loaded)
  def interpreter_allocateTensors(_self), do: :erlang.nif_error(:not_loaded)
  def interpreter_inputs(_self), do: :erlang.nif_error(:not_loaded)
  def interpreter_getInputName(_self, _index), do: :erlang.nif_error(:not_loaded)
  def interpreter_input_tensor(_self, _index, _data), do: :erlang.nif_error(:not_loaded)
  def interpreter_invoke(_self), do: :erlang.nif_error(:not_loaded)
  def interpreter_outputs(_self), do: :erlang.nif_error(:not_loaded)
  def interpreter_getOutputName(_self, _index), do: :erlang.nif_error(:not_loaded)
  def interpreter_output_tensor(_self, _index), do: :erlang.nif_error(:not_loaded)
  def interpreter_tensor(_self, _tensor_index), do: :erlang.nif_error(:not_loaded)
  def interpreter_setNumThreads(_self, _num_threads), do: :erlang.nif_error(:not_loaded)
  def interpreter_get_signature_defs(_self), do: :erlang.nif_error(:not_loaded)

  def tflitetensor_type(_self), do: :erlang.nif_error(:not_loaded)
  def tflitetensor_dims(_self), do: :erlang.nif_error(:not_loaded)
  def tflitetensor_quantization_params(_self), do: :erlang.nif_error(:not_loaded)
  def tflitetensor_to_binary(_self, _limit), do: :erlang.nif_error(:not_loaded)
  def tflitetensor_set_data(_self, _data), do: :erlang.nif_error(:not_loaded)

  def tflite_printInterpreterState(_interpreter), do: :erlang.nif_error(:not_loaded)
  def tflite_resetVariableTensor(_tflite_tensor), do: :erlang.nif_error(:not_loaded)

  # ================ Coral ===================
  def coral_contains_edgetpu_custom_op(_model), do: :erlang.nif_error(:not_loaded)
  def coral_edgetpu_devices(), do: :erlang.nif_error(:not_loaded)
  def coral_get_edgetpu_context(_device, _options), do: :erlang.nif_error(:not_loaded)
  def coral_make_edgetpu_interpreter(_model, _context), do: :erlang.nif_error(:not_loaded)

  def coral_dequantize_tensor(_interpreter, _tensor_index, _as_type),
    do: :erlang.nif_error(:not_loaded)
end

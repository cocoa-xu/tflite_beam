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

  def error_reporter_default_error_reporter(), do: :erlang.nif_error(:not_loaded)

  def flatbuffer_model_build_from_file(_filename, _error_reporter),
    do: :erlang.nif_error(:not_loaded)

  def flatbuffer_model_verify_and_build_from_file(_filename, _error_reporter),
    do: :erlang.nif_error(:not_loaded)

  def flatbuffer_model_build_from_buffer(_buffer, _error_reporter),
    do: :erlang.nif_error(:not_loaded)

  def flatbuffer_model_initialized(_self), do: :erlang.nif_error(:not_loaded)
  def flatbuffer_model_error_reporter(_self), do: :erlang.nif_error(:not_loaded)
  def flatbuffer_model_get_minimum_runtime(_self), do: :erlang.nif_error(:not_loaded)
  def flatbuffer_model_read_all_metadata(_self), do: :erlang.nif_error(:not_loaded)

  def ops_builtin_builtin_resolver_new(), do: :erlang.nif_error(:not_loaded)

  def interpreter_builder_new(_model, _resolver), do: :erlang.nif_error(:not_loaded)
  def interpreter_builder_build(_self, _builder), do: :erlang.nif_error(:not_loaded)
  def interpreter_builder_set_num_threads(_self, _num_threads), do: :erlang.nif_error(:not_loaded)

  def interpreter_new(), do: :erlang.nif_error(:not_loaded)
  def interpreter_set_inputs(_self, _inputs), do: :erlang.nif_error(:not_loaded)
  def interpreter_set_outputs(_self, _outputs), do: :erlang.nif_error(:not_loaded)
  def interpreter_allocate_tensors(_self), do: :erlang.nif_error(:not_loaded)
  def interpreter_inputs(_self), do: :erlang.nif_error(:not_loaded)
  def interpreter_get_input_name(_self, _index), do: :erlang.nif_error(:not_loaded)
  def interpreter_input_tensor(_self, _index, _data), do: :erlang.nif_error(:not_loaded)
  def interpreter_invoke(_self), do: :erlang.nif_error(:not_loaded)
  def interpreter_outputs(_self), do: :erlang.nif_error(:not_loaded)
  def interpreter_get_output_name(_self, _index), do: :erlang.nif_error(:not_loaded)
  def interpreter_output_tensor(_self, _index), do: :erlang.nif_error(:not_loaded)
  def interpreter_tensor(_self, _tensor_index), do: :erlang.nif_error(:not_loaded)
  def interpreter_set_num_threads(_self, _num_threads), do: :erlang.nif_error(:not_loaded)
  def interpreter_get_signature_defs(_self), do: :erlang.nif_error(:not_loaded)

  def tflitetensor_type(_self), do: :erlang.nif_error(:not_loaded)
  def tflitetensor_dims(_self), do: :erlang.nif_error(:not_loaded)
  def tflitetensor_quantization_params(_self), do: :erlang.nif_error(:not_loaded)
  def tflitetensor_to_binary(_self, _limit), do: :erlang.nif_error(:not_loaded)
  def tflitetensor_set_data(_self, _data), do: :erlang.nif_error(:not_loaded)

  def tflite_print_interpreter_state(_interpreter), do: :erlang.nif_error(:not_loaded)
  def tflite_reset_variable_tensor(_tflite_tensor), do: :erlang.nif_error(:not_loaded)

  # ================ Coral ===================
  def coral_contains_edgetpu_custom_op(_model), do: :erlang.nif_error(:not_loaded)
  def coral_edgetpu_devices(), do: :erlang.nif_error(:not_loaded)
  def coral_get_edgetpu_context(_device, _options), do: :erlang.nif_error(:not_loaded)
  def coral_make_edgetpu_interpreter(_model, _context), do: :erlang.nif_error(:not_loaded)

  def coral_dequantize_tensor(_interpreter, _tensor_index, _as_type),
    do: :erlang.nif_error(:not_loaded)
end

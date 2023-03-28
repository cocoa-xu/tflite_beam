-module(tflite_beam_nif).
-compile(nowarn_export_all).
-compile([export_all]).

-on_load(init/0).

-define(APPNAME, tflite_beam).
-define(LIBNAME, tflite_beam).

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).


error_reporter_default_error_reporter() ->
    not_loaded(?LINE).


flatbuffer_model_build_from_file(_filename, _error_reporter) ->
    not_loaded(?LINE).

flatbuffer_model_verify_and_build_from_file(_filename, _error_reporter) ->
    not_loaded(?LINE).

flatbuffer_model_build_from_buffer(_buffer, _error_reporter) ->
    not_loaded(?LINE).

flatbuffer_model_initialized(_self) ->
    not_loaded(?LINE).

flatbuffer_model_error_reporter(_self) ->
    not_loaded(?LINE).

flatbuffer_model_get_minimum_runtime(_self) ->
    not_loaded(?LINE).

flatbuffer_model_read_all_metadata(_self) ->
    not_loaded(?LINE).


ops_builtin_builtin_resolver_new() ->
    not_loaded(?LINE).


interpreter_builder_new(_model, _resolver) ->
    not_loaded(?LINE).

interpreter_builder_build(_self, _builder) ->
    not_loaded(?LINE).

interpreter_builder_set_num_threads(_self, _num_threads) ->
    not_loaded(?LINE).


interpreter_new() ->
    not_loaded(?LINE).

interpreter_set_inputs(_self, _inputs) ->
    not_loaded(?LINE).

interpreter_set_outputs(_self, _outputs) ->
    not_loaded(?LINE).

interpreter_set_variables(_self, _variables) ->
    not_loaded(?LINE).

interpreter_inputs(_self) ->
    not_loaded(?LINE).

interpreter_get_input_name(_self, _index) ->
    not_loaded(?LINE).

interpreter_outputs(_self) ->
    not_loaded(?LINE).

interpreter_variables(_self) ->
    not_loaded(?LINE).

interpreter_get_output_name(_self, _index) ->
    not_loaded(?LINE).

interpreter_tensors_size(_self) ->
    not_loaded(?LINE).

interpreter_nodes_size(_self) ->
    not_loaded(?LINE).

interpreter_execution_plan(_self) ->
    not_loaded(?LINE).

interpreter_tensor(_self, _tensor_index) ->
    not_loaded(?LINE).

interpreter_signature_keys(_self) ->
    not_loaded(?LINE).

interpreter_input_tensor(_self, _index, _data) ->
    not_loaded(?LINE).

interpreter_output_tensor(_self, _index) ->
    not_loaded(?LINE).

interpreter_allocate_tensors(_self) ->
    not_loaded(?LINE).

interpreter_invoke(_self) ->
    not_loaded(?LINE).

interpreter_set_num_threads(_self, _num_threads) ->
    not_loaded(?LINE).

interpreter_get_signature_defs(_self) ->
    not_loaded(?LINE).


tflitetensor_type(_self) ->
    not_loaded(?LINE).

tflitetensor_dims(_self) ->
    not_loaded(?LINE).

tflitetensor_quantization_params(_self) ->
    not_loaded(?LINE).

tflitetensor_to_binary(_self, _limit) ->
    not_loaded(?LINE).

tflitetensor_set_data(_self, _data) ->
    not_loaded(?LINE).


tflite_print_interpreter_state(_interpreter) ->
    not_loaded(?LINE).

tflite_reset_variable_tensor(_tflite_tensor) ->
    not_loaded(?LINE).


coral_contains_edgetpu_custom_op(_model) ->
    not_loaded(?LINE).

coral_edgetpu_devices() ->
    not_loaded(?LINE).

coral_get_edgetpu_context(_device, _options) ->
    not_loaded(?LINE).

coral_make_edgetpu_interpreter(_model, _context) ->
    not_loaded(?LINE).

coral_dequantize_tensor(_interpreter, _tensor_index, _as_type) ->
    not_loaded(?LINE).

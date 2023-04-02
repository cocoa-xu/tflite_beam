%% @doc
%% A typed multi-dimensional array used in Tensorflow Lite.

-module(tflite_beam_tensor).
-export([
    type/1,
    dims/1,
    shape/1,
    quantization_params/1,
    set_data/2,
    to_binary/1,
    to_binary/2
]).

-include("tflite_beam_records.hrl").

%% @doc Get the data type
-spec type(#tflite_beam_tensor{} | reference()) -> tflite_beam_tensor_type() | {error, binary()}.
type(#tflite_beam_tensor{type = Type}) ->
    Type;
type(Self) when is_reference(Self) ->
    tflite_beam_nif:tflitetensor_type(Self).

%% @doc
%% Get the dimensions (C++) API
-spec dims(#tflite_beam_tensor{} | reference()) -> list(integer()) | {error, binary()}.
dims(#tflite_beam_tensor{shape = Shape}) when is_tuple(Shape) ->
    [element(I,Shape) || I <- lists:seq(1,tuple_size(Shape))];
dims(Self) when is_reference(Self) ->
    case tflite_beam_nif:tflitetensor_dims(Self) of
        {ok, Dims} ->
            Dims;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Get the tensor shape
-spec shape(#tflite_beam_tensor{} | reference()) -> tuple() | {error, binary()}.
shape(#tflite_beam_tensor{shape = Shape}) when is_tuple(Shape) ->
    Shape;
shape(Self) when is_reference(Self) ->
    case tflite_beam_nif:tflitetensor_dims(Self) of
        {ok, Dims} ->
            list_to_tuple(Dims);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Get the quantization params
-spec quantization_params(#tflite_beam_tensor{} | reference()) -> #tflite_beam_quantization_params{} | {error, binary()}.
quantization_params(#tflite_beam_tensor{quantization_params = QuantizationParams}) ->
    QuantizationParams;
quantization_params(Self) when is_reference(Self) ->
    case tflite_beam_nif:tflitetensor_quantization_params(Self) of
        {ok, {Scale, ZeroPoint, QuantizedDimension}} ->
            #tflite_beam_quantization_params{scale = Scale, zero_point = ZeroPoint, quantized_dimension = QuantizedDimension};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Set tensor data
-spec set_data(#tflite_beam_tensor{} | reference(), binary()) -> ok | {error,  binary()}.
set_data(#tflite_beam_tensor{ref = Self}, Data) when is_reference(Self) and is_binary(Data) ->
    tflite_beam_nif:tflitetensor_set_data(Self, Data);
set_data(Self, Data) when is_reference(Self) and is_binary(Data) ->
    tflite_beam_nif:tflitetensor_set_data(Self, Data).

%% @doc Get binary data
-spec to_binary(#tflite_beam_tensor{} | reference()) -> binary() | {error, binary()}.
to_binary(#tflite_beam_tensor{ref = Self}) when is_reference(Self) ->
    to_binary(Self, 0);
to_binary(Self) when is_reference(Self) ->
    to_binary(Self, 0).

%% @doc Get binary data
-spec to_binary(#tflite_beam_tensor{} | reference(), non_neg_integer()) -> binary() | {error, binary()}.
to_binary(#tflite_beam_tensor{ref = Self}, MaxBytes) when is_reference(Self) and is_integer(MaxBytes) ->
    to_binary_impl(Self, MaxBytes);
to_binary(Self, MaxBytes) when is_reference(Self) ->
    to_binary_impl(Self, MaxBytes).

to_binary_impl(Self, MaxBytes) ->
    case tflite_beam_nif:tflitetensor_to_binary(Self, MaxBytes) of
        {ok, BinaryData} ->
            BinaryData;
        {error, Reason} ->
            {error, Reason}
    end.

%% @moduledoc
%% This module contains libcoral C++ API, which provides
%% convenient functions to perform inferencing and on-device transfer learning
%% with TensorFlow Lite models on [Coral devices](https://coral.ai/products/).

-module(tflite_beam_coral).
-export([
    'contains_edge_tpu_custom_op?'/1,
    edge_tpu_devices/0,
    get_edge_tpu_context/0,
    get_edge_tpu_context/1,
    make_edge_tpu_interpreter/2,
    dequantize_tensor/2,
    dequantize_tensor/3
]).

-include("tflite_beam_records.hrl").

%% @doc
%% Checks whether a tflite model contains any Edge TPU custom operator.
-spec 'contains_edge_tpu_custom_op?'(#tflite_beam_flatbuffer_model{} | reference()) -> boolean() | {error, binary()}.
'contains_edge_tpu_custom_op?'(#tflite_beam_flatbuffer_model{ref = Model}) when is_reference(Model) ->
    'contains_edge_tpu_custom_op?'(Model);
'contains_edge_tpu_custom_op?'(Model) when is_reference(Model) ->
    tflite_beam_nif:coral_contains_edgetpu_custom_op(Model).

%% @doc
%% Returns a list of connected edge TPU devices.
-spec edge_tpu_devices() -> [binary()] | {error, binary()}.
edge_tpu_devices() ->
    tflite_beam_nif:coral_edgetpu_devices().

%% @doc
%%
%% Returns TPU context or an error-tuple if requested TPU context is not available.
-spec get_edge_tpu_context() -> {ok, reference()} | {error, binary()}.
get_edge_tpu_context() ->
    get_edge_tpu_context([]).

%% @doc
%%
%% Returns TPU context or an error-tuple if requested TPU context is not available.
%% 
%% ==== Keyword Parameters ====
%% - `device': `binary()'. Possible values are
%%
%%  - ""      -- any TPU device
%%  - "usb"   -- any TPU device on USB bus
%%  - "pci"   -- any TPU device on PCIe bus
%%  - ":N"    -- N-th TPU device, e.g. ":0"
%%  - "usb:N" -- N-th TPU device on USB bus, e.g. "usb:0"
%%  - "pci:N" -- N-th TPU device on PCIe bus, e.g. "pci:0"
%%
%%  Default value is `""'.
%%
%%  Consider 2 USB devices and 4 PCIe devices connected to the host. The way to
%%  reference specifically USB devices:
%%
%%    "usb:0", "usb:1".
%%
%%  The way to reference specifically PCIe devices:
%%
%%    "pci:0", "pci:1", "pci:2", "pci:3".
%%
%%  The generic way to reference all devices (no assumption about device type):
%%
%%    ":0", ":1", ":2", ":3", ":4", ":5".
%%
%% - `options': `map()'. Possible key-value pairs are
%%
%%  - "Performance": `binary()'
%%
%%    - "Low"
%%    - "Medium"
%%    - "High"
%%    - "Max"
%%
%%    Default is "Max".
%%
%%    Adjust internal clock rate to achieve different performance / power balance.
%%
%%  - "Usb.AlwaysDfu": `boolean'
%%
%%    - `true'
%%    - `false'
%%
%%    Default is `false'.
%%
%%    Always perform device firmware update after reset. DFU is usually only
%%    necessary after power cycle.
%%
%%  - "Usb.MaxBulkInQueueLength": `binary()'
%%
%%    - ["0",.., "255"] (Default is "32")
%%
%%    Larger queue length may improve USB performance on the direction from
%%    device to host.
%%
%%    All TPUs are always enumerated in the same order assuming hardware
%%    configuration does not change (no added/removed devices between enumerations).
%%    Under the assumption above, the same index N will always point to the same
%%    device.
-spec get_edge_tpu_context(list()) -> {ok, reference()} | {error, binary()}.
get_edge_tpu_context(Opts) when is_list(Opts) ->
    Device = proplists:get_value(device, Opts, <<"">>),
    Options = proplists:get_value(options, Opts, #{}),
    tflite_beam_nif:coral_get_edgetpu_context(Device, Options).

%% @doc
%%
%% Creates a new interpreter instance for an Edge TPU model.
%%
%% ==== Positional Parameters ====
%% - `model': `#tflite_beam_flatbuffer_model{}'. The tflite model.
%% - `edgetpu_context': `reference()'.
%%
%%  The Edge TPU context, from `get_edge_tpu_context/1'.
%%
%%  If left `nil', the given interpreter will not resolve an Edge TPU delegate.
%%  PoseNet custom op is always supported.
-spec make_edge_tpu_interpreter(#tflite_beam_flatbuffer_model{} | reference(), reference()) -> {ok, reference()} | {error, binary()}.
make_edge_tpu_interpreter(#tflite_beam_flatbuffer_model{ref = Model}, EdgeTPUContext) when is_reference(Model) and is_reference(EdgeTPUContext) ->
    make_edge_tpu_interpreter(Model, EdgeTPUContext);
make_edge_tpu_interpreter(Model, EdgeTPUContext) when is_reference(Model) and is_reference(EdgeTPUContext) ->
    tflite_beam_nif:coral_make_edgetpu_interpreter(Model, EdgeTPUContext).

%% @doc
%% Returns a dequantized version of the given tensor.
-spec dequantize_tensor(reference(), non_neg_integer()) -> list(number()) | {error, binary()}.
dequantize_tensor(Interpreter, TensorIndex) when is_reference(Interpreter) and is_integer(TensorIndex) ->
    dequantize_tensor(Interpreter, TensorIndex, nil).

%% @doc
%% Returns a dequantized version of the given tensor.
-spec dequantize_tensor(reference(), non_neg_integer(), {u | s, 8 | 16 | 32 | 64} | {f, 32 | 64} | s8 | s16 | s32 | s64 | u8 | u16 | u32 | u64 | f32 | f64 | nil) -> list(number()) | {error, binary()}.
dequantize_tensor(Interpreter, TensorIndex, AsType) when is_reference(Interpreter) and is_integer(TensorIndex) ->
    case map_type(AsType) of
        MappedType when is_atom(MappedType) ->
            tflite_beam_nif:coral_dequantize_tensor(Interpreter, TensorIndex, MappedType);
        {error, Reason} ->
            {error, Reason}
    end.

map_type({f, 32}) ->
    f32;
map_type({f, 64}) ->
    f64;
map_type({u, 8}) ->
    u8;
map_type({u, 16}) ->
    u16;
map_type({u, 32}) ->
    u32;
map_type({u, 64}) ->
    u64;
map_type({s, 8}) ->
    s8;
map_type({s, 16}) ->
    s16;
map_type({s, 32}) ->
    s32;
map_type({s, 64}) ->
    s64;
map_type(Atom) when is_atom(Atom) ->
    Valid = lists:member(Atom, [s8, s16, s32, s64, u8, u16, u32, u64, f32, f64, nil]),
    if Valid ->
        Atom;
    true ->
        Reason = io:format("Invalid type `~p`", [Atom]),
        {error, unicode:iolist_to_binary(Reason)}
    end.

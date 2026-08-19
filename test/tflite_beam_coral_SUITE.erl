%% @doc The Edge TPU reached the same way as any other delegate. The cases that
%% need a device and a compiled model live in the require_tpu group, which is
%% not in all/0 -- run it with `rebar3 ct --group require_tpu'.
-module(tflite_beam_coral_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0]).
-export([
    edge_tpu_plugin_symbols/1,
    edge_tpu_default_path/1,
    edge_tpu_absent_device/1,
    edge_tpu_delegate_inference/1,
    edge_tpu_delegate_composes/1
]).

all() ->
    [
        edge_tpu_plugin_symbols,
        edge_tpu_default_path,
        edge_tpu_absent_device
    ].

groups() ->
    [{require_tpu, [], [edge_tpu_delegate_inference, edge_tpu_delegate_composes]}].

%% The fact the whole phase rests on: libedgetpu is already a TfLite delegate
%% plugin. If it is ever rebuilt without those exports, this fails first and
%% says why.
edge_tpu_plugin_symbols(_Config) ->
    with_bundled_runtime(fun(Path) ->
        case symbol_reader() of
            false ->
                {skip, "neither nm nor objdump is available"};
            Reader ->
                Symbols = os:cmd(Reader ++ " " ++ binary_to_list(Path) ++ " 2>/dev/null"),
                ?assertNotEqual(nomatch, string:find(Symbols, "tflite_plugin_create_delegate")),
                ?assertNotEqual(nomatch, string:find(Symbols, "tflite_plugin_destroy_delegate"))
        end
    end).

edge_tpu_default_path(_Config) ->
    with_bundled_runtime(fun(Path) ->
        ?assert(filelib:is_regular(Path))
    end).

%% Asking for a device that is not there has to be an ordinary error. This is
%% the exact shape that segfaults through TfLite's own external-delegate
%% constructor, which hands back a delegate whose Prepare is indeterminate when
%% the plugin declines.
edge_tpu_absent_device(_Config) ->
    with_bundled_runtime(fun(_Path) ->
        %% no tenth USB TPU exists anywhere, with or without hardware attached
        Result = tflite_beam_coral:edge_tpu_delegate(#{device => <<"usb:9">>}),
        ?assertMatch({error, _}, Result),
        {error, Reason} = Result,
        ?assertNotEqual(nomatch, binary:match(Reason, <<"declined to create a delegate">>))
    end).

%% The acceptance criterion: the delegate route and the existing
%% make_edge_tpu_interpreter/2 route agree, on a real device, byte for byte.
edge_tpu_delegate_inference(_Config) ->
    with_hardware(fun(Model) ->
        Existing = through_edge_tpu_context(Model),
        Delegated = through_delegate(Model, fun(_Builder) -> ok end),
        ?assertEqual(Existing, Delegated),
        %% and it is real output rather than two identically empty runs
        ?assert(lists:any(fun(Byte) -> Byte =/= 0 end, binary_to_list(hd(Delegated))))
    end).

%% What the existing route cannot do: it builds its own interpreter internally,
%% so the builder's settings never reach it.
edge_tpu_delegate_composes(_Config) ->
    with_hardware(fun(Model) ->
        Threaded = through_delegate(Model, fun(Builder) ->
            ok = tflite_beam_interpreter_builder:set_num_threads(Builder, 2)
        end),
        ?assertEqual(through_edge_tpu_context(Model), Threaded)
    end).

through_edge_tpu_context(Model) ->
    Loaded = tflite_beam_flatbuffer_model:build_from_file(Model),
    {ok, Context} = tflite_beam_coral:get_edge_tpu_context(),
    {ok, Interpreter} = tflite_beam_coral:make_edge_tpu_interpreter(element(4, Loaded), Context),
    ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
    tflite_beam_interpreter:predict(Interpreter, model_input()).

through_delegate(Model, Configure) ->
    {ok, Resolver} = tflite_beam_ops_builtin_builtin_resolver:new(),
    Loaded = tflite_beam_flatbuffer_model:build_from_file(Model),
    {ok, Builder} = tflite_beam_interpreter_builder:new(element(4, Loaded), Resolver),
    {ok, Delegate} = tflite_beam_coral:edge_tpu_delegate(),
    ok = tflite_beam_interpreter_builder:add_delegate(Builder, Delegate),
    ok = Configure(Builder),
    {ok, Interpreter} = tflite_beam_interpreter:new(),
    ok = tflite_beam_interpreter_builder:build(Builder, Interpreter),
    ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
    tflite_beam_interpreter:predict(Interpreter, model_input()).

model_input() ->
    binary:copy(<<7>>, 224 * 224 * 3).

with_bundled_runtime(Body) ->
    case tflite_beam_coral:default_libedgetpu_path() of
        {error, _} -> {skip, "this build bundles no libedgetpu runtime"};
        Path -> Body(Path)
    end.

%% The model is 4 MB and lives in tflite_elixir; naming it through the
%% environment keeps it out of a repository whose other fixtures are kilobytes.
with_hardware(Body) ->
    case {tflite_beam_coral:edge_tpu_devices(), os:getenv("TFLITE_BEAM_TEST_EDGETPU_MODEL")} of
        {[], _} ->
            {skip, "no Edge TPU device is attached"};
        {_, false} ->
            {skip, "TFLITE_BEAM_TEST_EDGETPU_MODEL is not set"};
        {_, Model} ->
            case filelib:is_regular(Model) of
                true -> Body(Model);
                false -> {skip, "TFLITE_BEAM_TEST_EDGETPU_MODEL does not name a file"}
            end
    end.

symbol_reader() ->
    case [Tool || Tool <- ["nm -g", "objdump -T"], os:find_executable(hd(string:split(Tool, " "))) =/= false] of
        [Reader | _] -> Reader;
        [] -> false
    end.

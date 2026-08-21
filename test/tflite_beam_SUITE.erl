%% @doc The existing API, plus the baseline that pins how much of a model
%% XNNPACK claims for itself today.
-module(tflite_beam_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../src/tflite_beam/tflite_beam_records.hrl").

-export([all/0]).
-export([
    model_from_file/1,
    model_from_buffer/1,
    model_from_missing_file/1,
    model_from_invalid_buffer/1,
    interpreter_from_builder/1,
    interpreter_tensor_metadata/1,
    tensor_accessors_take_the_record/1,
    build_version_is_injected/1,
    interpreter_invoke/1,
    interpreter_predict/1,
    signature_runner_invoke/1,
    execution_plan_baseline/1,
    quantized_model_is_not_delegated/1
]).

%% every tensor in multi_add.bin is a [1, 8, 8, 3] float32
-define(FILLED(V), binary:copy(<<V:32/float-native>>, 1 * 8 * 8 * 3)).

all() ->
    [
        model_from_file,
        model_from_buffer,
        model_from_missing_file,
        model_from_invalid_buffer,
        interpreter_from_builder,
        interpreter_tensor_metadata,
        tensor_accessors_take_the_record,
        build_version_is_injected,
        interpreter_invoke,
        interpreter_predict,
        signature_runner_invoke,
        execution_plan_baseline,
        quantized_model_is_not_delegated
    ].

model_from_file(_Config) ->
    Model = tflite_beam_flatbuffer_model:build_from_file(tflite_beam_test_models:path("add.bin")),
    ?assertMatch(#tflite_beam_flatbuffer_model{initialized = true}, Model),
    ?assert(is_reference(Model#tflite_beam_flatbuffer_model.ref)).

model_from_buffer(_Config) ->
    {ok, Buffer} = file:read_file(tflite_beam_test_models:path("add.bin")),
    ?assertMatch(#tflite_beam_flatbuffer_model{initialized = true},
                 tflite_beam_flatbuffer_model:build_from_buffer(Buffer)),
    ?assertMatch(#tflite_beam_flatbuffer_model{initialized = true},
                 tflite_beam_flatbuffer_model:verify_and_build_from_buffer(Buffer)).

model_from_missing_file(_Config) ->
    ?assertMatch({error, _},
                 tflite_beam_flatbuffer_model:build_from_file(
                     tflite_beam_test_models:path("no_such_model.bin"))).

model_from_invalid_buffer(_Config) ->
    ?assertEqual(invalid,
                 tflite_beam_flatbuffer_model:verify_and_build_from_buffer(<<"not a flatbuffer">>)).

interpreter_from_builder(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("multi_add.bin"),
    ?assertEqual(7, tflite_beam_interpreter:tensors_size(Interpreter)),
    ?assertEqual({ok, [0, 1, 2, 3]}, tflite_beam_interpreter:inputs(Interpreter)),
    ?assertEqual({ok, [5, 6]}, tflite_beam_interpreter:outputs(Interpreter)),
    ?assertEqual({ok, <<"a">>}, tflite_beam_interpreter:get_input_name(Interpreter, 0)),
    ?assertEqual({ok, <<"x">>}, tflite_beam_interpreter:get_output_name(Interpreter, 0)),
    ?assertEqual([<<"serving_default">>], tflite_beam_interpreter:signature_keys(Interpreter)).

interpreter_tensor_metadata(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("multi_add.bin"),
    Tensor = tflite_beam_interpreter:tensor(Interpreter, 0),
    ?assertMatch(#tflite_beam_tensor{name = <<"a">>, index = 0, type = {f, 32}}, Tensor),
    ?assertEqual([1, 8, 8, 3], tflite_beam_tensor:dims(Tensor#tflite_beam_tensor.ref)),
    ?assertEqual({f, 32}, tflite_beam_tensor:type(Tensor#tflite_beam_tensor.ref)),
    ?assert(is_binary(tflite_beam_tensor:to_binary(Tensor))),
    %% a handle borrows the interpreter's memory and does not keep it alive, so
    %% the interpreter has to still be reachable here or the reads above race a
    %% GC that would invalidate the handle. See
    %% tflite_beam_build_SUITE:tensor_handle_does_not_outlive_its_interpreter/1
    ?assertEqual(7, tflite_beam_interpreter:tensors_size(Interpreter)).

%% dims/1 and shape/1 accept the record as well as the handle. Their record
%% clauses guarded on is_tuple while the NIF fills the field with a list, so
%% they raised function_clause for every caller; only the handle path was ever
%% covered here.
tensor_accessors_take_the_record(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("multi_add.bin"),
    Tensor = tflite_beam_interpreter:tensor(Interpreter, 0),
    Ref = Tensor#tflite_beam_tensor.ref,
    ?assertEqual([1, 8, 8, 3], tflite_beam_tensor:dims(Tensor)),
    ?assertEqual({1, 8, 8, 3}, tflite_beam_tensor:shape(Tensor)),
    ?assertEqual(tflite_beam_tensor:dims(Ref), tflite_beam_tensor:dims(Tensor)),
    ?assertEqual(tflite_beam_tensor:shape(Ref), tflite_beam_tensor:shape(Tensor)),
    ?assertEqual(7, tflite_beam_interpreter:tensors_size(Interpreter)).

%% tflite_version/0 is what a delegate plugin gets matched against, and it comes
%% from TFLITE_VER injected at compile time rather than from the runtime, whose
%% own number is a hand-maintained constant upstream forgets to bump. This pins
%% the whole path -- Makefile to cmake to macro to NIF -- because every link in
%% it is silent when it breaks: a stale or missing value still returns a string.
build_version_is_injected(_Config) ->
    Version = tflite_beam:tflite_version(),
    ?assertNotEqual(<<"unknown">>, Version),
    ?assertEqual(makefile_tflite_ver(), Version),
    ?assert(is_binary(tflite_beam:tflite_runtime_version())),
    ?assert(is_binary(tflite_beam:tflite_extension_apis_version())),
    ?assert(tflite_beam:tflite_schema_version() > 0).

makefile_tflite_ver() ->
    Root = filename:join(code:lib_dir(tflite_beam), filename:join(lists:duplicate(4, ".."))),
    Makefile = case filelib:is_regular(filename:join(Root, "Makefile")) of
        true -> filename:join(Root, "Makefile");
        false -> "Makefile"
    end,
    {ok, Contents} = file:read_file(Makefile),
    [_, Rest] = binary:split(Contents, <<"\nTFLITE_VER ?= ">>),
    [Version | _] = binary:split(Rest, <<"\n">>),
    Version.

interpreter_invoke(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("multi_add.bin"),
    ok = tflite_beam_interpreter:input_tensor(Interpreter, 0, ?FILLED(1.0)),
    ok = tflite_beam_interpreter:input_tensor(Interpreter, 1, ?FILLED(2.0)),
    ok = tflite_beam_interpreter:input_tensor(Interpreter, 2, ?FILLED(3.0)),
    ok = tflite_beam_interpreter:input_tensor(Interpreter, 3, ?FILLED(4.0)),
    ok = tflite_beam_interpreter:invoke(Interpreter),
    %% x = a + (b + c), y = d + (b + c)
    ?assertEqual({ok, ?FILLED(6.0)}, tflite_beam_interpreter:output_tensor(Interpreter, 0)),
    ?assertEqual({ok, ?FILLED(9.0)}, tflite_beam_interpreter:output_tensor(Interpreter, 1)).

interpreter_predict(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("multi_add.bin"),
    ?assertEqual([?FILLED(6.0), ?FILLED(9.0)],
                 tflite_beam_interpreter:predict(
                     Interpreter, [?FILLED(1.0), ?FILLED(2.0), ?FILLED(3.0), ?FILLED(4.0)])).

signature_runner_invoke(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("multi_add.bin"),
    {ok, Runner} = tflite_beam_interpreter:get_signature_runner(Interpreter, <<"serving_default">>),
    ?assertEqual({ok, <<"serving_default">>}, tflite_beam_signature_runner:signature_key(Runner)),
    ?assertEqual({ok, 4}, tflite_beam_signature_runner:input_size(Runner)),
    {ok, InputNames} = tflite_beam_signature_runner:input_names(Runner),
    ?assertEqual([<<"a">>, <<"b">>, <<"c">>, <<"d">>], lists:sort(InputNames)),
    ok = tflite_beam_signature_runner:allocate_tensors(Runner),
    ok = tflite_beam_signature_runner:input_tensor(Runner, <<"a">>, ?FILLED(1.0)),
    ok = tflite_beam_signature_runner:input_tensor(Runner, <<"b">>, ?FILLED(2.0)),
    ok = tflite_beam_signature_runner:input_tensor(Runner, <<"c">>, ?FILLED(3.0)),
    ok = tflite_beam_signature_runner:input_tensor(Runner, <<"d">>, ?FILLED(4.0)),
    ok = tflite_beam_signature_runner:invoke(Runner),
    ?assertEqual({ok, ?FILLED(6.0)}, tflite_beam_signature_runner:output_tensor(Runner, <<"x">>)),
    ?assertEqual({ok, ?FILLED(9.0)}, tflite_beam_signature_runner:output_tensor(Runner, <<"y">>)).

%% multi_add.bin's three ADDs are one XNNPACK partition, and a delegate replaces
%% the partition it claims with a single execution-plan entry. That collapse is
%% how every later delegate change proves a delegate actually ran, so it is
%% pinned here. Measure it on an interpreter that has touched no signature
%% runner: `get_signature_runner/2' applies the lazy providers too.
%%
%% This is the lazy path specifically -- TfLite applying XNNPACK by itself inside
%% allocate_tensors/1 -- so it asks for the resolver that still does that. The
%% default resolver no longer does; the builder attaches XNNPACK explicitly
%% instead, and the plan has already collapsed by the time build/2 returns.
execution_plan_baseline(_Config) ->
    {Builder, Interpreter} = tflite_beam_test_models:lazy_builder("multi_add.bin"),
    ok = tflite_beam_interpreter_builder:build(Builder, Interpreter),
    ?assertEqual([0, 1, 2], tflite_beam_interpreter:execution_plan(Interpreter)),
    ?assertEqual(3, tflite_beam_interpreter:nodes_size(Interpreter)),
    ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
    case xnnpack_compiled_in() of
        false ->
            {skip, "XNNPACK is not compiled into this build"};
        true ->
            ?assertEqual(1, length(tflite_beam_interpreter:execution_plan(Interpreter))),
            %% the delegate node is appended rather than substituted, so the two
            %% numbers are never equal
            ?assertEqual(4, tflite_beam_interpreter:nodes_size(Interpreter))
    end.

%% The control: XNNPACK is compiled in here too, and claims nothing. Delegation
%% is per op pattern, so a plan that does not move is not evidence of anything.
quantized_model_is_not_delegated(_Config) ->
    {Builder, Interpreter} = tflite_beam_test_models:lazy_builder("add_quantized_int8.bin"),
    ok = tflite_beam_interpreter_builder:build(Builder, Interpreter),
    ?assertEqual([0, 1], tflite_beam_interpreter:execution_plan(Interpreter)),
    ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
    ?assertEqual([0, 1], tflite_beam_interpreter:execution_plan(Interpreter)),
    ?assertEqual(2, tflite_beam_interpreter:nodes_size(Interpreter)).

xnnpack_compiled_in() ->
    lists:member(xnnpack, tflite_beam_delegate:available()).

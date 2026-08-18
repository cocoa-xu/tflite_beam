%% @doc Locating and loading the model fixtures the suites share.
-module(tflite_beam_test_models).

-export([path/1, builder/1, interpreter/1]).

-include("../src/tflite_beam/tflite_beam_records.hrl").

path(Name) ->
    filename:join(dir(), Name).

%% @doc A builder over Name, and a fresh interpreter for it to build into.
builder(Name) ->
    {ok, Resolver} = tflite_beam_ops_builtin_builtin_resolver:new(),
    #tflite_beam_flatbuffer_model{ref = Model} =
        tflite_beam_flatbuffer_model:build_from_file(path(Name)),
    {ok, Builder} = tflite_beam_interpreter_builder:new(Model, Resolver),
    {ok, Interpreter} = tflite_beam_interpreter:new(),
    {Builder, Interpreter}.

%% @doc An interpreter over Name, built and with its tensors allocated.
interpreter(Name) ->
    {Builder, Interpreter} = builder(Name),
    ok = tflite_beam_interpreter_builder:build(Builder, Interpreter),
    ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
    Interpreter.

dir() ->
    Candidates = [
        filename:join(filename:dirname(code:which(?MODULE)), "models"),
        filename:join([code:lib_dir(tflite_beam), "test", "models"]),
        filename:absname("test/models")
    ],
    case lists:search(fun filelib:is_dir/1, Candidates) of
        {value, Dir} -> Dir;
        false -> error({test_models_not_found, Candidates})
    end.

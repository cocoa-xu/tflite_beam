%% @doc
%% An interpreter that lives inside a process, so that feeding it, running it
%% and reading the result back is one step that nothing can interleave with.
%%
%% The lower-level API is not wrong -- it mirrors TfLite's C API faithfully --
%% but it does not say anywhere that `input_tensor/3', `invoke/1' and
%% `output_tensor/2' have to be treated as one operation. Two processes taking
%% turns badly get each other's answers: measured on a real model, 147 wrong
%% results in 400 calls, silently. This module is the answer to that, and the
%% direct API stays exactly as it is for callers who would rather serialise
%% access themselves.
%%
%% The interpreter is handed to this process with
%% `tflite_beam_interpreter:controlling_process/2', so it cannot be reached from
%% anywhere else even by a caller holding the reference.
-module(tflite_beam_interpreter_server).
-behaviour(gen_server).

-export([
    start_link/1, start_link/2,
    start/1, start/2,
    predict/2, predict/3,
    with/2, with/3,
    stop/1
]).

-export([init/1, handle_call/3, handle_cast/2]).

-define(DEFAULT_TIMEOUT, 30000).

%% @doc Start an interpreter process for a model file.
-spec start_link(list() | binary()) -> {ok, pid()} | {error, term()}.
start_link(ModelPath) ->
    start_link(ModelPath, []).

%% @doc
%% Start an interpreter process for a model file.
%%
%% ==== Keyword Parameters ====
%% @param num_threads Passed to
%% `tflite_beam_interpreter_builder:set_num_threads/2' before the interpreter is
%% built, so it reaches the default XNNPACK delegate as well.
-spec start_link(list() | binary(), list()) -> {ok, pid()} | {error, term()}.
start_link(ModelPath, Opts) when is_list(Opts) ->
    gen_server:start_link(?MODULE, {ModelPath, Opts}, []).

%% @doc Start an interpreter process outside a supervision tree.
-spec start(list() | binary()) -> {ok, pid()} | {error, term()}.
start(ModelPath) ->
    start(ModelPath, []).

%% @doc Start an interpreter process outside a supervision tree.
-spec start(list() | binary(), list()) -> {ok, pid()} | {error, term()}.
start(ModelPath, Opts) when is_list(Opts) ->
    gen_server:start(?MODULE, {ModelPath, Opts}, []).

%% @doc
%% Feed, run and read back, as one operation.
%%
%% Concurrent callers are serialised by the process rather than racing inside
%% the interpreter, so each gets the answer to its own input.
-spec predict(pid(), binary() | list() | map()) -> list(binary()) | {error, binary()}.
predict(Server, Input) ->
    predict(Server, Input, ?DEFAULT_TIMEOUT).

%% @doc Feed, run and read back, with a call timeout.
-spec predict(pid(), binary() | list() | map(), timeout()) -> list(binary()) | {error, binary()}.
predict(Server, Input, Timeout) ->
    gen_server:call(Server, {predict, Input}, Timeout).

%% @doc
%% Run a function against the interpreter inside the owning process.
%%
%% For the sequences `predict/2' does not cover -- resizing an input and
%% reallocating, say, or driving a signature runner. The function runs in this
%% process, so nothing else touches the interpreter while it does, and it should
%% return promptly for the same reason.
-spec with(pid(), fun((reference()) -> Result)) -> Result | {error, binary()}.
with(Server, Fun) ->
    with(Server, Fun, ?DEFAULT_TIMEOUT).

%% @doc Run a function against the interpreter inside the owning process.
-spec with(pid(), fun((reference()) -> Result), timeout()) -> Result | {error, binary()}.
with(Server, Fun, Timeout) when is_function(Fun, 1) ->
    gen_server:call(Server, {with, Fun}, Timeout).

%% @doc Stop the process, and with it the interpreter.
-spec stop(pid()) -> ok.
stop(Server) ->
    gen_server:stop(Server).

%% gen_server

init({ModelPath, Opts}) ->
    case build(ModelPath, Opts) of
        {ok, Interpreter} ->
            %% belt as well as braces: even a caller who somehow obtains the
            %% reference is refused, rather than quietly racing us
            ok = tflite_beam_interpreter:controlling_process(Interpreter, self()),
            {ok, #{interpreter => Interpreter}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({predict, Input}, _From, State = #{interpreter := Interpreter}) ->
    {reply, tflite_beam_interpreter:predict(Interpreter, Input), State};
handle_call({with, Fun}, _From, State = #{interpreter := Interpreter}) ->
    {reply, Fun(Interpreter), State};
handle_call(_Request, _From, State) ->
    {reply, {error, <<"unknown request">>}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

build(ModelPath, Opts) ->
    case proplists:get_value(num_threads, Opts) of
        undefined ->
            tflite_beam_interpreter:new(ModelPath);
        NumThreads when is_integer(NumThreads) ->
            build_with_threads(ModelPath, NumThreads)
    end.

build_with_threads(ModelPath, NumThreads) ->
    case tflite_beam_flatbuffer_model:build_from_file(ModelPath) of
        {error, Reason} ->
            {error, Reason};
        Model ->
            {ok, Resolver} = tflite_beam_ops_builtin_builtin_resolver:new(),
            {ok, Builder} = tflite_beam_interpreter_builder:new(element(4, Model), Resolver),
            ok = tflite_beam_interpreter_builder:set_num_threads(Builder, NumThreads),
            {ok, Interpreter} = tflite_beam_interpreter:new(),
            case tflite_beam_interpreter_builder:build(Builder, Interpreter) of
                {error, Reason} ->
                    {error, Reason};
                _ ->
                    case tflite_beam_interpreter:allocate_tensors(Interpreter) of
                        ok -> {ok, Interpreter};
                        {error, Reason} -> {error, Reason}
                    end
            end
    end.

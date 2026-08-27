%% @doc
%% A compiled model that lives inside a process, so that no two callers share
%% its buffers.
%%
%% `tflite_beam_litert_compiled_model' is not wrong, and it mirrors LiteRT's C
%% API faithfully. What it does not say anywhere is that a compiled model owns
%% one set of input and output buffers for its whole life, allocated when it is
%% built rather than per call. `run/2' writes the caller's input into those
%% buffers, runs, and reads the outputs back out of them.
%%
%% Two processes doing that at once mostly fail loudly, and sometimes do not.
%% Measured with four processes running twenty-five inferences each against one
%% shared model, every one checking the answer to its own input: 74 of the 100
%% calls were refused with a runtime failure, 14 were right, and **12 came back
%% holding another process's answer**. The refusals are survivable. Those 12
%% are why this module exists, because nothing tells the caller about them.
%% Through this module the same measurement gives 100 right out of 100.
%%
%% This is a different hazard from the one
%% `tflite_beam_interpreter_server' answers. There the danger is a three-step
%% sequence being interleaved; here `run/2' is a single call and interleaving
%% still loses, because the state that races is the buffers behind it. Holding
%% the model in one process is the same answer to both.
%%
%% The profile is per model, not per call, so `summarise_profile/1' here reports
%% every run since the last `reset_profile/1' rather than the last one. That is
%% usually what you want from a server: a shape over many calls rather than one
%% sample. Reset it when you want to measure a change.
-module(tflite_beam_litert_compiled_model_server).
-behaviour(gen_server).

-export([
    start_link/2, start_link/3,
    start/2, start/3,
    run/2, run/3,
    with/2, with/3,
    fully_accelerated/1,
    io_sizes/1,
    profile/1,
    summarise_profile/1,
    reset_profile/1,
    stop/1
]).

-export([init/1, handle_call/3, handle_cast/2]).

-define(DEFAULT_TIMEOUT, 30000).
-define(M, tflite_beam_litert_compiled_model).

%% @doc Start a compiled model process, on the CPU with no profiling.
-spec start_link(reference(), binary() | list()) -> {ok, pid()} | {error, term()}.
start_link(Env, ModelPath) ->
    start_link(Env, ModelPath, #{}).

%% @doc
%% Start a compiled model process.
%%
%% `Opts' is what `tflite_beam_litert_compiled_model:new/3' takes. The
%% environment is created by the caller and may be shared between servers: it
%% carries where accelerator plugins are found and nothing per-model.
-spec start_link(reference(), binary() | list(), map()) -> {ok, pid()} | {error, term()}.
start_link(Env, ModelPath, Opts) when is_map(Opts) ->
    gen_server:start_link(?MODULE, {Env, ModelPath, Opts}, []).

%% @doc Start a compiled model process outside a supervision tree.
-spec start(reference(), binary() | list()) -> {ok, pid()} | {error, term()}.
start(Env, ModelPath) ->
    start(Env, ModelPath, #{}).

%% @doc Start a compiled model process outside a supervision tree.
-spec start(reference(), binary() | list(), map()) -> {ok, pid()} | {error, term()}.
start(Env, ModelPath, Opts) when is_map(Opts) ->
    gen_server:start(?MODULE, {Env, ModelPath, Opts}, []).

%% @doc
%% Run the model over `Inputs' and return its outputs.
%%
%% Concurrent callers are serialised by the process rather than racing over the
%% model's buffers, so each gets the answer to its own input.
-spec run(pid(), [binary()]) -> {ok, [binary()]} | {error, binary()}.
run(Server, Inputs) ->
    run(Server, Inputs, ?DEFAULT_TIMEOUT).

%% @doc
%% Run the model, with a call timeout.
%%
%% The timeout gives up on the answer; it does not stop the work. When it runs
%% out this exits the calling process, which is `gen_server:call/3' behaviour,
%% and the server carries on with the inference it was given. Anything queued
%% behind it still waits. Raise the timeout rather than retry: a retry joins the
%% queue behind the call it replaced.
-spec run(pid(), [binary()], timeout()) -> {ok, [binary()]} | {error, binary()}.
run(Server, Inputs, Timeout) when is_list(Inputs) ->
    gen_server:call(Server, {run, Inputs}, Timeout).

%% @doc
%% Run a function against the compiled model inside the owning process.
%%
%% For the sequences `run/2' does not cover, such as resetting the profile and
%% running a measured batch as one uninterrupted step. The function runs in this
%% process, so nothing else touches the model while it does, and it should
%% return promptly for the same reason.
-spec with(pid(), fun((reference()) -> Result)) -> Result | {error, binary()}.
with(Server, Fun) ->
    with(Server, Fun, ?DEFAULT_TIMEOUT).

%% @doc Run a function against the compiled model inside the owning process.
-spec with(pid(), fun((reference()) -> Result), timeout()) -> Result | {error, binary()}.
with(Server, Fun, Timeout) when is_function(Fun, 1) ->
    gen_server:call(Server, {with, Fun}, Timeout).

%% @doc Whether one accelerator claimed the whole graph.
-spec fully_accelerated(pid()) -> {ok, boolean()} | {error, binary()}.
fully_accelerated(Server) ->
    with(Server, fun(M) -> ?M:fully_accelerated(M) end).

%% @doc The byte size of each input and output buffer.
-spec io_sizes(pid()) -> {ok, {[non_neg_integer()], [non_neg_integer()]}} | {error, binary()}.
io_sizes(Server) ->
    with(Server, fun(M) -> ?M:io_sizes(M) end).

%% @doc Every profiling event recorded since the last reset.
-spec profile(pid()) -> {ok, [map()]} | {error, binary()}.
profile(Server) ->
    with(Server, fun(M) -> ?M:profile(M) end).

%% @doc Per-operator totals over every run since the last reset, slowest first.
-spec summarise_profile(pid()) -> {ok, [{binary(), pos_integer(), non_neg_integer()}]}
                                | {error, binary()}.
summarise_profile(Server) ->
    with(Server, fun(M) -> ?M:summarise_profile(M) end).

%% @doc Forget the events recorded so far and keep recording.
-spec reset_profile(pid()) -> ok | {error, binary()}.
reset_profile(Server) ->
    with(Server, fun(M) -> ?M:reset_profile(M) end).

%% @doc Stop the process, and with it the compiled model.
-spec stop(pid()) -> ok.
stop(Server) ->
    gen_server:stop(Server).

%% gen_server

init({Env, ModelPath, Opts}) ->
    case ?M:new(Env, ModelPath, Opts) of
        {ok, Model} ->
            %% the reference never leaves this process except inside `with/2',
            %% which runs its function here rather than handing the model out
            {ok, #{model => Model}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({run, Inputs}, _From, State = #{model := Model}) ->
    {reply, ?M:run(Model, Inputs), State};
handle_call({with, Fun}, _From, State = #{model := Model}) ->
    {reply, Fun(Model), State};
handle_call(_Request, _From, State) ->
    {reply, {error, <<"unknown request">>}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

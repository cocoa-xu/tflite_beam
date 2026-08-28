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
%% LiteRT states that its compiled model API is not verified for multithreading,
%% and the profile buffer underneath says outright that it is not thread safe,
%% so two callers inside one model at once is a data race and not merely crossed
%% outputs. The direct module therefore refuses a second concurrent caller
%% rather than admitting it, and before it did, four processes running
%% twenty-five inferences each against one shared model got a handful of answers
%% belonging to a different process, with nothing to say which ones.
%%
%% Refusal is honest but it is not a queue. This module is the queue: callers
%% wait their turn instead of being told to come back.
%%
%% This is a different hazard from the one
%% `tflite_beam_interpreter_server' answers. There the danger is a three-step
%% sequence being interleaved; here `run/2' is a single call and interleaving
%% still loses, because the state that races is the buffers behind it. Holding
%% the model in one process is the same answer to both.
%%
%% The profile is per model, not per call, so `summarise_profile/1' here reports
%% the runs since the last `reset_profile/1' rather than the last one. That is
%% usually what you want from a server: a shape over many calls rather than one
%% sample. The buffer under it is fixed at 512 * 1024 entries, so it is bounded
%% but not tight. Reset when you want to measure a change.
-module(tflite_beam_litert_compiled_model_server).
-behaviour(gen_server).

-export([
    start_link/2, start_link/3,
    start/2, start/3,
    run/2, run/3,
    with/2, with/3,
    fully_accelerated/1,
    io_sizes/1,
    run_with_metrics/2, run_with_metrics/3, run_with_metrics/4,
    profile/1, profile/2,
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
%% running a measured batch as one uninterrupted step.
%%
%% What this guarantees is narrow and worth stating exactly: the function runs
%% in this process, so the server handles no other message while it does. It
%% does not sandbox the function. A callback that raises takes the server, and
%% the model, down with it. A callback that keeps the reference and hands the
%% model to another process with
%% `tflite_beam_litert_compiled_model:controlling_process/2' leaves this server
%% alive and unable to use its own model. And a timeout on `with/3' ends the
%% wait, not the callback, which carries on holding the server.
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

%% @doc Run the model and collect whatever counters the accelerator reports.
-spec run_with_metrics(pid(), [binary()]) ->
    {ok, {[binary()], [{binary(), term()}]}} | {error, binary()}.
run_with_metrics(Server, Inputs) ->
    run_with_metrics(Server, Inputs, 0, ?DEFAULT_TIMEOUT).

%% @doc Run the model with metrics collection bracketing the inference.
-spec run_with_metrics(pid(), [binary()], ?M:detail_level()) ->
    {ok, {[binary()], [{binary(), term()}]}} | {error, binary()}.
run_with_metrics(Server, Inputs, DetailLevel) ->
    run_with_metrics(Server, Inputs, DetailLevel, ?DEFAULT_TIMEOUT).

%% @doc
%% Run the model with metrics, with a call timeout.
%%
%% The guard is here as well as in the direct module on purpose: an argument the
%% direct module refuses with `function_clause' would raise inside this server's
%% `handle_call' and take the server down with it, so a caller mistake would
%% cost the model rather than the call.
-spec run_with_metrics(pid(), [binary()], ?M:detail_level(), timeout()) ->
    {ok, {[binary()], [{binary(), term()}]}} | {error, binary()}.
run_with_metrics(Server, Inputs, DetailLevel, Timeout)
        when is_list(Inputs), is_integer(DetailLevel),
             DetailLevel >= 0, DetailLevel =< 2147483647 ->
    gen_server:call(Server, {run_with_metrics, Inputs, DetailLevel}, Timeout).

%% @doc Every profiling event recorded since the last reset.
-spec profile(pid()) -> {ok, [map()]} | {error, binary()}.
profile(Server) ->
    profile(Server, 0).

%% @doc
%% The most recent `Limit' profiling events, or all of them when `Limit' is zero.
%%
%% A server that runs for a long time is exactly the case where the bound is
%% worth using: nothing trims the profile except `reset_profile/1'.
-spec profile(pid(), non_neg_integer()) -> {ok, [map()]} | {error, binary()}.
profile(Server, Limit) ->
    with(Server, fun(M) -> ?M:profile(M, Limit) end).

%% @doc Per-operator totals over every run since the last reset, slowest first.
-spec summarise_profile(pid()) ->
    {ok, [{binary(), tflite_beam_litert_compiled_model:operator_kind(),
           pos_integer(), non_neg_integer()}]} | {error, binary()}.
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
            %% belt as well as braces: `with/2' runs its function here rather
            %% than handing the model out, but a function that captures the
            %% reference and uses it later from somewhere else is refused too
            ok = ?M:controlling_process(Model, self()),
            {ok, #{model => Model}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({run, Inputs}, _From, State = #{model := Model}) ->
    {reply, ?M:run(Model, Inputs), State};
handle_call({run_with_metrics, Inputs, DetailLevel}, _From, State = #{model := Model}) ->
    {reply, ?M:run_with_metrics(Model, Inputs, DetailLevel), State};
handle_call({with, Fun}, _From, State = #{model := Model}) ->
    {reply, Fun(Model), State};
handle_call(_Request, _From, State) ->
    {reply, {error, <<"unknown request">>}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

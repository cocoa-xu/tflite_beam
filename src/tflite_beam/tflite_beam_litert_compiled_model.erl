%% @doc
%% LiteRT's compiled model, as it is.
%%
%% This is not the faster way to run a model. Measured on one machine against
%% `mobilenet_v2_1.0_224', a compiled model on the GPU and an interpreter with
%% the same plugin attached through `tflite_beam_delegate:external/2' land in
%% the same band, because underneath they are the same delegate. Reach for this
%% when you want to see **where the time went**: a profiler can be attached to a
%% compiled model and to nothing else, and it reports every operator, how long
%% it took, and whether an accelerator or the CPU ran it.
%%
%% A model owns its input and output buffers, allocated once when it is built,
%% so `run/2' writes into those buffers, runs, and reads back out of them. LiteRT
%% does not promise its compiled model API is safe to enter from two threads, so
%% a second caller arriving while one is inside the model is **refused** with
%% `{error, <<"compiled model is in use by another caller">>}' rather than
%% admitted. That is a refusal, not a queue: callers who would rather wait their
%% turn want `tflite_beam_litert_compiled_model_server'.
-module(tflite_beam_litert_compiled_model).

-include_lib("kernel/include/file.hrl").

-export([
    environment/0, environment/1,
    signatures/2,
    new/2, new/3,
    run/2,
    fully_accelerated/1,
    io_sizes/1,
    profile/1, profile/2,
    reset_profile/1,
    summarise_profile/1,
    run_with_metrics/2, run_with_metrics/3,
    controlling_process/1, controlling_process/2,
    platform_support/0
]).

-type accelerator() :: cpu | gpu | npu.
-type precision() :: default | fp16 | fp32 | fp16_with_fp32_accum.
%% The signature index crosses into the NIF through enif_get_int, so the range
%% is the C int range and not every non_neg_integer().
-type signature_index() :: 0..2147483647.
%% Same range and for the same reason: it crosses into the NIF as a C int.
-type detail_level() :: 0..2147483647.
-type operator_kind() :: operator | delegate_operator | delegate_profiled.
-type opts() :: #{
    accelerators => [accelerator()],
    precision => precision(),
    profile => boolean(),
    signature => signature_index() | binary() | string(),
    max_model_bytes => non_neg_integer()
}.
-export_type([accelerator/0, precision/0, opts/0, signature_index/0,
              detail_level/0, operator_kind/0]).

%% @doc
%% What this build of the library can reach.
%%
%% Compile-time answers, decided by LiteRT from the macros it was built with and
%% by what this build turns off. `opencl' reads false everywhere here, because
%% this library defines `LITERT_DISABLE_OPENCL_SUPPORT': LiteRT's own OpenCL
%% layer exists so it can pass CL buffers around itself and nothing here asks it
%% to, while a GPU accelerator plugin brings its own OpenCL and is unaffected.
%% This says "was it compiled in", not "is a device present"; the second
%% question is answered by asking for the accelerator and being refused.
-spec platform_support() -> #{atom() => boolean()}.
platform_support() ->
    tflite_beam_nif:litert_platform_support().

%% @doc
%% An environment with no directory to look for accelerator plugins in.
%%
%% Not a guarantee that none is found: LiteRT still passes a bare filename to
%% the platform loader, which has its own search paths. It is the way to say
%% "I am not pointing you at one", not the way to say "there is none".
-spec environment() -> {ok, reference()} | {error, binary()}.
environment() ->
    environment(<<>>).

%% @doc
%% An environment that looks for accelerator plugins in `Dir'.
%%
%% A GPU accelerator is a shared library loaded at run time, and LiteRT looks
%% for it by filename relative to this directory. Leave it empty and the search
%% happens relative to nothing, so a GPU compile falls back to the CPU with
%% only a line in the log to say why.
-spec environment(binary() | string()) -> {ok, reference()} | {error, binary()}.
environment(Dir) when is_list(Dir) ->
    case to_binary(Dir) of
        {ok, Binary} -> environment(Binary);
        Error -> Error
    end;
environment(Dir) when is_binary(Dir) ->
    case check_no_nul(Dir, <<"runtime library directory">>) of
        ok -> tflite_beam_nif:litert_environment_new(Dir);
        Error -> Error
    end.

%% @doc
%% The signature keys of a model, in index order.
%%
%% A model with no named signatures still has one, and it comes back as the
%% empty key. Reading these needs the model but not a compile, so it is the
%% cheap way to find out what `new/3' can be asked for.
-spec signatures(reference(), binary() | string()) -> {ok, [binary()]} | {error, binary()}.
signatures(Env, Path) when is_list(Path) ->
    case to_binary(Path) of
        {ok, Binary} -> signatures(Env, Binary);
        Error -> Error
    end;
signatures(Env, Path) when is_binary(Path) ->
    case check_no_nul(Path, <<"model path">>) of
        ok -> tflite_beam_nif:litert_model_signatures(Env, Path);
        Error -> Error
    end.

%% @doc A compiled model on the CPU, with no profiling.
-spec new(reference(), binary() | string()) -> {ok, reference()} | {error, binary()}.
new(Env, Path) ->
    new(Env, Path, #{}).

%% @doc
%% Compile `Path' in `Env'.
%%
%% ==== Keyword Parameters ====
%% @param accelerators Which hardware may run the model, as a list. The default
%% is `[cpu]'. Naming `gpu' does not promise a GPU: if no accelerator plugin is
%% found the compile fails rather than quietly running on the CPU, so check the
%% result rather than assuming.
%% @param precision What a GPU accelerator should compute in.
%%
%% `default' asks for nothing and lets the accelerator decide, which is not the
%% same as asking for `fp32'. Measured against the plugins in
%% `tflite_delegate_plugins', `default' does come out as float32, because those
%% pass the delegate's own default and TFLite's GPU delegates keep float32; a
%% different accelerator may well choose float16 when it can. Name `fp32' if
%% the accuracy matters rather than relying on the default to mean it.
%%
%% `fp32' agrees with the CPU answer to within rounding. `fp16' is faster and
%% does not; on an M4 Max the largest element-wise difference against the CPU
%% went from 3.02e-6 to 2.01e-2. `fp16_with_fp32_accum' asks for float16
%% arithmetic accumulated in float32 where the backend offers it. All of them
%% are ignored by the CPU.
%% @param signature Which signature this model runs, as an index or a key. The
%% buffers are allocated for that one signature and `run/2' runs it, so a model
%% with several signatures needs one of these per signature rather than one that
%% switches. Defaults to the first.
%% @param max_model_bytes Refuse a model file larger than this, before LiteRT
%% reads and parses it. Zero, the default, means no ceiling. Worth setting when
%% the path comes from somewhere you do not control, because what LiteRT
%% allocates for a model is invisible to the emulator's memory accounting.
%% @param profile Whether to record per-operator timings, readable with
%% `profile/1'. Measured on `mobilenet_v2_1.0_224', 50 runs, three times each:
%% at most 1.05x on the CPU and nothing measurable on the GPU, where the whole
%% graph is one delegate node and there is no per-operator boundary left to
%% time. That is one model on one machine and not a promise; a graph the
%% accelerator splits into many nodes has many more boundaries to time.
%% The events accumulate across runs, so `reset_profile/1' is what keeps a
%% long-lived model bounded.
-spec new(reference(), binary() | string(), opts()) -> {ok, reference()} | {error, binary()}.
new(Env, Path, Opts) when is_list(Path) ->
    case to_binary(Path) of
        {ok, Binary} -> new(Env, Binary, Opts);
        Error -> Error
    end;
new(Env, Path, Opts) when is_binary(Path), is_map(Opts) ->
    case check_no_nul(Path, <<"model path">>) of
        ok -> new_checked(Env, Path, Opts);
        Error -> Error
    end.

%% Every option value is checked here rather than left to a guard, because a
%% wrong value in an options map is an ordinary mistake and the caller of a
%% function whose siblings all answer {error, Binary} should not have to catch
%% one of them instead.
new_checked(Env, Path, Opts) ->
    case check_model_size(Path, maps:get(max_model_bytes, Opts, 0)) of
        ok -> new_validated(Env, Path, Opts);
        Error -> Error
    end.

%% A model file is read and parsed by LiteRT before anything here can object, and
%% the memory that takes is invisible to the emulator's accounting. A caller that
%% takes model paths from somewhere it does not control wants a ceiling; one that
%% ships its own models does not, so the default is none.
check_model_size(_Path, 0) ->
    ok;
check_model_size(Path, Max) when is_integer(Max), Max > 0 ->
    case file:read_file_info(Path) of
        {ok, #file_info{size = Size}} when Size > Max ->
            {error, iolist_to_binary(
                io_lib:format("the model is ~p bytes and the limit is ~p", [Size, Max]))};
        {ok, _} ->
            ok;
        {error, Reason} ->
            {error, iolist_to_binary(
                io_lib:format("cannot read ~ts: ~p", [Path, Reason]))}
    end;
check_model_size(_Path, Other) ->
    {error, iolist_to_binary(
        io_lib:format("max_model_bytes must be a non-negative integer, got ~p", [Other]))}.

new_validated(Env, Path, Opts) ->
    with_result([
        fun() -> accelerator_set(maps:get(accelerators, Opts, [cpu])) end,
        fun() -> precision_value(maps:get(precision, Opts, default)) end,
        fun() -> profile_flag(maps:get(profile, Opts, false)) end
    ], fun([Accel, Prec, Profile]) ->
        new_with_signature(Env, Path, Accel, Prec, Profile, Opts)
    end).

%% Runs each check, stops at the first {error, _}, and hands the values on.
with_result(Checks, Continue) ->
    with_result(Checks, [], Continue).

with_result([], Acc, Continue) ->
    Continue(lists:reverse(Acc));
with_result([Check | Rest], Acc, Continue) ->
    case Check() of
        {ok, Value} -> with_result(Rest, [Value | Acc], Continue);
        {error, _} = Error -> Error
    end.

profile_flag(true) -> {ok, 1};
profile_flag(false) -> {ok, 0};
profile_flag(Other) ->
    {error, iolist_to_binary(
        io_lib:format("profile must be true or false, got ~p", [Other]))}.

new_with_signature(Env, Path, Accel, Prec, Profile, Opts) ->
    case signature_index(Env, Path, maps:get(signature, Opts, 0)) of
        {ok, Index} ->
            tflite_beam_nif:litert_compiled_model_new(Env, Path, Accel, Prec, Profile, Index);
        Error ->
            Error
    end.

%% @doc
%% Run the model over `Inputs' and return its outputs.
%%
%% One binary per input, each exactly the size `io_sizes/1' reports; anything
%% else is refused rather than truncated or padded.
-spec run(reference(), [binary()]) -> {ok, [binary()]} | {error, binary()}.
run(Model, Inputs) when is_list(Inputs) ->
    tflite_beam_nif:litert_compiled_model_run(Model, Inputs).

%% @doc
%% Whether anything is left for the ordinary interpreter to run.
%%
%% True means no undelegated operations remain, which several accelerators
%% between them can satisfy just as well as one. False does not have to mean a
%% split between accelerators either: it means at least one operation is running
%% the ordinary way. `profile/1' is what says which.
-spec fully_accelerated(reference()) -> {ok, boolean()} | {error, binary()}.
fully_accelerated(Model) ->
    tflite_beam_nif:litert_compiled_model_fully_accelerated(Model).

%% @doc The byte size of each input and output buffer.
-spec io_sizes(reference()) -> {ok, {[non_neg_integer()], [non_neg_integer()]}} | {error, binary()}.
io_sizes(Model) ->
    tflite_beam_nif:litert_compiled_model_io_sizes(Model).

%% @doc
%% Every profiling event recorded so far, oldest first.
%%
%% Empty unless the model was built with `profile => true'. Each event is a map
%% of `tag', `us', `type' and `source'.
%%
%% **Provisional.** `type' and `source' are LiteRT's own enumeration numbers,
%% passed through rather than named, so they can change with an upstream bump
%% and mean something different without this function changing. Build on
%% `summarise_profile/1', which speaks in atoms, unless you specifically need an
%% event this does not fold. Telemetry events are included and carry
%% a sentinel in place of a duration; `summarise_profile/1' drops them.
%%
%% The buffer behind this is fixed and large: a compiled model asks for
%% 512 * 1024 entries in LiteRT 2.2.0, not the 10240 a bare profiler defaults
%% to. So a long-lived model does not grow without bound, but neither will it
%% overflow soon. Reset it when you want to measure a change rather than to keep
%% it in check.
-spec profile(reference()) -> {ok, [map()]} | {error, binary()}.
profile(Model) ->
    profile(Model, 0).

%% @doc
%% The most recent `Limit' profiling events, oldest first, or all of them when
%% `Limit' is zero.
%%
%% Worth using on a long-lived model. Events accumulate until the buffer's
%% 512 * 1024 entries are full, and asking for all of them then builds half a
%% million maps in one call.
-spec profile(reference(), non_neg_integer()) -> {ok, [map()]} | {error, binary()}.
profile(Model, Limit) when is_integer(Limit), Limit >= 0, Limit =< 2147483647 ->
    tflite_beam_nif:litert_compiled_model_profile(Model, Limit).

%% @doc Run the model and collect whatever counters the accelerator reports.
-spec run_with_metrics(reference(), [binary()]) ->
    {ok, {[binary()], [{binary(), term()}]}} | {error, binary()}.
run_with_metrics(Model, Inputs) ->
    run_with_metrics(Model, Inputs, 0).

%% @doc
%% Run the model with metrics collection bracketing the inference.
%%
%% Usually the counters come back empty. Filling them in is the accelerator's
%% job, through two entries of its definition that are allowed to be null, so an
%% empty list means nobody offered anything rather than that something went
%% wrong. Every accelerator this library has been run against returned nothing;
%% what a vendor backend does is its own business and is not established here.
%%
%% Collection brackets the run rather than being started and stopped on its own,
%% because a backend asked to report on an interval containing no inference has
%% nothing to report on. Use `profile/1' for timings; this is for counters a
%% backend chooses to expose.
-spec run_with_metrics(reference(), [binary()], detail_level()) ->
    {ok, {[binary()], [{binary(), term()}]}} | {error, binary()}.
run_with_metrics(Model, Inputs, DetailLevel)
        when is_list(Inputs), is_integer(DetailLevel),
             DetailLevel >= 0, DetailLevel =< 2147483647 ->
    tflite_beam_nif:litert_compiled_model_run_with_metrics(Model, Inputs, DetailLevel).

%% @doc Which process this model belongs to, or `undefined' if nobody has
%% claimed it. Answers `{error, _}' while another caller is inside the model,
%% because the answer is read under the same lock that call holds.
-spec controlling_process(reference()) -> {ok, pid()} | undefined | {error, binary()}.
controlling_process(Model) when is_reference(Model) ->
    tflite_beam_nif:litert_compiled_model_get_controlling_process(Model).

%% @doc
%% Give the model to `Pid', after which every other process is refused.
%%
%% Unclaimed is the default and stays the default: a model nobody has claimed is
%% usable from wherever its reference reaches, which is what this module is for.
%% Claiming is how `tflite_beam_litert_compiled_model_server' makes its promise
%% enforced rather than a convention, and it is available here for anyone
%% building their own owner. A claim whose process has died is released, so a
%% model is never stranded.
-spec controlling_process(reference(), pid()) -> ok | {error, binary()}.
controlling_process(Model, Pid) when is_reference(Model), is_pid(Pid) ->
    tflite_beam_nif:litert_compiled_model_set_controlling_process(Model, Pid).

%% @doc Forget the events recorded so far and keep recording.
-spec reset_profile(reference()) -> ok | {error, binary()}.
reset_profile(Model) ->
    tflite_beam_nif:litert_compiled_model_reset_profile(Model).

%% @doc
%% `profile/1' folded into per-operator totals.
%%
%% Returns `{Tag, Kind, Count, MicrosecondsTotal}' sorted slowest first, over
%% operator events only.
%%
%% **Provisional.** The tuple is anonymous and positional, which is the wrong
%% shape to be stuck with: a map with named keys is where this should end up,
%% and adding a field to a tuple breaks every caller that matched on it. Treat
%% the shape as unsettled until 1.0.0 says otherwise.
%%
%% The events LiteRT records are nested. An `Invoke' encloses the operators it
%% ran, and `AllocateTensors' and LiteRT's own buffer handling sit beside them,
%% so folding everything together would count operators twice and put `Invoke'
%% at the top of a list that claims to name the slowest operator. Only operator
%% events are folded here; everything else is still in `profile/1'.
%%
%% Nesting does not stop there, which is why `Kind' is in the tuple rather than
%% summed away. A `delegate_operator' is an operator *inside* a delegate, and
%% its time can already be counted in the enclosing `delegate_profiled' entry
%% for the fused operation. LiteRT's own summariser keeps the two apart for the
%% same reason. Totals within one `Kind' are additive; totals across kinds are
%% not.
-spec summarise_profile(reference()) ->
    {ok, [{binary(), operator_kind(), pos_integer(), non_neg_integer()}]} | {error, binary()}.
summarise_profile(Model) ->
    case profile(Model, 0) of
        {ok, Events} ->
            Totals = lists:foldl(
                fun(E, Acc) ->
                    case operator_kind(maps:get(type, E)) of
                        not_an_operator ->
                            Acc;
                        Kind ->
                            maps:update_with({maps:get(tag, E), Kind},
                                             fun({C, U}) -> {C + 1, U + maps:get(us, E)} end,
                                             {1, maps:get(us, E)}, Acc)
                    end
                end, #{}, Events),
            {ok, lists:reverse(lists:keysort(4,
                [{Tag, Kind, C, U} || {{Tag, Kind}, {C, U}} <- maps:to_list(Totals)]))};
        Error ->
            Error
    end.

%% LiteRtProfilerEventType, of which only these three are an operator running.
%% DEFAULT encloses them and the rest are not operators, so naming the three is
%% the whole filter: telemetry, which carries a sentinel where a duration would
%% go, is excluded by its type rather than by how large its number is.
operator_kind(2) -> operator;
operator_kind(4) -> delegate_operator;
operator_kind(8) -> delegate_profiled;
operator_kind(_) -> not_an_operator.

accelerator_set([]) ->
    {error, <<"name at least one accelerator; [] selects nothing to run on">>};
accelerator_set(List) when is_list(List) ->
    lists:foldl(
        fun(_, {error, _} = Error) -> Error;
           (A, {ok, Acc}) ->
               case accelerator_bit(A) of
                   {ok, Bit} -> {ok, Acc bor Bit};
                   Error -> Error
               end
        end, {ok, 0}, List);
accelerator_set(Other) ->
    {error, iolist_to_binary(
        io_lib:format("accelerators must be a list, got ~p", [Other]))}.

accelerator_bit(cpu) -> {ok, 1};
accelerator_bit(gpu) -> {ok, 2};
accelerator_bit(npu) -> {ok, 4};
accelerator_bit(Other) ->
    {error, iolist_to_binary(
        io_lib:format("~p is not an accelerator; expecting cpu, gpu or npu", [Other]))}.

%% The index crosses into the NIF through enif_get_int, so anything outside the
%% C int range is refused here where it can be named rather than there where it
%% looks like a parse failure.
signature_index(_Env, _Path, Index) when is_integer(Index), Index >= 0,
                                         Index =< 2147483647 ->
    {ok, Index};
signature_index(_Env, _Path, Index) when is_integer(Index) ->
    {error, iolist_to_binary(
        io_lib:format("signature index ~p is outside 0..2147483647", [Index]))};
signature_index(Env, Path, Key) when is_list(Key) ->
    case to_binary(Key) of
        {ok, Binary} -> signature_index(Env, Path, Binary);
        Error -> Error
    end;
signature_index(Env, Path, Key) when is_binary(Key) ->
    case check_no_nul(Key, <<"signature key">>) of
        ok -> signature_index_by_key(Env, Path, Key);
        Error -> Error
    end.

signature_index_by_key(Env, Path, Key) ->
    case tflite_beam_nif:litert_model_signatures(Env, Path) of
        {ok, Keys} ->
            case index_of(Key, Keys, 0) of
                not_found ->
                    {error, iolist_to_binary(
                        [<<"no signature named ">>, Key, <<" in this model">>])};
                Index ->
                    {ok, Index}
            end;
        Error ->
            Error
    end.

%% list_to_binary/1 raises on anything outside a byte range, so a perfectly
%% ordinary Unicode string would throw where every sibling returns an error.
to_binary(List) ->
    case unicode:characters_to_binary(List) of
        Binary when is_binary(Binary) -> {ok, Binary};
        _ -> {error, <<"expecting text, which this is not">>}
    end.

%% Everything here reaches C as a NUL terminated string, so a NUL inside one
%% would not be rejected, it would silently shorten it and name something else.
check_no_nul(Binary, What) ->
    case binary:match(Binary, <<0>>) of
        nomatch -> ok;
        _ -> {error, iolist_to_binary([<<"the ">>, What, <<" contains a zero byte">>])}
    end.

index_of(_Key, [], _N) -> not_found;
index_of(Key, [Key | _], N) -> N;
index_of(Key, [_ | Rest], N) -> index_of(Key, Rest, N + 1).

precision_value(default) -> {ok, 0};
precision_value(fp16) -> {ok, 1};
precision_value(fp32) -> {ok, 2};
precision_value(fp16_with_fp32_accum) -> {ok, 3};
precision_value(Other) ->
    {error, iolist_to_binary(
        io_lib:format("~p is not a precision; expecting default, fp16, fp32 or "
                      "fp16_with_fp32_accum", [Other]))}.

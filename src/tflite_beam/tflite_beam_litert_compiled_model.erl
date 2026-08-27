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
%% A model owns its input and output buffers, allocated once when it is built.
%% `run/2' writes into those buffers, runs, and reads back out of them, so two
%% processes calling `run/2' on the same model overwrite each other's inputs
%% and can read each other's outputs. Nothing here prevents that, on purpose:
%% see `tflite_beam_litert_compiled_model_server' for a version that does, and
%% keep this one if you would rather serialise access yourself.
-module(tflite_beam_litert_compiled_model).

-export([
    environment/0, environment/1,
    signatures/2,
    new/2, new/3,
    run/2,
    fully_accelerated/1,
    io_sizes/1,
    profile/1,
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
-type operator_kind() :: operator | delegate_operator | delegate_profiled.
-type opts() :: #{
    accelerators => [accelerator()],
    precision => precision(),
    profile => boolean(),
    signature => signature_index() | binary() | string()
}.
-export_type([accelerator/0, precision/0, opts/0, signature_index/0, operator_kind/0]).

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
    environment(list_to_binary(Dir));
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
    signatures(Env, list_to_binary(Path));
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
%% @param precision What a GPU accelerator should compute in. `fp32' is the
%% default and agrees with the CPU answer to within rounding; `fp16' is faster
%% and does not. Ignored by the CPU.
%% @param signature Which signature this model runs, as an index or a key. The
%% buffers are allocated for that one signature and `run/2' runs it, so a model
%% with several signatures needs one of these per signature rather than one that
%% switches. Defaults to the first.
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
    new(Env, list_to_binary(Path), Opts);
new(Env, Path, Opts) when is_binary(Path), is_map(Opts) ->
    case check_no_nul(Path, <<"model path">>) of
        ok -> new_checked(Env, Path, Opts);
        Error -> Error
    end.

new_checked(Env, Path, Opts) ->
    Accel = accelerator_set(maps:get(accelerators, Opts, [cpu])),
    Prec = precision_value(maps:get(precision, Opts, default)),
    Profile = case maps:get(profile, Opts, false) of true -> 1; false -> 0 end,
    case Accel of
        0 ->
            {error, <<"name at least one accelerator; [] selects nothing to run on">>};
        _ ->
            new_with_signature(Env, Path, Accel, Prec, Profile, Opts)
    end.

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
%% Whether one accelerator claimed the whole graph.
%%
%% False means the graph was split, and `profile/1' says where.
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
%% of `tag', `us', `type' and `source'. Telemetry events are included and carry
%% a sentinel in place of a duration; `summarise_profile/1' drops them.
%%
%% The buffer behind this holds a fixed number of events, 10240 in LiteRT 2.2.0,
%% so a long-lived model does not accumulate for ever and a busy one loses the
%% oldest. Read it, or `reset_profile/1' it, on a cadence that suits how many
%% operators your model has.
-spec profile(reference()) -> {ok, [map()]} | {error, binary()}.
profile(Model) ->
    tflite_beam_nif:litert_compiled_model_profile(Model).

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
-spec run_with_metrics(reference(), [binary()], signature_index()) ->
    {ok, {[binary()], [{binary(), term()}]}} | {error, binary()}.
run_with_metrics(Model, Inputs, DetailLevel)
        when is_list(Inputs), is_integer(DetailLevel), DetailLevel >= 0 ->
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
    case profile(Model) of
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

accelerator_set(List) when is_list(List) ->
    lists:foldl(fun(A, Acc) -> Acc bor accelerator_bit(A) end, 0, List).

accelerator_bit(cpu) -> 1;
accelerator_bit(gpu) -> 2;
accelerator_bit(npu) -> 4.

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
    signature_index(Env, Path, list_to_binary(Key));
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

precision_value(default) -> 0;
precision_value(fp16) -> 1;
precision_value(fp32) -> 2;
precision_value(fp16_with_fp32_accum) -> 3.

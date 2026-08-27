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
    metrics/1, metrics/2,
    platform_support/0
]).

-type accelerator() :: cpu | gpu | npu.
-type precision() :: default | fp16 | fp32 | fp16_with_fp32_accum.
-type opts() :: #{
    accelerators => [accelerator()],
    precision => precision(),
    profile => boolean(),
    signature => non_neg_integer() | binary() | list()
}.
-export_type([accelerator/0, precision/0, opts/0]).

%% @doc
%% What this build of the library can reach.
%%
%% Compile-time answers, decided by LiteRT from the macros it was built with, so
%% `opencl' reads false on Apple and true elsewhere whether or not a driver is
%% installed. This says "was it compiled in", not "is a device present"; the
%% second question is answered by asking for the accelerator and being refused.
-spec platform_support() -> #{atom() => boolean()}.
platform_support() ->
    tflite_beam_nif:litert_platform_support().

%% @doc An environment that will not find an accelerator plugin.
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
-spec environment(binary() | list()) -> {ok, reference()} | {error, binary()}.
environment(Dir) when is_list(Dir) ->
    environment(list_to_binary(Dir));
environment(Dir) when is_binary(Dir) ->
    tflite_beam_nif:litert_environment_new(Dir).

%% @doc
%% The signature keys of a model, in index order.
%%
%% A model with no named signatures still has one, and it comes back as the
%% empty key. Reading these needs the model but not a compile, so it is the
%% cheap way to find out what `new/3' can be asked for.
-spec signatures(reference(), binary() | list()) -> {ok, [binary()]} | {error, binary()}.
signatures(Env, Path) when is_list(Path) ->
    signatures(Env, list_to_binary(Path));
signatures(Env, Path) when is_binary(Path) ->
    tflite_beam_nif:litert_model_signatures(Env, Path).

%% @doc A compiled model on the CPU, with no profiling.
-spec new(reference(), binary() | list()) -> {ok, reference()} | {error, binary()}.
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
%% `profile/1'. Measured on `mobilenet_v2_1.0_224', 50 runs: on the CPU it cost
%% between 1.1x and 1.4x, and on the GPU nothing measurable, because there the
%% whole graph is one delegate node and there is no per-operator boundary to
%% time. Cheap enough to turn on when you want the answer; the events also
%% accumulate, so `reset_profile/1' is what keeps a long-lived model bounded.
-spec new(reference(), binary() | list(), opts()) -> {ok, reference()} | {error, binary()}.
new(Env, Path, Opts) when is_list(Path) ->
    new(Env, list_to_binary(Path), Opts);
new(Env, Path, Opts) when is_binary(Path), is_map(Opts) ->
    Accel = accelerator_set(maps:get(accelerators, Opts, [cpu])),
    Prec = precision_value(maps:get(precision, Opts, default)),
    Profile = case maps:get(profile, Opts, false) of true -> 1; false -> 0 end,
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
-spec profile(reference()) -> {ok, [map()]} | {error, binary()}.
profile(Model) ->
    tflite_beam_nif:litert_compiled_model_profile(Model).

%% @doc Hardware counters an accelerator chose to report, at detail level zero.
-spec metrics(reference()) -> {ok, [{binary(), term()}]} | {error, binary()}.
metrics(Model) ->
    metrics(Model, 0).

%% @doc
%% Hardware counters an accelerator chose to report.
%%
%% Usually `{ok, []}'. The two entries of an accelerator definition that would
%% fill these in may be null, and are null in both the plugins here and in
%% Google\'s own prebuilt GPU accelerator, so an empty list means nobody offered
%% anything rather than that something went wrong. Use `profile/1' for timings;
%% this is for counters a vendor backend exposes.
-spec metrics(reference(), non_neg_integer()) -> {ok, [{binary(), term()}]} | {error, binary()}.
metrics(Model, DetailLevel) when is_integer(DetailLevel), DetailLevel >= 0 ->
    tflite_beam_nif:litert_compiled_model_metrics(Model, DetailLevel).

%% @doc Forget the events recorded so far and keep recording.
-spec reset_profile(reference()) -> ok | {error, binary()}.
reset_profile(Model) ->
    tflite_beam_nif:litert_compiled_model_reset_profile(Model).

%% @doc
%% `profile/1' folded into per-operator totals.
%%
%% Returns a list of `{Tag, Count, MicrosecondsTotal}' sorted slowest first,
%% over the events that carry a duration.
-spec summarise_profile(reference()) -> {ok, [{binary(), pos_integer(), non_neg_integer()}]}
                                      | {error, binary()}.
summarise_profile(Model) ->
    case profile(Model) of
        {ok, Events} ->
            Timed = [E || E <- Events, timed(E)],
            Totals = lists:foldl(
                fun(E, Acc) ->
                    maps:update_with(maps:get(tag, E),
                                     fun({C, U}) -> {C + 1, U + maps:get(us, E)} end,
                                     {1, maps:get(us, E)}, Acc)
                end, #{}, Timed),
            {ok, lists:reverse(lists:keysort(3,
                [{Tag, C, U} || {Tag, {C, U}} <- maps:to_list(Totals)]))};
        Error ->
            Error
    end.

%% Telemetry markers are reported with 1 bsl 32 where a duration would go, and
%% event types above the operator ones are not operators at all.
timed(#{us := Us, type := Type}) ->
    Us < (1 bsl 32) andalso Type =< 8.

accelerator_set(List) when is_list(List) ->
    lists:foldl(fun(A, Acc) -> Acc bor accelerator_bit(A) end, 0, List).

accelerator_bit(cpu) -> 1;
accelerator_bit(gpu) -> 2;
accelerator_bit(npu) -> 4.

signature_index(_Env, _Path, Index) when is_integer(Index), Index >= 0 ->
    {ok, Index};
signature_index(Env, Path, Key) when is_list(Key) ->
    signature_index(Env, Path, list_to_binary(Key));
signature_index(Env, Path, Key) when is_binary(Key) ->
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

index_of(_Key, [], _N) -> not_found;
index_of(Key, [Key | _], N) -> N;
index_of(Key, [_ | Rest], N) -> index_of(Key, Rest, N + 1).

precision_value(default) -> 0;
precision_value(fp16) -> 1;
precision_value(fp32) -> 2;
precision_value(fp16_with_fp32_accum) -> 3.

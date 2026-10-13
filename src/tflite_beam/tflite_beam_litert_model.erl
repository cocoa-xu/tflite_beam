-module(tflite_beam_litert_model).

-export([inspect/2, rewrite/3]).
-export_type([source/0, edit/0]).

-type source() :: binary() | string() | {buffer, binary()}.
-type edit() :: #{subgraph := non_neg_integer(), operator := non_neg_integer(), options := map()}.

%% @doc Read subgraph connectivity and OneHot, SplitV and StableHLO composite options.
-spec inspect(reference(), source()) -> {ok, [map()]} | {error, binary()}.
inspect(Env, Source) ->
    with_source(Source, fun(Value) -> tflite_beam_nif:litert_model_inspect(Env, Value) end).

%% @doc Rewrite operator options in a private model and return a TFLite binary.
%% Edits refer to the indices returned by inspect/2. The source is never modified.
%% Tensor shapes and connectivity are preserved; the caller must keep them
%% consistent with the new options. This experimental API uses LiteRT's builder.
-spec rewrite(reference(), source(), [edit()]) -> {ok, binary()} | {error, binary()}.
rewrite(Env, Source, Edits) ->
    with_source(Source, fun(Value) -> tflite_beam_nif:litert_model_rewrite(Env, Value, Edits) end).

with_source(Source, Continue) when is_list(Source) ->
    try unicode:characters_to_binary(Source) of
        Binary when is_binary(Binary) -> with_source(Binary, Continue);
        _ -> {error, <<"invalid model path">>}
    catch
        error:badarg -> {error, <<"invalid model path">>}
    end;
with_source(Source, Continue) when is_binary(Source) ->
    case binary:match(Source, <<0>>) of
        nomatch -> Continue(Source);
        _ -> {error, <<"model path contains NUL">>}
    end;
with_source({buffer, Binary} = Source, Continue) when is_binary(Binary) -> Continue(Source);
with_source(_, _) -> {error, <<"expecting a model path or {buffer, binary model}">>}.

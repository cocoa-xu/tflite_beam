%% @doc
%% Build an interpreter capable of interpreting model.

-module(tflite_beam_interpreter_builder).
-export([
    new/2,
    build/2,
    set_num_threads/2,
    add_delegate/2, add_delegate/3
]).

-include("tflite_beam_records.hrl").

%% @doc New InterpreterBuilder
-spec new(#tflite_beam_flatbuffer_model{} | reference(), reference()) -> {ok, reference()} | {error, binary()}.
new(#tflite_beam_flatbuffer_model{ref = Model}, Resolver) when is_reference(Model) and is_reference(Resolver) ->
    new(Model, Resolver);
new(Model, Resolver) when is_reference(Model) and is_reference(Resolver) ->
    tflite_beam_nif:interpreter_builder_new(Model, Resolver).

%% @doc
%% Build the interpreter with the InterpreterBuilder.
%%
%% Note: all Interpreters should be built with the InterpreterBuilder,
%% which allocates memory for the Interpreter and does various set up
%% tasks so that the Interpreter can read the provided model.
%%
%% Returns `{ok, delegate_declined}' when a delegate added with
%% `on_decline => fallback' could not take the graph and the interpreter was
%% built without it. See `add_delegate/3'.
-spec build(reference(), reference()) -> ok | {ok, delegate_declined} | {error, binary()}.
build(Builder, Interpreter) when is_reference(Builder) and is_reference(Interpreter) ->
    case ensure_default_delegate(Builder) of
        ok ->
            tflite_beam_nif:interpreter_builder_build(Builder, Interpreter);
        {error, Reason} ->
            {error, Reason}
    end.

%% Left to itself TfLite applies XNNPACK lazily, inside allocate_tensors/1, with
%% a thread count nothing can reach and no way to say no. A resolver that
%% declines that -- which is the default -- gets an XNNPACK delegate attached
%% here instead, so the same acceleration happens somewhere it can be seen.
%%
%% Not attached when the caller has added a delegate of their own, and not where
%% XNNPACK was never compiled in, which is armv6 and armv7l.
ensure_default_delegate(Builder) ->
    case tflite_beam_nif:interpreter_builder_state(Builder) of
        {ok, {0, NumThreads, false}} ->
            attach_default_delegate(Builder, NumThreads);
        {ok, _} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

attach_default_delegate(Builder, NumThreads) ->
    case lists:member(xnnpack, tflite_beam_delegate:available()) of
        false ->
            ok;
        true ->
            %% set_num_threads/2 has to keep reaching XNNPACK, and an unset
            %% builder means one thread -- TfLite's kDefaultNumThreadpoolThreads,
            %% which is what the lazy delegate has been getting all along
            Threads = case NumThreads of
                -1 -> 1;
                N -> N
            end,
            case tflite_beam_delegate:xnnpack(#{num_threads => Threads}) of
                {ok, Delegate} ->
                    add_delegate(Builder, Delegate);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc Attach a delegate to the builder, with the default decline policy.
%%
%% Equivalent to `add_delegate(Builder, Delegate, #{})'.
-spec add_delegate(reference(), reference()) -> ok | {error, binary()}.
add_delegate(Builder, Delegate) when is_reference(Builder) and is_reference(Delegate) ->
    add_delegate(Builder, Delegate, #{}).

%% @doc
%% Attach a delegate to every interpreter this builder goes on to build.
%%
%% The delegate is applied in the order delegates were added, and it has to
%% outlive every interpreter built from this builder -- which is why there is no
%% way to detach or delete one. Holding the reference is not required: the
%% builder and each interpreter keep the delegate alive for as long as they need
%% it.
%%
%% ==== Keyword Parameters ====
%% @param on_decline What to do when a delegate reports that it cannot take the
%% graph, but leaves the graph runnable -- a static-shape delegate meeting a
%% dynamic tensor, say. TfLite discards the whole interpreter in that case.
%% <ul>
%%   <li>`error' (the default) -- the decline surfaces as `{error, Reason}' from
%%   `build/2'.</li>
%%   <li>`fallback' -- `build/2' builds again without the delegates that were
%%   added with this policy, and answers `{ok, delegate_declined}'. Only a
%%   decline is retried; every other failure still fails.</li>
%% </ul>
%%
%% Note that an interpreter, and any delegate attached to it, belongs to one
%% process at a time. Nothing here is serialised for you.
-spec add_delegate(reference(), reference(), map()) -> ok | {error, binary()}.
add_delegate(Builder, Delegate, Opts) when is_reference(Builder), is_reference(Delegate), is_map(Opts) ->
    case validate_delegate_opts(Opts) of
        {ok, OnDecline} ->
            tflite_beam_nif:interpreter_builder_add_delegate(Builder, Delegate, OnDecline);
        {error, Reason} ->
            {error, Reason}
    end.

validate_delegate_opts(Opts) ->
    case maps:keys(Opts) -- [on_decline] of
        [] ->
            case maps:get(on_decline, Opts, error) of
                error -> {ok, error};
                fallback -> {ok, fallback};
                Other -> {error, unicode:characters_to_binary(
                    io_lib:format("expecting on_decline to be either error or fallback, got ~p", [Other]))}
            end;
        Unknown ->
            {error, unicode:characters_to_binary(
                io_lib:format("unknown delegate option(s): ~p", [Unknown]))}
    end.

%% @doc
%% Sets the number of CPU threads to use for the interpreter.
%% Returns `ok' on success, `{error, Reason}' on error.
%%
%% `NumThreads' follows TfLite: `-1' asks the runtime to choose, `0' means the
%% same as `1', and anything below `-1' is refused. The spec said
%% `pos_integer()', which excluded both of the first two.
-spec set_num_threads(reference(), integer()) -> ok | {error, binary()}.
set_num_threads(Builder, NumThreads) when is_reference(Builder) and is_integer(NumThreads) ->
    tflite_beam_nif:interpreter_builder_set_num_threads(Builder, NumThreads).

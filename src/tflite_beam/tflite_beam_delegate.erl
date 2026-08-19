%% @doc
%% TfLite delegates: graph accelerators an interpreter builder can be given.

-module(tflite_beam_delegate).
-export([
    available/0,
    xnnpack/0, xnnpack/1
]).

%% @doc
%% Which delegate kinds this build of the library can construct.
%%
%% This answers "was it compiled in", not "is a device present" -- the two have
%% different answers on the same binary. Whether a device is there is discovered
%% by trying to create the delegate and getting `{error, Reason}' back.
%%
%% Note that an interpreter, and any delegate attached to it, belongs to one
%% process at a time. See `tflite_beam_interpreter_builder:add_delegate/2'.
-spec available() -> list(atom()).
available() ->
    tflite_beam_nif:delegate_available().

%% @doc An XNNPACK delegate with XNNPACK's own defaults.
-spec xnnpack() -> {ok, reference()} | {error, binary()}.
xnnpack() ->
    xnnpack(#{}).

%% @doc
%% Create an XNNPACK delegate.
%%
%% XNNPACK is compiled into every target except armv6 and armv7l; `available/0'
%% is what says so, and this returns `{error, Reason}' where it is not.
%%
%% ==== Keyword Parameters ====
%% @param num_threads Size of the delegate's thread pool. Zero or less means no
%% thread pool at all, which is XNNPACK's own default and therefore this one.
%% Note that this is not the same knob as
%% `tflite_beam_interpreter_builder:set_num_threads/2', which drives TfLite's CPU
%% backend: a delegate created here carries its own pool.
%% @param flags A list of atoms, added to XNNPACK's defaults rather than
%% replacing them -- TfLite spells turning a default off as its own flag, such as
%% `disable_subgraph_reshaping'. One of `qs8', `qu8', `force_fp16',
%% `dynamic_fully_connected', `variable_operators',
%% `transient_indirection_buffer', `enable_latest_operators',
%% `enable_subgraph_reshaping', `slow_consistent_arithmetic',
%% `disable_subgraph_reshaping' or `disable_dynamically_quantized_ops'.
%% @param weight_cache_file_path Where to keep XNNPACK's cache of packed weights,
%% which is read if it exists and written if it does not.
%%
%% Note that an interpreter, and any delegate attached to it, belongs to one
%% process at a time.
-spec xnnpack(map()) -> {ok, reference()} | {error, binary()}.
xnnpack(Opts) when is_map(Opts) ->
    case maps:keys(Opts) -- [num_threads, flags, weight_cache_file_path] of
        [] ->
            xnnpack(maps:get(num_threads, Opts, 0),
                    maps:get(flags, Opts, []),
                    maps:get(weight_cache_file_path, Opts, nil));
        Unknown ->
            {error, unicode:characters_to_binary(
                io_lib:format("unknown xnnpack option(s): ~p", [Unknown]))}
    end.

xnnpack(NumThreads, _Flags, _Path) when not is_integer(NumThreads) ->
    {error, <<"expecting num_threads to be an integer">>};
xnnpack(_NumThreads, Flags, _Path) when not is_list(Flags) ->
    {error, <<"expecting flags to be a list of atoms">>};
xnnpack(NumThreads, Flags, Path) when is_list(Path) ->
    xnnpack(NumThreads, Flags, unicode:characters_to_binary(Path));
xnnpack(NumThreads, Flags, Path) when Path =:= nil; is_binary(Path) ->
    tflite_beam_nif:delegate_xnnpack_new(NumThreads, Flags, Path);
xnnpack(_NumThreads, _Flags, _Path) ->
    {error, <<"expecting weight_cache_file_path to be a string or nil">>}.

%% @doc
%% TfLite delegates: graph accelerators an interpreter builder can be given.

-module(tflite_beam_delegate).
-export([
    available/0,
    xnnpack/0, xnnpack/1,
    external/1, external/2
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

%% @doc A delegate from a plugin library, with no options.
-spec external(binary() | list()) -> {ok, reference()} | {error, binary()}.
external(LibraryPath) ->
    external(LibraryPath, #{}).

%% @doc
%% Load a delegate from a shared library that implements TfLite's delegate
%% plugin interface -- `tflite_plugin_create_delegate' and
%% `tflite_plugin_destroy_delegate'. That covers Edge TPU, a GPU delegate built
%% elsewhere, and any vendor delegate, without this library having to know
%% anything about them.
%%
%% The path is resolved to an absolute one before loading, because the loader is
%% asked for exactly the file named: a bare `libfoo.so' would otherwise be looked
%% up along the system search path, which is not where anyone means.
%%
%% Options are handed to the plugin as strings, since that is the whole of the
%% plugin ABI -- atoms and integers are converted, and at most 256 pairs fit.
%% What the keys mean is the plugin's business.
%%
%% ```
%% {ok, Delegate} = tflite_beam_delegate:external("/opt/lib/libvendor_delegate.so",
%%                                                #{device => 0, precision => fp16}),
%% ok = tflite_beam_interpreter_builder:add_delegate(Builder, Delegate).
%% '''
%%
%% The library is never unloaded, which is what TfLite does too -- an interpreter
%% outliving a closed plugin would be considerably worse than a loader reference
%% held to the end of the VM's life.
%%
%% Note that an interpreter, and any delegate attached to it, belongs to one
%% process at a time.
-spec external(binary() | list(), map()) -> {ok, reference()} | {error, binary()}.
external(LibraryPath, Options) when is_list(LibraryPath) ->
    external(unicode:characters_to_binary(LibraryPath), Options);
external(LibraryPath, Options) when is_binary(LibraryPath), is_map(Options) ->
    case filelib:is_regular(LibraryPath) of
        false ->
            {error, <<"no such delegate library: ", LibraryPath/binary>>};
        true ->
            case external_options(Options) of
                {ok, Pairs} ->
                    tflite_beam_nif:delegate_external_new(filename:absname(LibraryPath), Pairs);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% The plugin ABI stops at 256 pairs and reports nothing useful past it, so the
%% refusal happens here where it can say why.
external_options(Options) when map_size(Options) > 256 ->
    {error, <<"a delegate takes at most 256 options">>};
external_options(Options) ->
    try
        {ok, [{as_string(K), as_string(V)} || {K, V} <- maps:to_list(Options)]}
    catch
        throw:{not_a_string, What} ->
            {error, unicode:characters_to_binary(
                io_lib:format("delegate options must be atoms, integers or strings, got ~p", [What]))}
    end.

as_string(Value) when is_binary(Value) ->
    Value;
as_string(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
as_string(Value) when is_integer(Value) ->
    integer_to_binary(Value);
as_string(Value) when is_list(Value) ->
    case unicode:characters_to_binary(Value) of
        Binary when is_binary(Binary) -> Binary;
        _ -> throw({not_a_string, Value})
    end;
as_string(Value) ->
    throw({not_a_string, Value}).

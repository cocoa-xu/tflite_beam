%% @doc What this build of the library is.
-module(tflite_beam).

-export([
    tflite_version/0,
    tflite_runtime_version/0,
    tflite_extension_apis_version/0,
    tflite_schema_version/0,
    xnnpack_max_tensor_dims/0,
    source_tree/0
]).

%% @doc The version of the TfLite sources this library was built from, e.g.
%% `<<"2.21.0">>'.
%%
%% **This is the one to compare a delegate plugin against.** Open-source TfLite
%% offers no binary-stable delegate interface: a plugin loaded through
%% {@link tflite_beam_delegate:external/1} must have been built from the same
%% release, and a mismatch is undefined behaviour rather than an error.
-spec tflite_version() -> binary().
tflite_version() ->
    tflite_beam_nif:tflite_version().

%% @doc What the linked TfLite runtime reports about itself.
%%
%% Kept for diagnosis, and deliberately not what {@link tflite_version/0}
%% returns: `lite/version.h' carries a hand-maintained version number that
%% upstream forgets to bump -- the 2.21.0 tree still says `2.19.0' -- and it
%% only applies when the build system injects nothing, which Bazel does and
%% CMake does not. Two builds from different releases are indistinguishable
%% through this value.
-spec tflite_runtime_version() -> binary().
tflite_runtime_version() ->
    tflite_beam_nif:tflite_runtime_version().

%% @doc The version of the APIs for extending TfLite with custom ops and
%% delegates, as the runtime reports it.
%%
%% Narrower in scope than the runtime version -- it covers `c_api_opaque.h',
%% `common.h', `builtin_op_data.h' and `builtin_ops.h' -- but it is derived from
%% the same stale number, so the caveat on {@link tflite_runtime_version/0}
%% applies here too.
-spec tflite_extension_apis_version() -> binary().
tflite_extension_apis_version() ->
    tflite_beam_nif:tflite_extension_apis_version().

%% @doc The major schema version this runtime reads model files at.
%%
%% Unlike the version strings above this one is real: it is defined next to the
%% schema it describes. A model serialised at a different schema version may not
%% load.
-spec tflite_schema_version() -> integer().
tflite_schema_version() ->
    tflite_beam_nif:tflite_schema_version().

%% @doc The widest tensor the delegate in this build can describe, or `nil' if
%% no delegate here imposes a width.
%%
%% XNNPACK holds a tensor's dimensions in a fixed-width array and checks the
%% count once, when it decides whether to take a graph. Nothing rechecks it
%% afterwards, so growing an already-delegated tensor past this many dimensions
%% would write the dimensions you passed in past the end of that array.
%% {@link tflite_beam_interpreter:resize_input_tensor/3} and its signature
%% runner counterpart refuse that one transition rather than perform it.
%%
%% A tensor that is already wider than this was refused by the delegate to begin
%% with, was therefore never delegated, and can still be reshaped freely. The
%% armv6 and armv7l builds carry no XNNPACK and answer `nil', where nothing is
%% refused.
-spec xnnpack_max_tensor_dims() -> integer() | nil.
xnnpack_max_tensor_dims() ->
    tflite_beam_nif:xnnpack_max_tensor_dims().

%% @doc Which source tree this shared object was built from.
%%
%% Answers `litert\'. There is no other answer: the C++ side names a type that
%% only LiteRT\'s schema defines, so a binary built against anything else does
%% not compile, and a release from before the move has no such function at all.
%%
%% Worth asking in a test rather than trusting the build, because the wrong tree
%% does not announce itself and a stale precompiled artifact looks exactly like a
%% fresh one from the outside.
-spec source_tree() -> litert.
source_tree() ->
    tflite_beam_nif:tflite_source_tree().

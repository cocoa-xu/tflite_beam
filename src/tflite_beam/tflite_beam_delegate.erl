%% @doc
%% TfLite delegates: graph accelerators an interpreter builder can be given.

-module(tflite_beam_delegate).
-export([
    available/0
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

%% @doc
%% ErrorReporter to provide reporting destinations.

-module(tflite_beam_error_reporter).
-export([
    default_error_reporter/0
]).

-include("tflite_beam_records.hrl").

%% @doc TFLite's own error reporter, the one it uses when none is given.
-spec default_error_reporter() -> #tflite_beam_error_reporter{} | {error, binary()}.
default_error_reporter() ->
    case tflite_beam_nif:error_reporter_default_error_reporter() of
        {ok, ErrorReporter} ->
            #tflite_beam_error_reporter{ref = ErrorReporter};
        {error, Reason} ->
            {error, Reason}
    end.

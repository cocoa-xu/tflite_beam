%% @moduledoc
%% ErrorReporter to provide reporting destinations.

-module(tflite_beam_error_reporter).
-export([
    default_error_reporter/0
]).

-include("tflite_beam_records.hrl").

default_error_reporter() ->
    case tflite_beam_nif:error_reporter_default_error_reporter() of
        {ok, ErrorReporter} ->
            #tflite_beam_error_reporter{ref = ErrorReporter};
        {error, Reason} ->
            {error, Reason}
    end.

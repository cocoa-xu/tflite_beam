%% @doc
%% An RAII object that represents a read-only tflite model, copied from disk, or
%% mmapped.

-module(tflite_beam_flatbuffer_model).
-export([
    build_from_file/1, build_from_file/2, 
    verify_and_build_from_file/1, verify_and_build_from_file/2,
    build_from_buffer/1, build_from_buffer/2,
    verify_and_build_from_buffer/1, verify_and_build_from_buffer/2,
    initialized/1,
    error_reporter/1,
    get_minimum_runtime/1,
    read_all_metadata/1,
    list_associated_files/1,
    get_associated_file/2
]).

-include("tflite_beam_records.hrl").

%% @doc Build model from a given .tflite file
%%
%% ==== Positional Parameters ====
%% @param Filename Path to the .tflite file.
%%
%% Note that if the tensorflow-lite library was compiled with `TFLITE_MCU',
%% then this function will always have return type `{error, binary()}'.
-spec build_from_file(list() | binary()) -> #tflite_beam_flatbuffer_model{} | {error, binary()}.
build_from_file(Filename) when is_list(Filename) ->
    build_from_file(unicode:characters_to_binary(Filename), []);
build_from_file(Filename) when is_binary(Filename) ->
    build_from_file(Filename, []).

%% @doc Build model from a given .tflite file
%%
%% ==== Positional Parameters ====
%% @param Filename Path to the .tflite file.
%%
%% ==== Keyword Parameters ====
%% @param error_reporter Error reporter.
%%
%% Note that if the tensorflow-lite library was compiled with `TFLITE_MCU',
%% then this function will always have return type `{error, binary()}'.
-spec build_from_file(list() | binary(), list()) -> #tflite_beam_flatbuffer_model{} | {error, binary()}.
build_from_file(Filename, Opts) when is_list(Filename), is_list(Opts) ->
    build_from_file(unicode:characters_to_binary(Filename), Opts);
build_from_file(Filename, Opts) when is_binary(Filename), is_list(Opts) ->
    ErrorReporter = case proplists:get_value(error_reporter, Opts, nil) of
        #tflite_beam_error_reporter{ref = ErrorReporterRef} ->
            ErrorReporterRef;
        ErrorReporter_ when is_reference(ErrorReporter_) ->
            ErrorReporter_;
        nil ->
            nil
        end,
    case tflite_beam_nif:flatbuffer_model_build_from_file(Filename, ErrorReporter) of
        {ok, Model} ->
            Initialized = tflite_beam_nif:flatbuffer_model_initialized(Model),
            MinimumRuntime = tflite_beam_nif:flatbuffer_model_get_minimum_runtime(Model),
            #tflite_beam_flatbuffer_model{initialized = Initialized, minimum_runtime = MinimumRuntime, ref = Model};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Verifies whether the content of the file is legit, then builds a model
%% based on the file.
%%
%% ==== Positional Parameters ====
%% @param Filename Path to the .tflite file.
%%
%% @return `invalid' in case of failure.
-spec verify_and_build_from_file(list() | binary()) -> #tflite_beam_flatbuffer_model{} | invalid | {error, binary()}.
verify_and_build_from_file(Filename) when is_list(Filename) ->
    verify_and_build_from_file(unicode:characters_to_binary(Filename), []);
verify_and_build_from_file(Filename) when is_binary(Filename) ->
    verify_and_build_from_file(Filename, []).

%% @doc
%% Verifies whether the content of the file is legit, then builds a model
%% based on the file.
%%
%% ==== Positional Parameters ====
%% @param Filename Path to the .tflite file.
%%
%% ==== Keyword Parameters ====
%% @param error_reporter Error reporter.
%%
%% @return `invalid' in case of failure.
-spec verify_and_build_from_file(list() | binary(), list()) -> #tflite_beam_flatbuffer_model{} | invalid | {error, binary()}.
verify_and_build_from_file(Filename, Opts) when is_list(Filename), is_list(Opts) ->
    verify_and_build_from_file(unicode:characters_to_binary(Filename), Opts);
verify_and_build_from_file(Filename, Opts) when is_binary(Filename), is_list(Opts) ->
    ErrorReporter = case proplists:get_value(error_reporter, Opts, nil) of
        #tflite_beam_error_reporter{ref = ErrorReporterRef} ->
            ErrorReporterRef;
        ErrorReporter_ when is_reference(ErrorReporter_) ->
            ErrorReporter_;
        nil ->
            nil
        end,
    case tflite_beam_nif:flatbuffer_model_verify_and_build_from_file(Filename, ErrorReporter) of
        {ok, Model} ->
            Initialized = tflite_beam_nif:flatbuffer_model_initialized(Model),
            MinimumRuntime = tflite_beam_nif:flatbuffer_model_get_minimum_runtime(Model),
            #tflite_beam_flatbuffer_model{initialized = Initialized, minimum_runtime = MinimumRuntime, ref = Model};
        invalid ->
            invalid;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Verifies whether the buffer is legit, then builds a model from it
%%
%% ==== Positional Parameters ====
%% @param Buffer The content of a .tflite file.
%%
%% @return `invalid' in case of failure.
-spec verify_and_build_from_buffer(binary()) -> #tflite_beam_flatbuffer_model{} | invalid | {error, binary()}.
verify_and_build_from_buffer(Buffer) ->
    verify_and_build_from_buffer(Buffer, []).

%% @doc Verifies whether the buffer is legit, then builds a model from it
%%
%% ==== Positional Parameters ====
%% @param Buffer The content of a .tflite file.
%%
%% ==== Keyword Parameters ====
%% @param error_reporter Error reporter.
%%
%% @return `invalid' in case of failure.
-spec verify_and_build_from_buffer(binary(), list()) -> #tflite_beam_flatbuffer_model{} | invalid | {error, binary()}.
verify_and_build_from_buffer(Buffer, Opts) when is_binary(Buffer), is_list(Opts) ->
    ErrorReporter = case proplists:get_value(error_reporter, Opts, nil) of
        #tflite_beam_error_reporter{ref = ErrorReporterRef} ->
            ErrorReporterRef;
        ErrorReporter_ when is_reference(ErrorReporter_) ->
            ErrorReporter_;
        nil ->
            nil
        end,
    case tflite_beam_nif:flatbuffer_model_verify_and_build_from_buffer(Buffer, ErrorReporter) of
        {ok, Model} ->
            Initialized = tflite_beam_nif:flatbuffer_model_initialized(Model),
            MinimumRuntime = tflite_beam_nif:flatbuffer_model_get_minimum_runtime(Model),
            #tflite_beam_flatbuffer_model{initialized = Initialized, minimum_runtime = MinimumRuntime, ref = Model};
        invalid ->
            invalid;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Build model from caller owned memory buffer
-spec build_from_buffer(binary()) -> #tflite_beam_flatbuffer_model{} | {error, binary()}.
build_from_buffer(Buffer) ->
    build_from_buffer(Buffer, []).

%% @doc Build model from caller owned memory buffer
-spec build_from_buffer(binary(), list()) -> #tflite_beam_flatbuffer_model{} | {error, binary()}.
build_from_buffer(Buffer, Opts) ->
    ErrorReporter = case proplists:get_value(error_reporter, Opts, nil) of
        #tflite_beam_error_reporter{ref = ErrorReporterRef} ->
            ErrorReporterRef;
        ErrorReporter_ when is_reference(ErrorReporter_) ->
            ErrorReporter_;
        nil ->
            nil
        end,
    case tflite_beam_nif:flatbuffer_model_build_from_buffer(Buffer, ErrorReporter) of
        {ok, Model} ->
            Initialized = tflite_beam_nif:flatbuffer_model_initialized(Model),
            MinimumRuntime = tflite_beam_nif:flatbuffer_model_get_minimum_runtime(Model),
            #tflite_beam_flatbuffer_model{initialized = Initialized, minimum_runtime = MinimumRuntime, ref = Model};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Check whether current model has been initialized
-spec initialized(#tflite_beam_flatbuffer_model{} | reference()) -> boolean() | {error, binary()}.
initialized(#tflite_beam_flatbuffer_model{ref = Model}) when is_reference(Model) ->
    initialized(Model);
initialized(Model) when is_reference(Model) ->
    tflite_beam_nif:flatbuffer_model_initialized(Model).

%% @doc Get the error report of the current FlatBuffer model.
-spec error_reporter(#tflite_beam_flatbuffer_model{} | reference()) -> #tflite_beam_error_reporter{} | {error, binary()}.
error_reporter(#tflite_beam_flatbuffer_model{ref = Model}) when is_reference(Model) ->
    error_reporter(Model);
error_reporter(Model) when is_reference(Model) ->
    case tflite_beam_nif:flatbuffer_model_error_reporter(Model) of
        {ok, ErrorReporter} ->
            #tflite_beam_error_reporter{ref = ErrorReporter};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Returns the minimum runtime version from the flatbuffer. This runtime
%% version encodes the minimum required interpreter version to run the
%% flatbuffer model. If the minimum version can't be determined, an empty
%% string will be returned.
%%
%% Note that the returned minimum version is a lower-bound but not a strict
%% lower-bound; ops in the graph may not have an associated runtime version,
%% in which case the actual required runtime might be greater than the
%% reported minimum.
-spec get_minimum_runtime(#tflite_beam_flatbuffer_model{} | reference()) -> binary() | {error, binary()}.
get_minimum_runtime(#tflite_beam_flatbuffer_model{ref = Model}) when is_reference(Model) ->
    get_minimum_runtime(Model);
get_minimum_runtime(Model) when is_reference(Model) ->
    case tflite_beam_nif:flatbuffer_model_get_minimum_runtime(Model) of
        MinimumRuntime when is_binary(MinimumRuntime) ->
            MinimumRuntime;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Return model metadata as a mapping of name and buffer strings.
%% 
%% See Metadata table in TFLite schema.
-spec read_all_metadata(#tflite_beam_flatbuffer_model{} | reference()) -> map() | {error, binary()}.
read_all_metadata(#tflite_beam_flatbuffer_model{ref = Model}) when is_reference(Model) ->
    read_all_metadata(Model);
read_all_metadata(Model) when is_reference(Model) ->
    case tflite_beam_nif:flatbuffer_model_read_all_metadata(Model) of
        Metadata when is_map(Metadata) ->
            Metadata;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Get a list of all associated file(s) in a TFLite model file
-spec list_associated_files(binary()) -> list(binary()) | {error, binary()}.
list_associated_files(Buffer) when is_binary(Buffer) ->
    case zip:table(Buffer) of
        {ok, Entries} ->
            lists:filtermap(
                fun(Entry) ->
                    case Entry of
                        {zip_file, Filename, _, _, _, _} ->
                            {true, unicode:characters_to_binary(Filename)};
                        _ ->
                            false
                    end
                end,
                Entries);
        {error, Reason} ->
            {error, Reason}
    end.

%% is_list/1 cannot tell "labels.txt" from [<<"labels.txt">>]: an Erlang string
%% is a list, so the many-files branch used to walk it character by character and
%% raise badarg formatting the integer $l into a message. A string is one name.
is_list_of_names(Names) when is_list(Names) ->
    lists:all(fun(N) -> is_binary(N) orelse (is_list(N) andalso io_lib:printable_unicode_list(N)) end,
              Names) andalso not io_lib:printable_unicode_list(Names);
is_list_of_names(_) ->
    false.

%% @doc Get associated file(s) from a FlatBuffer model
%% TODO: support `list(list())' as `Filename', accepts a list of Erlang strings
-spec get_associated_file(binary(), list(binary()) | binary()) -> map() | binary() | {error, binary()}.
get_associated_file(Buffer, Filename) when is_binary(Buffer) and (is_binary(Filename) or is_list(Filename)) ->
    case list_associated_files(Buffer) of
        AssociatedFiles when is_list(AssociatedFiles) ->
            case zip:zip_open(Buffer, [memory]) of 
                {ok, Z} ->
                    FileContent = case is_list_of_names(Filename) of
                        true ->
                            MapItems = lists:map(
                                fun(F) ->
                                    %% a name in the list may be an Erlang string while the archive
                                    %% names are binaries. Without this the member test never matched,
                                    %% so every string inside a list came back as "cannot find" while
                                    %% the same string on its own worked.
                                    Name = case is_list(F) of
                                        true -> unicode:characters_to_binary(F);
                                        false -> F
                                    end,
                                    case lists:member(Name, AssociatedFiles) of
                                        true ->
                                            {F, get_associated_file_impl(Z, Name)};
                                        false ->
                                            Reason = io_lib:format("cannot find associated file `~ts`", [Name]),
                                            {F, {error, unicode:characters_to_binary(Reason)}}
                                    end
                                end,
                                Filename
                            ),
                            maps:from_list(MapItems);
                        false ->
                            One = case is_list(Filename) of
                                true -> unicode:characters_to_binary(Filename);
                                false -> Filename
                            end,
                            case lists:member(One, AssociatedFiles) of
                                true ->
                                    get_associated_file_impl(Z, One);
                                false ->
                                    Reason = io_lib:format("cannot find associated file `~ts`", [One]),
                                    {error, unicode:characters_to_binary(Reason)}
                            end
                    end,
                    zip:zip_close(Z),
                    FileContent;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

get_associated_file_impl(Z, Filename) ->
    case zip:zip_get(Filename, Z) of 
        {ok, {_, Content}} ->
            Content;
        {error, Reason} ->
            {error, Reason}
    end.

-module(tflite_beam_private_utils_unicode_data).
-export([
    get_puncuation_list_from_unicode_data/1,
    punctuation_set/1,
    release_memory/0
]).
-define(PUNCTUATION_SET, {?MODULE, punctuation_set}).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).
-record(state, {
    unicode_data_file = undefined,
    puncuation_list = []
}).

%% gen_server:call's own five seconds is not a budget anyone chose here. The
%% work behind it is one pass over a 1.9MB table, 105ms on a developer machine,
%% and this library ships precompiled binaries for armv6 and riscv64, where the
%% same pass is slower by a factor nobody has measured. Giving up on it turns a
%% slow board into a crash and buys nothing back.
-define(PARSE_TIMEOUT, 30000).

get_puncuation_list_from_unicode_data(UnicodeDataFile) ->
    ServerPid = get_running_instance(true),
    gen_server:call(ServerPid, {get_puncuation_list, UnicodeDataFile}, ?PARSE_TIMEOUT).

%% Membership answered from a term the caller already holds, rather than from a
%% call into this process. is_punctuation/1 runs once per code point, and the
%% round trip plus the path lookup that fed it cost 5.6us of the 5.6us a
%% character took to tokenize: the tokenizing itself did not register.
-spec punctuation_set(fun(() -> file:name_all())) -> map().
punctuation_set(UnicodeDataFileFun) when is_function(UnicodeDataFileFun, 0) ->
    File = UnicodeDataFileFun(),
    %% Keyed by the file it was read from. Answering from the cache whatever file
    %% the caller named would make the argument a decoration, and a caller asking
    %% for a different table would silently get this one.
    case persistent_term:get(?PUNCTUATION_SET, undefined) of
        {File, Set} ->
            Set;
        _ ->
            case gen_server:call(get_running_instance(true),
                                 {get_puncuation_set, File}, ?PARSE_TIMEOUT) of
                {error, Reason} ->
                    %% is_punctuation/1 answers a boolean and has nowhere to put a
                    %% reason, and guessing false would quietly move word boundaries
                    %% instead of failing. The table ships with the library, so one
                    %% that cannot be read is a broken install: name the file and the
                    %% reason rather than letting the comprehension below raise
                    %% bad_generator over the error tuple.
                    erlang:error({unicode_data_unavailable, File, Reason});
                Set when is_map(Set) ->
                    Set
            end
    end.

%% The server goes first. gen_server:stop/1 waits for the call in flight, and
%% that call is the only thing that publishes, so nothing can put the table back
%% after the erase below.
release_memory() ->
    case get_running_instance(false) of
        undefined ->
            ok;
        ServerPid when is_pid(ServerPid) ->
            gen_server:stop(ServerPid)
    end,
    _ = persistent_term:erase(?PUNCTUATION_SET),
    ok.

get_running_instance(CreateIfNotRunning) ->
    case erlang:whereis(?MODULE) of
        undefined ->
            if 
                CreateIfNotRunning ->
                    %% Two first callers can both see undefined here. The loser
                    %% used to crash on {error, {already_started, Pid}}, and the
                    %% winner left the server linked to whichever process
                    %% happened to arrive first.
                    case gen_server:start({local, ?MODULE}, ?MODULE, [], []) of
                        {ok, Pid} -> Pid;
                        {error, {already_started, Pid}} -> Pid
                    end;
                true ->
                    undefined
            end;
        Pid ->
            Pid
    end.

init(_) ->
    {ok, #state{}}.

%% Keyed by the file, like the set above it. Once this had parsed anything it
%% answered with that for every later file it was handed, so a caller naming a
%% different table was given the first one without being told.
%% Built and published from inside this process, so release_memory/0 cannot land
%% between a caller reading the table and putting it away. A caller doing its own
%% put could be descheduled after the erase and bring the cache back from the
%% dead, with release_memory/0 having already answered ok.
handle_call({get_puncuation_set, UnicodeDataFile}, From, State) ->
    case handle_call({get_puncuation_list, UnicodeDataFile}, From, State) of
        {reply, {error, Reason}, NewState} ->
            {reply, {error, Reason}, NewState};
        {reply, List, NewState} when is_list(List) ->
            Set = maps:from_list([{CodePoint, []} || CodePoint <- List]),
            persistent_term:put(?PUNCTUATION_SET, {UnicodeDataFile, Set}),
            {reply, Set, NewState}
    end;
handle_call({get_puncuation_list, UnicodeDataFile}, _From, State) ->
    Cached = State#state.unicode_data_file =:= UnicodeDataFile,
    case State#state.puncuation_list of
        _ when not Cached ->
            %% A missing file used to kill the server through the badmatch, and
            %% the descriptor was never closed on the way out either.
            case file:open(UnicodeDataFile, [read, raw]) of
                {ok, FileDescriptor} ->
                    Result = read_from_unicode_data(FileDescriptor, #{}),
                    _ = file:close(FileDescriptor),
                    case Result of
                        {ok, PuncuationList} ->
                            {reply, PuncuationList,
                             State#state{unicode_data_file = UnicodeDataFile,
                                         puncuation_list = PuncuationList}};
                        {error, Reason} ->
                            %% and a read error partway through used to be taken
                            %% for the end of the file, so a truncated table was
                            %% cached and every later caller got it
                            {reply, {error, Reason}, State}
                    end;
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        PuncuationList when is_list(PuncuationList) ->
            {reply, PuncuationList, State}        
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

read_from_unicode_data(FileDescriptor, TypeAcc) ->
    case file:read_line(FileDescriptor) of
        {ok, Line} ->
            case process_unicode_data_line(Line, TypeAcc) of
                {ok, Updated} -> read_from_unicode_data(FileDescriptor, Updated);
                {error, Reason} -> {error, Reason}
            end;
        eof ->
            {ok, lists:flatten([lists:reverse(V) || V <- maps:values(TypeAcc)])};
        {error, Reason} ->
            {error, Reason}
    end.

%% A missing file was already answered rather than matched against; a file that
%% is present but wrong was not. Every line here was destructured as though it
%% had its fields, so one short line, one stray newline in the middle, or a
%% partly written copy on a flash card took the server down and the caller with
%% it. A blank line is nothing to report. A line with content but without the
%% three fields a row is made of means the table is not the table, and saying so
%% beats caching half of it: the punctuation set decides where words split, and
%% a caller cannot tell a short answer from a right one.
process_unicode_data_line(Line, TypeAcc) ->
    BinaryLine = unicode:characters_to_binary(Line),
    case binary:split(BinaryLine, <<";">>, [global]) of
        [CodePoint, _Name, Type | _Rest] ->
            case accumlate_type(CodePoint, Type, TypeAcc) of
                {error, Field} ->
                    {error, iolist_to_binary(
                        ["unicode data row has no code point in its first field: ",
                         io_lib:format("~p", [Field])])};
                Updated ->
                    {ok, Updated}
            end;
        _Short ->
            case string:trim(BinaryLine) of
                <<>> -> {ok, TypeAcc};
                Trimmed ->
                    {error, iolist_to_binary(
                        ["unicode data line is not a row: ", io_lib:format("~p", [Trimmed])])}
            end
    end.

%% The first field is read as hexadecimal, and a row whose first field is not
%% hexadecimal, or is empty, ended the process through list_to_integer/2. Having
%% the right number of fields was checked; having a code point in the first one
%% was not, so the check stopped one layer short of the data it was guarding.
accumlate_type(CodePoint, Type, TypeAcc) ->
    case Type of
        <<"P", _>> ->
            case code_point_of(CodePoint) of
                {ok, Value} -> accumlate_value(Value, Type, TypeAcc);
                error -> {error, CodePoint}
            end;
        _ ->
            TypeAcc
    end.

code_point_of(Field) ->
    try erlang:list_to_integer(unicode:characters_to_list(Field), 16) of
        Value when Value >= 0, Value =< 16#10FFFF -> {ok, Value};
        _OutOfRange -> error
    catch
        error:badarg -> error
    end.

accumlate_value(Value, Type, TypeAcc) ->
    case maps:is_key(Type, TypeAcc) of
        true -> maps:update(Type, [Value | maps:get(Type, TypeAcc)], TypeAcc);
        false -> maps:put(Type, [Value], TypeAcc)
    end.

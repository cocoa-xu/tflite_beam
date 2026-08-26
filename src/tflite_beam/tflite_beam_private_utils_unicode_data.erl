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
    case persistent_term:get(?PUNCTUATION_SET, undefined) of
        undefined ->
            List = get_puncuation_list_from_unicode_data(UnicodeDataFileFun()),
            Set = maps:from_list([{CodePoint, []} || CodePoint <- List]),
            persistent_term:put(?PUNCTUATION_SET, Set),
            Set;
        Set ->
            Set
    end.

release_memory() ->
    _ = persistent_term:erase(?PUNCTUATION_SET),
    case get_running_instance(false) of
        undefined ->
            ok;
        ServerPid when is_pid(ServerPid) ->
            gen_server:stop(ServerPid)
    end.

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

handle_call({get_puncuation_list, UnicodeDataFile}, _From, State) ->
    case State#state.puncuation_list of
        [] ->
            %% A missing file used to kill the server through the badmatch, and
            %% the descriptor was never closed on the way out either.
            case file:open(UnicodeDataFile, [read, raw]) of
                {ok, FileDescriptor} ->
                    Result = read_from_unicode_data(FileDescriptor, #{}),
                    _ = file:close(FileDescriptor),
                    case Result of
                        {ok, PuncuationList} ->
                            {reply, PuncuationList,
                             State#state{puncuation_list = PuncuationList}};
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
            {ok, lists:flatten(maps:values(TypeAcc))};
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
            {ok, accumlate_type(CodePoint, Type, TypeAcc)};
        _Short ->
            case string:trim(BinaryLine) of
                <<>> -> {ok, TypeAcc};
                Trimmed ->
                    {error, iolist_to_binary(
                        ["unicode data line is not a row: ", io_lib:format("~p", [Trimmed])])}
            end
    end.

accumlate_type(CodePoint, Type, TypeAcc) ->
    case Type of
        <<"P", _>> ->
            Value = erlang:list_to_integer(unicode:characters_to_list(CodePoint), 16),
            IsKey = maps:is_key(Type, TypeAcc),
            if
                IsKey ->
                    SameTypeValues = maps:get(Type, TypeAcc),
                    maps:update(Type, SameTypeValues ++ [Value], TypeAcc);
                true ->
                    maps:put(Type, [Value], TypeAcc)
            end;
        _ ->
            TypeAcc
    end.

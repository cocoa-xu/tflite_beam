-module(tflite_beam_private_utils_unicode_data).
-export([
    get_puncuation_list_from_unicode_data/1,
    release_memory/0
]).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).
-record(state, {
    puncuation_list = []
}).

get_puncuation_list_from_unicode_data(UnicodeDataFile) ->
    ServerPid = get_running_instance(true),
    gen_server:call(ServerPid, {get_puncuation_list, UnicodeDataFile}).

release_memory() ->
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
            read_from_unicode_data(FileDescriptor, process_unicode_data_line(Line, TypeAcc));
        eof ->
            {ok, lists:flatten(maps:values(TypeAcc))};
        {error, Reason} ->
            {error, Reason}
    end.

process_unicode_data_line(Line, TypeAcc) ->
    BinaryLine = unicode:characters_to_binary(Line),
    process_unicode_data_line_impl(BinaryLine, [], TypeAcc).

process_unicode_data_line_impl(BinaryLine, Acc, TypeAcc) ->
    case binary:split(BinaryLine, <<";">>) of
        [Chunk, Rest] ->
            process_unicode_data_line_impl(Rest, Acc ++ [Chunk], TypeAcc);
        [Chunk] ->
            LineValues = Acc ++ [Chunk],
            accumlate_type(LineValues, TypeAcc)
    end.

accumlate_type(LineValues, TypeAcc) ->
    [CodePoint, _Name, Type | _Rest] = LineValues,
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

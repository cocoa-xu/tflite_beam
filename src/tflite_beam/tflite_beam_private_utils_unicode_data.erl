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
                    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
                    Pid;
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
            {ok, FileDescriptor} = file:open(UnicodeDataFile, [read, raw]),
            PuncuationList = read_from_unicode_data(FileDescriptor, #{}),
            {reply, PuncuationList, State#state{puncuation_list = PuncuationList}};
        PuncuationList when is_list(PuncuationList) ->
            {reply, PuncuationList, State}        
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

read_from_unicode_data(FileDescriptor, TypeAcc) ->
    case file:read_line(FileDescriptor) of
        {ok, Line} ->
            read_from_unicode_data(FileDescriptor, process_unicode_data_line(Line, TypeAcc));
        _ ->
            lists:flatten(maps:values(TypeAcc))
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

%% @doc
%% A compiled model running on a node of its own.
%%
%% Everything else here runs LiteRT inside the emulator, which is fast and is
%% the right default. It is also unconditional: a NIF cannot be interrupted, and
%% a segmentation fault inside an accelerator plugin, a delegate that aborts, or
%% an inference that never returns takes the whole virtual machine, every other
%% model in it, and every process that had nothing to do with any of it.
%%
%% There is no flag that fixes that, because the problem is that the code shares
%% an address space and a scheduler pool with everything else. What does fix it
%% is not sharing them. This module starts a second Erlang node, builds the
%% model there, and forwards calls to it, so a crash costs one node and a
%% supervisor can start another. `run/2' and its siblings answer the same way
%% they do on `tflite_beam_litert_compiled_model_server', and a dead node comes
%% back as `{error, Binary}' rather than as a dead caller: this process stays up
%% after its node goes down, answering every call with
%% `{error, <<"the isolated model's node went down">>}', so that recovering is
%% something the caller decides on rather than something that happens to it.
%% `stop/1' when it is done.
%%
%% What it costs, so the choice is an informed one:
%%
%% <ul>
%%   <li>Inputs and outputs are copied between nodes, twice per call. On a large
%%       image that is real, and on a small tensor it is not.</li>
%%   <li>Starting a node takes hundreds of milliseconds, and the model is built
%%       again on it.</li>
%%   <li>The emulator must be distributed, which means a name and a cookie.
%%       `start_link/1' starts distribution if it is not already running.</li>
%% </ul>
%%
%% Use it when a model is untrusted, when an accelerator plugin is new, or when
%% one inference must not be able to take the system down. Use the in-process
%% server when the model is yours and the cost is not worth paying.
-module(tflite_beam_litert_compiled_model_isolated).
-behaviour(gen_server).

-export([
    start_link/1, start_link/2,
    start/1, start/2,
    run/2, run/3,
    with/2, with/3,
    io_sizes/1,
    fully_accelerated/1,
    run_with_metrics/2, run_with_metrics/3, run_with_metrics/4,
    profile/1, profile/2,
    pending_events/1,
    summarise_profile/1,
    reset_profile/1,
    node_of/1,
    stop/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Runs on the peer, reached by rpc. Not for callers.
-export([build_remote/3]).

-define(DEFAULT_TIMEOUT, 30000).
-define(M, tflite_beam_litert_compiled_model).
-define(SERVER, tflite_beam_litert_compiled_model_server).

-type opts() :: #{
    model_path := binary() | string(),
    runtime_library_dir => binary() | string(),
    accelerators => [?M:accelerator()],
    precision => ?M:precision(),
    profile => boolean(),
    signature => ?M:signature_index() | binary() | string(),
    max_model_bytes => non_neg_integer(),
    max_queue => non_neg_integer(),
    peer_args => [string()]
}.
-export_type([opts/0]).

%% @doc Start a model on a node of its own.
-spec start_link(opts()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    start_link(Opts, []).

%% @doc Start a model on a node of its own, with gen_server options.
-spec start_link(opts(), list()) -> {ok, pid()} | {error, term()}.
start_link(Opts, GenOpts) when is_map(Opts) ->
    gen_server:start_link(?MODULE, Opts, GenOpts).

%% @doc Start one outside a supervision tree.
-spec start(opts()) -> {ok, pid()} | {error, term()}.
start(Opts) ->
    start(Opts, []).

%% @doc Start one outside a supervision tree, with gen_server options.
-spec start(opts(), list()) -> {ok, pid()} | {error, term()}.
start(Opts, GenOpts) when is_map(Opts) ->
    gen_server:start(?MODULE, Opts, GenOpts).

%% @doc Run the model over `Inputs'.
-spec run(pid(), [binary()]) -> {ok, [binary()]} | {error, binary()}.
run(Server, Inputs) ->
    run(Server, Inputs, ?DEFAULT_TIMEOUT).

%% @doc
%% Run the model, with a call timeout.
%%
%% The timeout ends the wait rather than the inference, the same as it does
%% everywhere else. The difference here is what happens when the node dies
%% under it: the call returns an error and this process stays up, so a caller
%% learns about it instead of being taken down with it.
-spec run(pid(), [binary()], timeout()) -> {ok, [binary()]} | {error, binary()}.
run(Server, Inputs, Timeout) when is_list(Inputs) ->
    call(Server, {run, Inputs}, Timeout).

%% @doc The byte size of each input and output buffer.
-spec io_sizes(pid()) -> {ok, {[non_neg_integer()], [non_neg_integer()]}} | {error, binary()}.
io_sizes(Server) ->
    call(Server, io_sizes, ?DEFAULT_TIMEOUT).

%% @doc Whether anything is left for the ordinary interpreter to run.
-spec fully_accelerated(pid()) -> {ok, boolean()} | {error, binary()}.
fully_accelerated(Server) ->
    call(Server, fully_accelerated, ?DEFAULT_TIMEOUT).

%% @doc
%% Run a function against the compiled model, on the node that owns it.
%%
%% The callback is sent to that node and applied there, so it must return
%% something worth sending back: a value, not a handle to something local to it.
-spec with(pid(), fun((reference()) -> Result)) -> Result | {error, binary()}
    when Result :: term().
with(Server, Fun) ->
    with(Server, Fun, ?DEFAULT_TIMEOUT).

%% @doc As `with/2', waiting at most `Timeout'.
-spec with(pid(), fun((reference()) -> Result), timeout()) -> Result | {error, binary()}
    when Result :: term().
with(Server, Fun, Timeout) when is_function(Fun, 1) ->
    call(Server, {with, Fun}, Timeout).

%% @doc Run the model and collect whatever counters the accelerator reports.
-spec run_with_metrics(pid(), [binary()]) ->
    {ok, {[binary()], [{binary(), ?M:metric_value()}]}} | {error, binary()}.
run_with_metrics(Server, Inputs) ->
    run_with_metrics(Server, Inputs, 0).

%% @doc As `run_with_metrics/2', at a given detail level.
-spec run_with_metrics(pid(), [binary()], ?M:detail_level()) ->
    {ok, {[binary()], [{binary(), ?M:metric_value()}]}} | {error, binary()}.
run_with_metrics(Server, Inputs, DetailLevel) ->
    run_with_metrics(Server, Inputs, DetailLevel, ?DEFAULT_TIMEOUT).

%% @doc As `run_with_metrics/3', waiting at most `Timeout'.
-spec run_with_metrics(pid(), [binary()], ?M:detail_level(), timeout()) ->
    {ok, {[binary()], [{binary(), ?M:metric_value()}]}} | {error, binary()}.
run_with_metrics(Server, Inputs, DetailLevel, Timeout)
        when is_list(Inputs), is_integer(DetailLevel),
             DetailLevel >= 0, DetailLevel =< 2147483647 ->
    call(Server, {run_with_metrics, Inputs, DetailLevel}, Timeout).

%% @doc Every profiling event recorded since the last reset.
-spec profile(pid()) -> {ok, [?M:event()]} | {error, binary()}.
profile(Server) ->
    profile(Server, 0).

%% @doc The most recent `Limit' events, or all of them when `Limit' is zero.
-spec profile(pid(), non_neg_integer()) -> {ok, [?M:event()]} | {error, binary()}.
profile(Server, Limit) when is_integer(Limit), Limit >= 0, Limit =< 2147483647 ->
    call(Server, {profile, Limit}, ?DEFAULT_TIMEOUT).

%% @doc Per-operator totals, slowest first.
-spec summarise_profile(pid()) -> {ok, [?M:summary_entry()]} | {error, binary()}.
summarise_profile(Server) ->
    call(Server, summarise_profile, ?DEFAULT_TIMEOUT).

%% @doc How many profiling events are waiting, without reading them.
-spec pending_events(pid()) -> {ok, non_neg_integer()} | {error, binary()}.
pending_events(Server) ->
    call(Server, pending_events, ?DEFAULT_TIMEOUT).

%% @doc Forget the events recorded so far and keep recording.
-spec reset_profile(pid()) -> ok | {error, binary()}.
reset_profile(Server) ->
    call(Server, reset_profile, ?DEFAULT_TIMEOUT).

%% @doc
%% The node the model is on.
%%
%% For a supervisor that wants to know what it lost, and for a test that wants
%% to kill it.
-spec node_of(pid()) -> {ok, node()} | {error, binary()}.
node_of(Server) ->
    call(Server, node_of, ?DEFAULT_TIMEOUT).

%% @doc Stop this process and the node with it.
-spec stop(pid()) -> ok.
stop(Server) ->
    gen_server:stop(Server).

%% A call whose whole point is that the far side may die: an exit from the
%% gen_server call is turned into an error, because a caller of an isolated
%% model asked not to be taken down by it.
call(Server, Request, Timeout) ->
    try gen_server:call(Server, Request, Timeout)
    catch
        exit:{noproc, _} -> {error, <<"the isolated model is not running">>};
        exit:{timeout, _} -> {error, <<"the isolated model did not answer in time">>};
        exit:{Reason, _} ->
            {error, iolist_to_binary(io_lib:format("the isolated model exited: ~p", [Reason]))}
    end.

%% gen_server

init(Opts) ->
    process_flag(trap_exit, true),
    case ensure_distributed() of
        ok ->
            case start_peer(Opts) of
                {ok, Peer, Node} ->
                    case start_model_on(Node, Opts) of
                        {ok, Remote} ->
                            monitor_node(Node, true),
                            {ok, #{peer => Peer, node => Node, remote => Remote}};
                        {error, Reason} ->
                            peer:stop(Peer),
                            {stop, Reason}
                    end;
                {error, Reason} ->
                    {stop, Reason}
            end;
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(node_of, _From, State = #{node := Node}) ->
    {reply, {ok, Node}, State};
handle_call(_Request, _From, State = #{remote := down}) ->
    {reply, {error, <<"the isolated model's node went down">>}, State};
handle_call({with, Fun}, _From, State = #{remote := Remote, node := Node}) ->
    %% A fun carries the module that made it, and applying it on the peer needs
    %% that module there. Loading it from the peer's code path covers a fun from
    %% any compiled module, which is the ordinary case. It does not cover one
    %% made where no module was written to disk: an ExUnit test body, an escript,
    %% an iex session. Those have no object code to send, so the peer answers
    %% undef, and this says which fun and why rather than passing that on.
    Reply =
        case ensure_module_on(Node, erlang:fun_info(Fun, module)) of
            ok ->
                try ?SERVER:with(Remote, Fun)
                catch
                    exit:{noproc, _} -> {error, <<"the isolated model is no longer there">>};
                    exit:{nodedown, _} -> {error, <<"the isolated model's node went down">>}
                end;
            {error, Why} ->
                {error, Why}
        end,
    {reply, Reply, State};
handle_call(Request, _From, State = #{remote := Remote}) ->
    %% The far side is a plain in-process server, so this is the same call it
    %% would have got locally; only the wire is different.
    Reply =
        try forward(Remote, Request)
        catch
            exit:{noproc, _} -> {error, <<"the isolated model is no longer there">>};
            exit:{nodedown, _} -> {error, <<"the isolated model's node went down">>};
            exit:{Reason, _} ->
                {error, iolist_to_binary(
                    io_lib:format("the isolated model failed: ~p", [Reason]))}
        end,
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({nodedown, Node}, State = #{node := Node}) ->
    %% Surviving rather than stopping, because this module exists precisely so
    %% that a dead node is not a dead caller. Stopping here would exit with
    %% {nodedown, Node} and, through the link start_link/1 makes, take the
    %% caller with it, which is the outcome isolation was supposed to prevent.
    %% The process stays as a handle that answers, and what to do about it is
    %% then a decision taken from a return value instead of an exit signal.
    {noreply, State#{remote := down}};
handle_info({'EXIT', Peer, _Reason}, State = #{peer := Peer}) ->
    %% peer:start_link links its control process to this one. It going away
    %% means the same thing as the node going away, and the two do not arrive
    %% in a guaranteed order.
    {noreply, State#{remote := down}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #{peer := Peer}) ->
    catch peer:stop(Peer),
    ok;
terminate(_Reason, _State) ->
    ok.

%% The peer starts with this node's code path, so a module that lives in a beam
%% file is already reachable there and this is a no-op. What it adds is the
%% honest refusal for one that does not: a fun made in an ExUnit case or a
%% script belongs to a module the compiler kept in memory, and no code path
%% reaches it.
ensure_module_on(Node, {module, Module}) ->
    case erpc:call(Node, code, ensure_loaded, [Module]) of
        {module, Module} ->
            ok;
        {error, _} ->
            case code:get_object_code(Module) of
                {Module, Binary, Filename} ->
                    case erpc:call(Node, code, load_binary, [Module, Filename, Binary]) of
                        {module, Module} -> ok;
                        {error, Reason} -> {error, load_failed(Module, Reason)}
                    end;
                error ->
                    {error, no_object_code(Module)}
            end
    end.

load_failed(Module, Reason) ->
    iolist_to_binary(io_lib:format(
        "the isolated node could not load ~p, which the callback belongs to: ~p",
        [Module, Reason])).

no_object_code(Module) ->
    iolist_to_binary(io_lib:format(
        "the callback belongs to ~p, which has no compiled file to send to the "
        "isolated node. A function captured from a module on disk, such as "
        "fun mod:f/1, crosses; one written inline in a test case or a script "
        "does not.", [Module])).

forward(Remote, {run, Inputs}) -> ?SERVER:run(Remote, Inputs);
forward(Remote, io_sizes) -> ?SERVER:io_sizes(Remote);
forward(Remote, fully_accelerated) -> ?SERVER:fully_accelerated(Remote);
forward(Remote, {run_with_metrics, Inputs, DetailLevel}) ->
    ?SERVER:run_with_metrics(Remote, Inputs, DetailLevel);
forward(Remote, {profile, Limit}) -> ?SERVER:profile(Remote, Limit);
forward(Remote, summarise_profile) -> ?SERVER:summarise_profile(Remote);
forward(Remote, pending_events) -> ?SERVER:pending_events(Remote);
forward(Remote, reset_profile) -> ?SERVER:reset_profile(Remote).

%% Distribution has to be up before a peer can be started, and a library should
%% not insist the caller arranged that in advance.
ensure_distributed() ->
    case is_alive() of
        true ->
            ok;
        false ->
            Name = list_to_atom("tflite_beam_" ++ integer_to_list(erlang:unique_integer([positive]))),
            case net_kernel:start([Name, shortnames]) of
                {ok, _} -> ok;
                {error, {already_started, _}} -> ok;
                {error, Reason} ->
                    {error, iolist_to_binary(
                        io_lib:format("cannot start distribution: ~p", [Reason]))}
            end
    end.

start_peer(Opts) ->
    %% the same code path, so the peer finds this library and its NIF
    Args = ["-pa" | code:get_path()] ++ maps:get(peer_args, Opts, []),
    case peer:start_link(#{name => peer:random_name(), args => Args, connection => standard_io}) of
        {ok, Peer, Node} -> {ok, Peer, Node};
        {error, Reason} ->
            {error, iolist_to_binary(
                io_lib:format("cannot start the isolated node: ~p", [Reason]))}
    end.

start_model_on(Node, Opts) ->
    Dir = maps:get(runtime_library_dir, Opts, <<>>),
    Path = maps:get(model_path, Opts),
    ServerOpts = maps:without([model_path, runtime_library_dir, peer_args], Opts),
    case rpc:call(Node, ?MODULE, build_remote, [Dir, Path, ServerOpts], 60000) of
        {ok, Remote} -> {ok, Remote};
        {error, Reason} -> {error, Reason};
        {badrpc, Reason} ->
            {error, iolist_to_binary(
                io_lib:format("cannot build the model on the isolated node: ~p", [Reason]))}
    end.

build_remote(Dir, Path, ServerOpts) ->
    case ?M:environment(Dir) of
        {ok, Env} -> ?SERVER:start(Env, Path, ServerOpts);
        {error, Reason} -> {error, Reason}
    end.

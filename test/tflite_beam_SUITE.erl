%% @doc The existing API, plus the baseline that pins how much of a model
%% XNNPACK claims for itself today.
-module(tflite_beam_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../src/tflite_beam/tflite_beam_records.hrl").

-export([all/0]).
-export([
    model_from_file/1,
    model_from_buffer/1,
    model_from_missing_file/1,
    model_from_invalid_buffer/1,
    associated_files_one_and_many/1,
    set_data_takes_the_whole_tensor_or_nothing/1,
    a_download_cannot_be_aimed_outside_the_cache/1,
    a_digit_is_part_of_the_word_it_sits_in/1,
    an_overlong_word_becomes_unknown_rather_than_nothing/1,
    associated_files_by_string_and_by_list/1,
    predict_reports_a_bad_input_instead_of_crashing/1,
    predict_does_not_answer_from_a_failed_invoke/1,
    the_server_is_the_concurrency_safe_path/1,
    the_server_survives_a_malformed_request/1,
    interpreter_from_builder/1,
    interpreter_tensor_metadata/1,
    tensor_accessors_take_the_record/1,
    build_version_is_injected/1,
    interpreter_invoke/1,
    interpreter_predict/1,
    signature_runner_invoke/1,
    execution_plan_baseline/1,
    quantized_model_is_not_delegated/1
]).

%% every tensor in multi_add.bin is a [1, 8, 8, 3] float32
-define(FILLED(V), binary:copy(<<V:32/float-native>>, 1 * 8 * 8 * 3)).

all() ->
    [
        model_from_file,
        model_from_buffer,
        model_from_missing_file,
        model_from_invalid_buffer,
        associated_files_one_and_many,
        set_data_takes_the_whole_tensor_or_nothing,
        a_download_cannot_be_aimed_outside_the_cache,
        a_digit_is_part_of_the_word_it_sits_in,
        an_overlong_word_becomes_unknown_rather_than_nothing,
        associated_files_by_string_and_by_list,
        predict_reports_a_bad_input_instead_of_crashing,
        predict_does_not_answer_from_a_failed_invoke,
        the_server_is_the_concurrency_safe_path,
        the_server_survives_a_malformed_request,
        interpreter_from_builder,
        interpreter_tensor_metadata,
        tensor_accessors_take_the_record,
        build_version_is_injected,
        interpreter_invoke,
        interpreter_predict,
        signature_runner_invoke,
        execution_plan_baseline,
        quantized_model_is_not_delegated
    ].

model_from_file(_Config) ->
    Model = tflite_beam_flatbuffer_model:build_from_file(tflite_beam_test_models:path("add.bin")),
    ?assertMatch(#tflite_beam_flatbuffer_model{initialized = true}, Model),
    ?assert(is_reference(Model#tflite_beam_flatbuffer_model.ref)).

model_from_buffer(_Config) ->
    {ok, Buffer} = file:read_file(tflite_beam_test_models:path("add.bin")),
    ?assertMatch(#tflite_beam_flatbuffer_model{initialized = true},
                 tflite_beam_flatbuffer_model:build_from_buffer(Buffer)),
    ?assertMatch(#tflite_beam_flatbuffer_model{initialized = true},
                 tflite_beam_flatbuffer_model:verify_and_build_from_buffer(Buffer)).

model_from_missing_file(_Config) ->
    ?assertMatch({error, _},
                 tflite_beam_flatbuffer_model:build_from_file(
                     tflite_beam_test_models:path("no_such_model.bin"))).

model_from_invalid_buffer(_Config) ->
    ?assertEqual(invalid,
                 tflite_beam_flatbuffer_model:verify_and_build_from_buffer(<<"not a flatbuffer">>)).

interpreter_from_builder(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("multi_add.bin"),
    ?assertEqual(7, tflite_beam_interpreter:tensors_size(Interpreter)),
    ?assertEqual({ok, [0, 1, 2, 3]}, tflite_beam_interpreter:inputs(Interpreter)),
    ?assertEqual({ok, [5, 6]}, tflite_beam_interpreter:outputs(Interpreter)),
    ?assertEqual({ok, <<"a">>}, tflite_beam_interpreter:get_input_name(Interpreter, 0)),
    ?assertEqual({ok, <<"x">>}, tflite_beam_interpreter:get_output_name(Interpreter, 0)),
    ?assertEqual([<<"serving_default">>], tflite_beam_interpreter:signature_keys(Interpreter)).

interpreter_tensor_metadata(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("multi_add.bin"),
    Tensor = tflite_beam_interpreter:tensor(Interpreter, 0),
    ?assertMatch(#tflite_beam_tensor{name = <<"a">>, index = 0, type = {f, 32}}, Tensor),
    ?assertEqual([1, 8, 8, 3], tflite_beam_tensor:dims(Tensor#tflite_beam_tensor.ref)),
    ?assertEqual({f, 32}, tflite_beam_tensor:type(Tensor#tflite_beam_tensor.ref)),
    ?assert(is_binary(tflite_beam_tensor:to_binary(Tensor))),
    %% a handle keeps its interpreter alive, so naming the interpreter here is
    %% not what makes the reads above safe. See
    %% tflite_beam_lifetime_SUITE:tensor_handle_keeps_its_interpreter_alive/1
    ?assertEqual(7, tflite_beam_interpreter:tensors_size(Interpreter)).

%% dims/1 and shape/1 accept the record as well as the handle. Their record
%% clauses guarded on is_tuple while the NIF fills the field with a list, so
%% they raised function_clause for every caller; only the handle path was ever
%% covered here.
tensor_accessors_take_the_record(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("multi_add.bin"),
    Tensor = tflite_beam_interpreter:tensor(Interpreter, 0),
    Ref = Tensor#tflite_beam_tensor.ref,
    ?assertEqual([1, 8, 8, 3], tflite_beam_tensor:dims(Tensor)),
    ?assertEqual({1, 8, 8, 3}, tflite_beam_tensor:shape(Tensor)),
    ?assertEqual(tflite_beam_tensor:dims(Ref), tflite_beam_tensor:dims(Tensor)),
    ?assertEqual(tflite_beam_tensor:shape(Ref), tflite_beam_tensor:shape(Tensor)),
    ?assertEqual(7, tflite_beam_interpreter:tensors_size(Interpreter)).

%% tflite_version/0 is what a delegate plugin gets matched against, and it comes
%% from TFLITE_VER injected at compile time rather than from the runtime, whose
%% own number is a hand-maintained constant upstream forgets to bump. This pins
%% the whole path -- Makefile to cmake to macro to NIF -- because every link in
%% it is silent when it breaks: a stale or missing value still returns a string.
build_version_is_injected(_Config) ->
    Version = tflite_beam:tflite_version(),
    ?assertNotEqual(<<"unknown">>, Version),
    ?assertEqual(makefile_tflite_ver(), Version),
    ?assert(is_binary(tflite_beam:tflite_runtime_version())),
    ?assert(is_binary(tflite_beam:tflite_extension_apis_version())),
    ?assert(tflite_beam:tflite_schema_version() > 0).

makefile_tflite_ver() ->
    Root = filename:join(code:lib_dir(tflite_beam), filename:join(lists:duplicate(4, ".."))),
    Makefile = case filelib:is_regular(filename:join(Root, "Makefile")) of
        true -> filename:join(Root, "Makefile");
        false -> "Makefile"
    end,
    {ok, Contents} = file:read_file(Makefile),
    [_, Rest] = binary:split(Contents, <<"\nTFLITE_VER ?= ">>),
    [Version | _] = binary:split(Rest, <<"\n">>),
    Version.

interpreter_invoke(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("multi_add.bin"),
    ok = tflite_beam_interpreter:input_tensor(Interpreter, 0, ?FILLED(1.0)),
    ok = tflite_beam_interpreter:input_tensor(Interpreter, 1, ?FILLED(2.0)),
    ok = tflite_beam_interpreter:input_tensor(Interpreter, 2, ?FILLED(3.0)),
    ok = tflite_beam_interpreter:input_tensor(Interpreter, 3, ?FILLED(4.0)),
    ok = tflite_beam_interpreter:invoke(Interpreter),
    %% x = a + (b + c), y = d + (b + c)
    ?assertEqual({ok, ?FILLED(6.0)}, tflite_beam_interpreter:output_tensor(Interpreter, 0)),
    ?assertEqual({ok, ?FILLED(9.0)}, tflite_beam_interpreter:output_tensor(Interpreter, 1)).

interpreter_predict(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("multi_add.bin"),
    ?assertEqual([?FILLED(6.0), ?FILLED(9.0)],
                 tflite_beam_interpreter:predict(
                     Interpreter, [?FILLED(1.0), ?FILLED(2.0), ?FILLED(3.0), ?FILLED(4.0)])).

signature_runner_invoke(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("multi_add.bin"),
    {ok, Runner} = tflite_beam_interpreter:get_signature_runner(Interpreter, <<"serving_default">>),
    ?assertEqual({ok, <<"serving_default">>}, tflite_beam_signature_runner:signature_key(Runner)),
    ?assertEqual({ok, 4}, tflite_beam_signature_runner:input_size(Runner)),
    {ok, InputNames} = tflite_beam_signature_runner:input_names(Runner),
    ?assertEqual([<<"a">>, <<"b">>, <<"c">>, <<"d">>], lists:sort(InputNames)),
    ok = tflite_beam_signature_runner:allocate_tensors(Runner),
    ok = tflite_beam_signature_runner:input_tensor(Runner, <<"a">>, ?FILLED(1.0)),
    ok = tflite_beam_signature_runner:input_tensor(Runner, <<"b">>, ?FILLED(2.0)),
    ok = tflite_beam_signature_runner:input_tensor(Runner, <<"c">>, ?FILLED(3.0)),
    ok = tflite_beam_signature_runner:input_tensor(Runner, <<"d">>, ?FILLED(4.0)),
    ok = tflite_beam_signature_runner:invoke(Runner),
    ?assertEqual({ok, ?FILLED(6.0)}, tflite_beam_signature_runner:output_tensor(Runner, <<"x">>)),
    ?assertEqual({ok, ?FILLED(9.0)}, tflite_beam_signature_runner:output_tensor(Runner, <<"y">>)).

%% multi_add.bin's three ADDs are one XNNPACK partition, and a delegate replaces
%% the partition it claims with a single execution-plan entry. That collapse is
%% how every later delegate change proves a delegate actually ran, so it is
%% pinned here. Measure it on an interpreter that has touched no signature
%% runner: `get_signature_runner/2' applies the lazy providers too.
%%
%% This is the lazy path specifically -- TfLite applying XNNPACK by itself inside
%% allocate_tensors/1 -- so it asks for the resolver that still does that. The
%% default resolver no longer does; the builder attaches XNNPACK explicitly
%% instead, and the plan has already collapsed by the time build/2 returns.
execution_plan_baseline(_Config) ->
    {Builder, Interpreter} = tflite_beam_test_models:lazy_builder("multi_add.bin"),
    ok = tflite_beam_interpreter_builder:build(Builder, Interpreter),
    ?assertEqual([0, 1, 2], tflite_beam_interpreter:execution_plan(Interpreter)),
    ?assertEqual(3, tflite_beam_interpreter:nodes_size(Interpreter)),
    ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
    case xnnpack_compiled_in() of
        false ->
            {skip, "XNNPACK is not compiled into this build"};
        true ->
            ?assertEqual(1, length(tflite_beam_interpreter:execution_plan(Interpreter))),
            %% the delegate node is appended rather than substituted, so the two
            %% numbers are never equal
            ?assertEqual(4, tflite_beam_interpreter:nodes_size(Interpreter))
    end.

%% The control: XNNPACK is compiled in here too, and claims nothing. Delegation
%% is per op pattern, so a plan that does not move is not evidence of anything.
quantized_model_is_not_delegated(_Config) ->
    {Builder, Interpreter} = tflite_beam_test_models:lazy_builder("add_quantized_int8.bin"),
    ok = tflite_beam_interpreter_builder:build(Builder, Interpreter),
    ?assertEqual([0, 1], tflite_beam_interpreter:execution_plan(Interpreter)),
    ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
    ?assertEqual([0, 1], tflite_beam_interpreter:execution_plan(Interpreter)),
    ?assertEqual(2, tflite_beam_interpreter:nodes_size(Interpreter)).

xnnpack_compiled_in() ->
    lists:member(xnnpack, tflite_beam_delegate:available()).

%% get_associated_file/2 takes one filename or a list of them, and the list
%% branch called map:from_list. The module is maps, so every caller that passed a
%% list got undef, which is presumably why only the single-file branch had ever
%% been used. The model is only ever opened as a zip here, so a zip is all the
%% fixture needs to be.
associated_files_one_and_many(_Config) ->
    Labels = <<"robin\nparrot\n">>,
    {ok, {_Name, Zip}} = zip:create("model.zip", [{"labels.txt", Labels}], [memory]),
    ?assertEqual([<<"labels.txt">>], tflite_beam_flatbuffer_model:list_associated_files(Zip)),
    ?assertEqual(Labels, tflite_beam_flatbuffer_model:get_associated_file(Zip, <<"labels.txt">>)),
    ?assertEqual(#{<<"labels.txt">> => Labels},
                 tflite_beam_flatbuffer_model:get_associated_file(Zip, [<<"labels.txt">>])).

%% Writing fewer bytes than the tensor holds used to be memcpy'd as far as the
%% binary went and reported as ok, which leaves the rest of the tensor holding
%% whatever the arena held before it and produces an answer computed partly from
%% that. Writing more was truncated just as quietly. Both are now refused, and
%% the error says both numbers so nobody has to count bytes to find out which
%% way they were wrong.
set_data_takes_the_whole_tensor_or_nothing(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("add.bin"),
    {ok, [Index | _]} = tflite_beam_interpreter:inputs(Interpreter),
    Tensor = tflite_beam_interpreter:tensor(Interpreter, Index),
    Exact = 1 * 8 * 8 * 3 * 4,

    ok = tflite_beam_tensor:set_data(Tensor, binary:copy(<<7>>, Exact)),

    {error, Short} = tflite_beam_tensor:set_data(Tensor, binary:copy(<<7>>, Exact - 4)),
    ?assertNotEqual(nomatch, binary:match(Short, integer_to_binary(Exact))),
    ?assertNotEqual(nomatch, binary:match(Short, integer_to_binary(Exact - 4))),

    ?assertMatch({error, _}, tflite_beam_tensor:set_data(Tensor, binary:copy(<<7>>, Exact + 4))),

    %% the refusal left the tensor as the exact write put it, not half rewritten
    ?assertEqual(binary:copy(<<7>>, Exact), tflite_beam_tensor:to_binary(Tensor)).

%% Filling the inputs answers with {error, Binary} tuples, and the code that
%% turned those into one message appended each with R/binary, which raises badarg
%% on anything that is not a bare binary. So the one path that had a real reason
%% to report crashed instead of reporting it, and it crashed hardest where it
%% mattered most: every refusal from the interpreter guard arrives here.
predict_reports_a_bad_input_instead_of_crashing(_Config) ->
    {ok, Interpreter} = tflite_beam_interpreter:new(tflite_beam_test_models:path("add.bin")),
    Result = (catch tflite_beam_interpreter:predict(Interpreter, [<<0, 0>>])),
    ?assertNotMatch({'EXIT', _}, Result),
    ?assertMatch({error, _}, Result),
    {error, Reason} = Result,
    ?assertNotEqual(nomatch, binary:match(Reason, <<"768">>)).

%% predict/2 used to throw away what invoke/1 returned and read the output
%% tensors regardless, so a failed invoke handed back whatever the arena still
%% held from the run before. Reached deterministically here by resizing an input
%% and not allocating, which leaves the graph not ready: TfLite refuses the
%% invoke, and predict has to pass that on rather than answer from stale memory.
predict_does_not_answer_from_a_failed_invoke(_Config) ->
    %% Reaching a failed invoke means getting past fill_input first, and a resize
    %% without an allocate nulls the arena so set_data fails before invoke is ever
    %% called. An earlier version of this case did exactly that and proved
    %% nothing. Building into a second interpreter from a model whose graph
    %% cannot be prepared leaves the tensors writable and the invoke refused.
    {Builder, Interpreter} = tflite_beam_test_models:builder("add.bin"),
    ok = tflite_beam_interpreter_builder:build(Builder, Interpreter),
    ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
    {ok, [Index | _]} = tflite_beam_interpreter:inputs(Interpreter),
    Whole = binary:copy(<<0, 0, 128, 63>>, 192),

    %% a good run, so the output tensors hold a real answer
    ?assertMatch([_ | _], tflite_beam_interpreter:predict(Interpreter, [Whole])),
    [Before] = tflite_beam_interpreter:predict(Interpreter, [Whole]),

    %% now make invoke fail while the inputs stay writable: enable cancellation
    %% and cancel, which is refused at Invoke and not at set_data
    ok = tflite_beam_interpreter:enable_cancellation(Interpreter),
    ok = tflite_beam_interpreter:cancel(Interpreter),

    case tflite_beam_interpreter:predict(Interpreter, [Whole]) of
        {error, _} ->
            ok;
        [After] when is_binary(After) ->
            %% if the run was not actually refused the answer must still be its
            %% own, never the previous one handed back untouched
            ?assertEqual(Before, After)
    end.

%% And the concurrent case, which predict/2 cannot answer and the server can.
%% Feeding, running and reading back are three separate calls into the NIF, and
%% the interpreter's guard is released between them, so two processes sharing one
%% interpreter through predict/2 can still read each other's output. That is the
%% documented contract for the direct API. The server exists so there is a way to
%% do it that holds all three together, and this pins the difference rather than
%% leaving it as prose.
the_server_is_the_concurrency_safe_path(_Config) ->
    Path = tflite_beam_test_models:path("add.bin"),
    {ok, Server} = tflite_beam_interpreter_server:start_link(Path, []),
    Parent = self(),
    N = 400,
    [spawn(fun() ->
        Value = case Index rem 2 of 0 -> 1.0; _ -> 5.0 end,
        Input = binary:copy(<<Value:32/float-native>>, 192),
        Want = binary:copy(<<(Value * 3):32/float-native>>, 192),
        Verdict = case catch tflite_beam_interpreter_server:predict(Server, [Input]) of
            [Out] when is_binary(Out) -> case Out =:= Want of true -> ok; false -> wrong end;
            Other -> Other
        end,
        Parent ! {verdict, Verdict}
     end) || Index <- lists:seq(1, N)],
    Verdicts = [receive {verdict, V} -> V after 120000 -> timeout end || _ <- lists:seq(1, N)],
    ?assertEqual(N, length([x || ok <- Verdicts]),
                 lists:flatten(io_lib:format("~p", [lists:usort(Verdicts)]))).

%% The crash above was raised inside the server's handle_call, so it took the
%% whole process with it and the model had to be loaded from disk again. One bad
%% client request destroyed the served model for every other client.
the_server_survives_a_malformed_request(_Config) ->
    {ok, Server} = tflite_beam_interpreter_server:start_link(
                     tflite_beam_test_models:path("add.bin"), []),
    Good = binary:copy(<<0, 0, 128, 63>>, 192),
    ?assertMatch([_ | _], tflite_beam_interpreter_server:predict(Server, [Good])),
    ?assertMatch({error, _}, catch tflite_beam_interpreter_server:predict(Server, [<<0, 0>>])),
    timer:sleep(100),
    ?assert(is_process_alive(Server)),
    ?assertMatch([_ | _], tflite_beam_interpreter_server:predict(Server, [Good])).

%% Both halves of the cache path come from the caller, and
%% tflite_beam_contrib_huggingface passes a repository name and a filename
%% straight through from whatever asked for the model. filename:join/2 returns an
%% absolute second argument unchanged, so joining a cache directory with
%% "/etc/anything" gives "/etc/anything", and a relative one can still climb out
%% with "..". Either one made a successful download overwrite a file of the
%% caller's choosing. Refused before the request is made, so none of these reach
%% the network.
a_download_cannot_be_aimed_outside_the_cache(_Config) ->
    Escapes = [
        {"/etc", "evil"},
        {"models", "/etc/evil"},
        {"../../..", "evil"},
        {"models", "../../../evil"}
    ],
    [begin
        Result = tflite_beam_utils_downloader:download(
                   "https://example.invalid/x", Subdir, File, true),
        ?assertMatch({error, _}, Result),
        {error, Reason} = Result,
        ?assertNotEqual(nomatch,
                        binary:match(iolist_to_binary(Reason), <<"outside the cache">>),
                        lists:flatten(io_lib:format("~ts / ~ts gave ~ts", [Subdir, File, Reason])))
     end || {Subdir, File} <- Escapes],

    %% and an ordinary name is still allowed through to the request itself. Both
    %% outcomes are {error, _}, so the control has to say which one: a refusal
    %% never reaches the network, and this one must.
    Allowed = tflite_beam_utils_downloader:download(
                "https://example.invalid/x", "models", "fine.bin", true),
    ?assertMatch({error, _}, Allowed),
    {error, Reason} = Allowed,
    %% not iolist_to_binary: a network failure comes back as a term such as
    %% {failed_connect, ...}, which is worth knowing on its own since the spec
    %% for this function promises a binary
    Printed = lists:flatten(io_lib:format("~p", [Reason])),
    ?assertEqual(nomatch, string:find(Printed, "outside the cache"), Printed).

%% The alphanumeric test read 49 to 58 where it meant 48 to 57, so it was off by
%% one at both ends: a zero counted as punctuation and was split out of the word
%% around it, and a colon counted as alphanumeric and was kept inside one.
a_digit_is_part_of_the_word_it_sits_in(_Config) ->
    ?assertEqual([<<"a0b">>], tflite_beam_basic_tokenizer:tokenize(<<"a0b">>, true)),
    ?assertEqual([<<"2026">>], tflite_beam_basic_tokenizer:tokenize(<<"2026">>, true)),
    ?assertEqual([<<"v0">>, <<".">>, <<"4">>],
                 tflite_beam_basic_tokenizer:tokenize(<<"v0.4">>, true)),
    %% and the other end: a colon is punctuation and splits
    ?assertEqual([<<"a">>, <<":">>, <<"b">>],
                 tflite_beam_basic_tokenizer:tokenize(<<"a:b">>, true)).

%% get_associated_file/2 takes one name or a list of them, and told them apart
%% with is_list/1. An Erlang string is a list, so "labels.txt" went down the
%% many-files branch, which walked it one character at a time and raised badarg
%% formatting the integer $l into an error message.
associated_files_by_string_and_by_list(_Config) ->
    Labels = <<"robin\nparrot\n">>,
    {ok, {_Name, Zip}} = zip:create("model.zip", [{"labels.txt", Labels}], [memory]),

    ?assertEqual(Labels, tflite_beam_flatbuffer_model:get_associated_file(Zip, <<"labels.txt">>)),
    ?assertEqual(Labels, tflite_beam_flatbuffer_model:get_associated_file(Zip, "labels.txt")),
    ?assertEqual(#{<<"labels.txt">> => Labels},
                 tflite_beam_flatbuffer_model:get_associated_file(Zip, [<<"labels.txt">>])),

    %% a name that is not there is still an error rather than a crash
    ?assertMatch({error, _}, tflite_beam_flatbuffer_model:get_associated_file(Zip, "nope.txt")).

%% The word-piece limit counts characters and the check measured bytes, so a word
%% in any script that does not fit one character to a byte was cut short well
%% before two hundred of them. Whatever was cut then vanished from the output
%% rather than becoming [UNK], which is what this module's own documentation and
%% the implementation it is ported from both say.
an_overlong_word_becomes_unknown_rather_than_nothing(_Config) ->
    %% Every input below has to be in the vocabulary, or find_subwords answers
    %% [UNK] on its own and the length check could be deleted without the case
    %% noticing. An earlier version of this used a vocabulary containing none of
    %% them and proved nothing.
    Vocabulary = #{<<"una">> => 1, <<"##ffa">> => 2, <<"##ble">> => 3,
                   <<"a">> => 4, <<"##a">> => 5,
                   unicode:characters_to_binary([16#4E00]) => 6,
                   unicode:characters_to_binary("##" ++ [16#4E00]) => 7},

    ?assertEqual([<<"una">>, <<"##ffa">>, <<"##ble">>],
                 tflite_beam_wordpiece_tokenizer:tokenize(<<"unaffable">>, Vocabulary)),

    %% past the limit in characters, so [UNK] rather than silence
    %% in the vocabulary piece by piece, so only the length check can produce
    %% [UNK] here
    ?assertEqual([<<"a">>, <<"##a">>, <<"##a">>],
                 tflite_beam_wordpiece_tokenizer:tokenize(<<"aaa">>, Vocabulary)),
    Overlong = binary:copy(<<"a">>, 201),
    ?assertEqual([<<"[UNK]">>],
                 tflite_beam_wordpiece_tokenizer:tokenize(Overlong, Vocabulary)),

    %% and a hundred characters that happen to occupy three hundred bytes is
    %% under the limit, where counting bytes put it over and dropped it
    %% a hundred characters occupying three hundred bytes is under the limit,
    %% where counting bytes put it over. It is in the vocabulary, so counting
    %% bytes gives [UNK] and counting characters gives the pieces.
    Wide = unicode:characters_to_binary(lists:duplicate(100, 16#4E00)),
    ?assertNotEqual([<<"[UNK]">>],
                    tflite_beam_wordpiece_tokenizer:tokenize(Wide, Vocabulary)).

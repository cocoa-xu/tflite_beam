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
    word_pieces_come_back_in_the_order_they_were_read/1,
    punctuation_membership_matches_the_table_it_came_from/1,
    releasing_the_unicode_table_lets_it_be_rebuilt/1,
    a_wrong_unicode_table_is_reported_not_matched_against/1,
    an_empty_tensor_says_so_rather_than_blaming_allocate_tensors/1,
    ideographs_are_split_the_way_bert_splits_them/1,
    an_unreadable_unicode_table_names_itself/1,
    releasing_the_table_beats_a_populate_already_running/1,
    a_symlink_out_of_the_cache_is_refused_and_a_raw_name_is_not/1,
    signature_defs_names_are_binaries_not_atoms/1,
    an_https_download_without_a_ca_store_is_refused_not_downgraded/1,
    a_timeout_ends_the_wait_and_not_the_work/1,
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
    quantized_model_is_not_delegated/1,
    a_rank_above_six_is_refused_rather_than_overrunning_the_stack/1,
    an_unnamed_tensor_reads_as_empty_rather_than_dereferencing_null/1,
    a_truncated_model_is_refused_rather_than_walked_off_the_end/1,
    a_nested_cache_subdirectory_is_created_not_refused/1,
    the_delegate_width_is_reportable_rather_than_a_hidden_rule/1,
    num_threads_follows_tflites_own_contract/1,
    metadata_reaches_the_corners_of_its_own_schema/1,
    the_loaded_object_came_from_litert/1,
    the_two_eight_bit_floats_are_told_apart/1,
    a_resize_takes_the_shape_this_library_hands_out/1,
    text_that_is_not_utf8_is_named_not_raised_from_unicode/1
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
        word_pieces_come_back_in_the_order_they_were_read,
        punctuation_membership_matches_the_table_it_came_from,
        releasing_the_unicode_table_lets_it_be_rebuilt,
        a_wrong_unicode_table_is_reported_not_matched_against,
        an_empty_tensor_says_so_rather_than_blaming_allocate_tensors,
        ideographs_are_split_the_way_bert_splits_them,
        an_unreadable_unicode_table_names_itself,
        releasing_the_table_beats_a_populate_already_running,
        a_symlink_out_of_the_cache_is_refused_and_a_raw_name_is_not,
        signature_defs_names_are_binaries_not_atoms,
        an_https_download_without_a_ca_store_is_refused_not_downgraded,
        a_timeout_ends_the_wait_and_not_the_work,
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
        quantized_model_is_not_delegated,
        a_rank_above_six_is_refused_rather_than_overrunning_the_stack,
        an_unnamed_tensor_reads_as_empty_rather_than_dereferencing_null,
        a_truncated_model_is_refused_rather_than_walked_off_the_end,
        a_nested_cache_subdirectory_is_created_not_refused,
        the_delegate_width_is_reportable_rather_than_a_hidden_rule,
        num_threads_follows_tflites_own_contract,
        metadata_reaches_the_corners_of_its_own_schema,
        the_loaded_object_came_from_litert,
        the_two_eight_bit_floats_are_told_apart,
        a_resize_takes_the_shape_this_library_hands_out,
        text_that_is_not_utf8_is_named_not_raised_from_unicode
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

%% Both of these answer {error, _}, and asserting only that is what let the
%% loader spend a release telling anyone with a typo in their path that their
%% model was not a valid flatbuffer. The two have to be told apart.
model_from_missing_file(Config) ->
    Missing = tflite_beam_flatbuffer_model:build_from_file(
                  tflite_beam_test_models:path("no_such_model.bin")),
    ?assertMatch({error, _}, Missing),
    {error, MissingReason} = Missing,
    ?assertNotEqual(nomatch, binary:match(MissingReason, <<"cannot read model file">>),
                    MissingReason),

    %% a file that is there but holds something else is the other answer
    NotAModel = filename:join(?config(priv_dir, Config), "not_a_model.bin"),
    ok = file:write_file(NotAModel, <<"this is not a flatbuffer, it is a sentence">>),
    Malformed = tflite_beam_flatbuffer_model:build_from_file(NotAModel),
    ?assertMatch({error, _}, Malformed),
    {error, MalformedReason} = Malformed,
    ?assertNotEqual(nomatch, binary:match(MalformedReason, <<"not a valid flatbuffer">>),
                    MalformedReason),

    ?assertNotEqual(MissingReason, MalformedReason).

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
    %% tflite_version/0 answers LiteRT's version now, because that is where the
    %% runtime comes from, and tensorflow_version/0 answers the release LiteRT
    %% pins for the pieces it borrows. Both are read back from the Makefile so a
    %% build that injected nothing, or injected the wrong one into the wrong
    %% place, is caught rather than reported as a version.
    Version = tflite_beam:tflite_version(),
    ?assertNotEqual(<<"unknown">>, Version),
    ?assertEqual(makefile_var(<<"LITERT_VER">>), Version),

    TensorFlow = tflite_beam:tensorflow_version(),
    ?assertNotEqual(<<"unknown">>, TensorFlow),
    ?assertEqual(makefile_var(<<"TFLITE_VER">>), TensorFlow),

    %% the two are different numbers, and swapping them would pass every check
    %% above that did not compare them
    ?assertNotEqual(Version, TensorFlow),

    ?assert(is_binary(tflite_beam:tflite_runtime_version())),
    ?assert(is_binary(tflite_beam:tflite_extension_apis_version())),
    ?assert(tflite_beam:tflite_schema_version() > 0).

makefile_var(Name) ->
    Root = filename:join(code:lib_dir(tflite_beam), filename:join(lists:duplicate(4, ".."))),
    Makefile = case filelib:is_regular(filename:join(Root, "Makefile")) of
        true -> filename:join(Root, "Makefile");
        false -> "Makefile"
    end,
    {ok, Contents} = file:read_file(Makefile),
    [_, Rest] = binary:split(Contents, <<"\n", Name/binary, " ?= ">>),
    [Value | _] = binary:split(Rest, <<"\n">>),
    Value.

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
                 tflite_beam_flatbuffer_model:get_associated_file(Zip, [<<"labels.txt">>])),

    %% A name may be an Erlang string, and on its own one always worked. Inside a
    %% list it did not: the archive names are binaries, the member test compared
    %% a string against them, and every such name came back as "cannot find" from
    %% an archive that had it. The keys come back as they were asked for, so a
    %% caller can match what it passed.
    ?assertEqual(Labels, tflite_beam_flatbuffer_model:get_associated_file(Zip, "labels.txt")),
    ?assertEqual(#{"labels.txt" => Labels},
                 tflite_beam_flatbuffer_model:get_associated_file(Zip, ["labels.txt"])),

    {ok, {_, Two}} = zip:create("two.zip",
                                [{"labels.txt", Labels}, {"notes.txt", <<"hello">>}], [memory]),
    ?assertEqual(#{"labels.txt" => Labels, <<"notes.txt">> => <<"hello">>},
                 tflite_beam_flatbuffer_model:get_associated_file(
                     Two, ["labels.txt", <<"notes.txt">>])),

    ?assertMatch(#{"nope.txt" := {error, _}},
                 tflite_beam_flatbuffer_model:get_associated_file(Zip, ["nope.txt"])).

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
    ?assertMatch([_ | _], tflite_beam_interpreter_server:predict(Server, [Good])),

    %% the wrong size was answered; the wrong type was not. A list or map the
    %% outer guard accepts, holding something that is not binary data, matched
    %% no clause further in and took the server down with the interpreter on it.
    %% and the outer shape too: a scalar matched none of predict/2's clauses and
    %% raised before any of the element checks were reached.
    Malformed = [[not_binary], [123], [nil], #{<<"input">> => not_binary},
                 #{<<"input">> => 123}, #{<<"input">> => [1, 2, 3]},
                 not_binary, 123, nil, {1, 2}, 1.5],
    lists:foreach(
        fun(Bad) ->
            ?assertMatch({error, _},
                         catch tflite_beam_interpreter_server:predict(Server, Bad)),
            timer:sleep(20),
            ?assert(is_process_alive(Server), Bad)
        end,
        Malformed),
    ?assertMatch([_ | _], tflite_beam_interpreter_server:predict(Server, [Good])).

%% Both halves of the cache path come from the caller, and are passed straight
%% through from whatever asked for the model. filename:join/2 returns an
%% absolute second argument unchanged, so joining a cache directory with
%% "/etc/anything" gives "/etc/anything", and a relative one can still climb out
%% with "..". Either one made a successful download overwrite a file of the
%% caller's choosing. Refused before the request is made, so none of these reach
%% the network.
a_download_cannot_be_aimed_outside_the_cache(_Config) ->
    %% Every one of these was written as an Erlang string, and the check only
    %% ever worked for those: filename:split/1 keeps the representation it was
    %% given, so a binary component split into binaries and the comparison
    %% against ".." matched nothing. Elixir strings are binaries, which is to say
    %% the guard was off for the callers this library mostly has.
    Escapes = [
        {"/etc", "evil"},
        {"models", "/etc/evil"},
        {"../../..", "evil"},
        {"models", "../../../evil"},
        {<<"/etc">>, <<"evil">>},
        {<<"models">>, <<"/etc/evil">>},
        {<<"../../..">>, <<"evil">>},
        {<<"models">>, <<"../../../evil">>},
        {<<"a/../../b">>, <<"evil">>}
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

%% The delegate holds a tensor's dimensions in a fixed-width array and bounds
%% the count only when it first decides to take the graph. Nothing rechecks it
%% on the reshape a resize reaches, so growing a delegated tensor across that
%% bound wrote the caller's own integers past the end of the array: rank 7 and 8
%% tripped the stack protector, rank 10 took SIGBUS, and a dimension of
%% 16#12345678 reached SIGSEGV. The bound itself is asked for rather than
%% assumed, so this test says the same thing on a build that has no delegate.
a_rank_above_six_is_refused_rather_than_overrunning_the_stack(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("add.bin"),
    {ok, [Input | _]} = tflite_beam_interpreter:inputs(Interpreter),

    case tflite_beam:xnnpack_max_tensor_dims() of
        nil ->
            %% nothing here imposes a width, so nothing may be refused for one
            ?assertEqual(ok, tflite_beam_interpreter:resize_input_tensor(
                                 Interpreter, Input, [1, 8, 8, 3, 1, 1, 1]));
        Max when is_integer(Max), Max > 3 ->
            AtBound = [1, 8, 8, 3] ++ lists:duplicate(Max - 4, 1),
            ?assertEqual(ok, tflite_beam_interpreter:resize_input_tensor(
                                 Interpreter, Input, AtBound)),
            ?assertEqual(ok, tflite_beam_interpreter:allocate_tensors(Interpreter)),

            lists:foreach(fun(Over) ->
                Dims = AtBound ++ lists:duplicate(Over, 1),
                ?assertMatch({error, _},
                             tflite_beam_interpreter:resize_input_tensor(
                                 Interpreter, Input, Dims))
            end, [1, 2, 4]),

            %% the value written past the end was the dimension itself, so the
            %% one that reached SIGSEGV belongs here too
            ?assertMatch({error, _},
                         tflite_beam_interpreter:resize_input_tensor(
                             Interpreter, Input, AtBound ++ [16#12345678])),

            %% the refusal has to leave the interpreter usable, not half resized
            ?assertEqual(ok, tflite_beam_interpreter:allocate_tensors(Interpreter)),

            %% the signature runner reaches the same resize and needs the same
            %% guard. Every input of this one feeds the same add, so they move
            %% together or the shapes stop broadcasting and allocation fails for
            %% a reason that is not the guard.
            Multi = tflite_beam_test_models:interpreter("multi_add.bin"),
            {ok, Runner} = tflite_beam_interpreter:get_signature_runner(Multi, <<"serving_default">>),
            lists:foreach(fun(Name) ->
                ?assertEqual(ok, tflite_beam_signature_runner:resize_input_tensor(
                                     Runner, Name, AtBound))
            end, [<<"a">>, <<"b">>, <<"c">>, <<"d">>]),
            ?assertMatch({error, _}, tflite_beam_signature_runner:resize_input_tensor(
                                         Runner, <<"a">>, AtBound ++ [1])),
            ?assertMatch({error, _}, tflite_beam_signature_runner:resize_input_tensor(
                                         Runner, <<"a">>, AtBound ++ [16#12345678])),
            ?assertEqual(ok, tflite_beam_signature_runner:allocate_tensors(Runner))
    end.

%% TfLite leaves name null on the scratch tensors an op allocates through
%% context->AddTensors, and conv3d asks for one to hold its im2col buffer. The
%% name helper ran strlen on that null and took the whole VM down, so reading a
%% tensor by index was unsafe on any model with a scratch tensor: this one
%% reaches it at index 3, a detection model at 261. An unnamed tensor is not an
%% error to report, it just has no name, so it reads as empty.
an_unnamed_tensor_reads_as_empty_rather_than_dereferencing_null(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("conv3d_huge_im2col.bin"),
    ?assertEqual(ok, tflite_beam_interpreter:allocate_tensors(Interpreter)),
    Count = tflite_beam_interpreter:tensors_size(Interpreter),
    ?assert(Count > 0),
    Names = [begin
                 Tensor = tflite_beam_interpreter:tensor(Interpreter, Index),
                 ?assertMatch(#tflite_beam_tensor{}, Tensor),
                 Tensor#tflite_beam_tensor.name
             end || Index <- lists:seq(0, Count - 1)],
    ?assert(lists:all(fun is_binary/1, Names)),
    %% the scratch tensor is the one that used to be fatal, so the model has to
    %% keep having one for this to be testing anything
    ?assert(lists:member(<<>>, Names)).

%% BuildFromBuffer and BuildFromFile do not verify, and a truncated model sent
%% them straight off the end of the buffer: cutting this one to a fiftieth of
%% its length segfaulted inside the NIF before it returned anything at all.
%% Both now verify, so the whole model still loads and every prefix of it is
%% refused rather than walked.
a_truncated_model_is_refused_rather_than_walked_off_the_end(_Config) ->
    Path = tflite_beam_test_models:path("dynamic_shapes.bin"),
    {ok, Whole} = file:read_file(Path),

    ?assertMatch(#tflite_beam_flatbuffer_model{initialized = true},
                 tflite_beam_flatbuffer_model:build_from_buffer(Whole)),

    Dir = filename:dirname(Path),
    lists:foreach(fun(Percent) ->
        Keep = byte_size(Whole) * Percent div 100,
        <<Prefix:Keep/binary, _/binary>> = Whole,
        ?assertMatch({error, _}, tflite_beam_flatbuffer_model:build_from_buffer(Prefix)),

        Cut = filename:join(Dir, "truncated_" ++ integer_to_list(Percent) ++ ".bin"),
        ok = file:write_file(Cut, Prefix),
        try
            ?assertMatch({error, _}, tflite_beam_flatbuffer_model:build_from_file(Cut))
        after
            file:delete(Cut)
        end
    end, [90, 50, 10, 2]).

%% A cache subdirectory naming a model usually has a parent in it, an owner or a
%% collection. mkdir_dir_p called the non-recursive make_dir, which fails with
%% enoent when that parent is missing, so no such download could be made at all.
a_nested_cache_subdirectory_is_created_not_refused(Config) ->
    Nested = ["an_owner/a_model"],

    Cache = filename:join(?config(priv_dir, Config), "cache"),
    Previous = os:getenv("TFLITE_BEAM_CACHE_DIR"),
    true = os:putenv("TFLITE_BEAM_CACHE_DIR", Cache),
    try
        Subdir = hd(Nested),
        Result = tflite_beam_utils_downloader:download(
                   "https://example.invalid/x", Subdir, "model.tflite", true),
        ?assertMatch({error, _}, Result),
        {error, Reason} = Result,
        %% a refused directory and a failed request are both {error, _}, so the
        %% assertion has to say which one this is: it has to be the request
        Printed = lists:flatten(io_lib:format("~p", [Reason])),
        ?assertEqual(nomatch, string:find(Printed, "Cannot create"), Printed),
        ?assert(filelib:is_dir(filename:join(Cache, Subdir)))
    after
        case Previous of
            false -> os:unsetenv("TFLITE_BEAM_CACHE_DIR");
            _ -> os:putenv("TFLITE_BEAM_CACHE_DIR", Previous)
        end
    end.

%% The width the resize guard enforces is asked for, not written down twice: the
%% C side takes it from XNNPACK's own header and reports it here, so a build
%% that changes it cannot leave the guard and the documentation disagreeing, and
%% a caller who hits the refusal can find out what the number is.
the_delegate_width_is_reportable_rather_than_a_hidden_rule(_Config) ->
    case tflite_beam:xnnpack_max_tensor_dims() of
        nil ->
            %% a build with no delegate imposing a width refuses nothing for one
            Interpreter = tflite_beam_test_models:interpreter("add.bin"),
            {ok, [Input | _]} = tflite_beam_interpreter:inputs(Interpreter),
            ?assertEqual(ok, tflite_beam_interpreter:resize_input_tensor(
                                 Interpreter, Input, [1, 8, 8, 3, 1, 1, 1, 1]));
        Max ->
            ?assert(is_integer(Max)),
            ?assert(Max > 0),
            %% and the number it reports is the one the guard actually applies
            Interpreter = tflite_beam_test_models:interpreter("add.bin"),
            {ok, [Input | _]} = tflite_beam_interpreter:inputs(Interpreter),
            AtBound = [1, 8, 8, 3] ++ lists:duplicate(Max - 4, 1),
            ?assertEqual(ok, tflite_beam_interpreter:resize_input_tensor(
                                 Interpreter, Input, AtBound)),
            ?assertMatch({error, _}, tflite_beam_interpreter:resize_input_tensor(
                                         Interpreter, Input, AtBound ++ [1]))
    end.

%% TfLite says "num_threads should be >= 0 or just -1 to let TFLite runtime set
%% the value". The interpreter refused everything below 1, so it turned away the
%% one value that asks TfLite to choose, and disagreed with the builder next to
%% it, which passes the integer straight through.
num_threads_follows_tflites_own_contract(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("add.bin"),

    %% -1 is how TfLite is asked to decide, and 0 means the same as 1
    lists:foreach(fun(N) ->
        ?assertEqual(ok, tflite_beam_interpreter:set_num_threads(Interpreter, N))
    end, [-1, 0, 1, 2]),

    %% below -1 is the only thing TfLite itself refuses
    ?assertMatch({error, _}, tflite_beam_interpreter:set_num_threads(Interpreter, -2)),

    %% and the builder, which was always right, still agrees
    {Builder, _} = tflite_beam_test_models:builder("add.bin"),
    lists:foreach(fun(N) ->
        ?assertEqual(ok, tflite_beam_interpreter_builder:set_num_threads(Builder, N))
    end, [-1, 0, 1]).

%% Four parts of the metadata schema that no model in this corpus reaches, so
%% test/models/metadata_corners.bin is built for them by
%% scripts/make_metadata_fixture.cpp. Every assertion here failed before its fix.
metadata_reaches_the_corners_of_its_own_schema(_Config) ->
    Model = tflite_beam_flatbuffer_model:build_from_file(
                tflite_beam_test_models:path("metadata_corners.bin")),
    #{'TFLITE_METADATA' := Metadata} =
        tflite_beam_flatbuffer_model:read_all_metadata(Model),
    #{subgraph_metadata := [Subgraph]} = Metadata,
    #{input_tensor_metadata := [Features, Scores, Tokens]} = Subgraph,

    %% FeatureProperties is an empty marker table, so there is nothing in it that
    %% can fail. Reporting one discarded the whole content map built around it,
    %% and the range with it.
    ?assertMatch(#{content := #{content_properties_type := <<"FeatureProperties">>,
                                content_properties := #{},
                                range := #{min := 1, max := 1}}},
                 Features),

    %% the thresholding option put global_score_threshold under default_score,
    %% which is a real and different field on ScoreCalibrationOptions
    #{process_units := ScoreUnits} = Scores,
    ?assertMatch([#{options_type := <<"ScoreThresholdingOptions">>,
                    options := #{global_score_threshold := 0.25}},
                  #{options_type := <<"NormalizationOptions">>}],
                 ScoreUnits),
    [#{options := ScoreOptions} | _] = ScoreUnits,
    ?assertNot(maps:is_key(default_score, ScoreOptions)),

    %% vocab_file is optional here, and its absence used to take every process
    %% unit on the tensor down with it, the unrelated one included
    #{process_units := TokenUnits} = Tokens,
    ?assertEqual(2, length(TokenUnits)),
    ?assertMatch([#{options_type := <<"SentencePieceTokenizerOptions">>},
                  #{options_type := <<"NormalizationOptions">>}],
                 TokenUnits),
    [#{options := SentencePiece} | _] = TokenUnits,
    ?assert(maps:is_key(sentencePiece_model, SentencePiece)),
    ?assertNot(maps:is_key(vocab_file, SentencePiece)),

    %% and custom_metadata has been in the schema all along without ever being
    %% read
    ?assertMatch(#{custom_metadata := [#{name := <<"beam_test">>,
                                         data := <<1, 2, 3, 4>>}]},
                 Subgraph).

%% Nothing about a shared object says which source tree it came from, and the
%% ways to end up holding the wrong one are quiet: a precompiled artifact
%% downloaded because priv/ happened to be empty, a stale copy left in _build, a
%% local build that resolved its includes against TensorFlow because that tree is
%% on the path for LiteRT's own reasons. Each of those builds, links, and passes
%% most of what follows.
%%
%% So ask. source_tree/0 exists only in an object built from LiteRT, and the C++
%% behind it names a type only LiteRT's schema defines, so neither the question
%% nor the answer can be satisfied by anything else. A release from before the
%% move fails this with undef rather than with a wrong answer.
the_loaded_object_came_from_litert(_Config) ->
    ?assertEqual(litert, tflite_beam:source_tree()),

    %% and the pieces that would be read out of the wrong tree still work, which
    %% is what went wrong the first time this was attempted
    Interpreter = tflite_beam_test_models:interpreter("multi_add.bin"),
    ?assertEqual([<<"serving_default">>],
                 tflite_beam_interpreter:signature_keys(Interpreter)).

%% Two 8 bit float types, and the width they share says nothing about which is
%% which: E5M2 spends five bits on the exponent and has infinities, E4M3FN
%% spends four and has none. Reading one as the other does not fail, it answers
%% a different number, so 0x78 is 32768 under E5M2 and 256 under E4M3FN. Neither
%% is only distinguishable by name. Nothing in the corpus carries one, so
%% test/models/fp8_types.bin is built for them by scripts/make_fp8_fixture.cpp.
the_two_eight_bit_floats_are_told_apart(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("fp8_types.bin"),
    Type = fun(Index) ->
        Tensor = tflite_beam_interpreter:tensor(Interpreter, Index),
        ?assertMatch(#tflite_beam_tensor{}, Tensor),
        Tensor#tflite_beam_tensor.type
    end,

    %% the names are Nx's, so nothing downstream has to translate them
    ?assertEqual({f8_e4m3fn, 8}, Type(0)),
    ?assertEqual({f, 8}, Type(1)),
    ?assertNotEqual(Type(0), Type(1)),

    %% a float whose width does say which it is still reads as it always did
    ?assertEqual({f, 32}, Type(2)).

%% The accumulator was turned back to front to stop it being quadratic in the
%% word count, so the thing worth pinning is that the pieces still come out in
%% reading order, across the word, the subword and the unknown paths at once.
word_pieces_come_back_in_the_order_they_were_read(_Config) ->
    Vocabulary = #{
        <<"una">> => 1, <<"##ffa">> => 2, <<"##ble">> => 3,
        <<"the">> => 4, <<"dog">> => 5, <<"[UNK]">> => 6
    },
    ?assertEqual(
        [<<"the">>, <<"una">>, <<"##ffa">>, <<"##ble">>, <<"[UNK]">>, <<"dog">>],
        tflite_beam_wordpiece_tokenizer:tokenize(<<"the unaffable unaffableX dog">>, Vocabulary)
    ),
    Words = lists:duplicate(50, <<"the">>),
    ?assertEqual(
        Words,
        tflite_beam_wordpiece_tokenizer:tokenize(
            iolist_to_binary(lists:join(<<" ">>, Words)), Vocabulary)
    ).

%% is_punctuation/1 reads a set now rather than calling into the table's process
%% once per code point. The set has to answer exactly what the list answered.
punctuation_membership_matches_the_table_it_came_from(_Config) ->
    File = filename:join(code:priv_dir(tflite_beam), "unicode_data.txt"),
    List = tflite_beam_private_utils_unicode_data:get_puncuation_list_from_unicode_data(File),
    Set = tflite_beam_private_utils_unicode_data:punctuation_set(fun() -> File end),
    ?assertEqual(length(List), maps:size(Set)),
    Disagreements =
        [CodePoint || CodePoint <- lists:seq(0, 16#3000),
                      lists:member(CodePoint, List) =/= maps:is_key(CodePoint, Set)],
    ?assertEqual([], Disagreements).

releasing_the_unicode_table_lets_it_be_rebuilt(_Config) ->
    Key = {tflite_beam_private_utils_unicode_data, punctuation_set},
    ?assertEqual([<<"before">>, <<".">>], tflite_beam_basic_tokenizer:tokenize(<<"before.">>, true)),
    ?assertNotEqual(absent, persistent_term:get(Key, absent)),

    ok = tflite_beam_private_utils_unicode_data:release_memory(),
    %% both halves have to go: the process holding the parse, and the set that
    %% was lifted out of it. Leaving the set behind is memory that release_memory
    %% was asked for and did not give back.
    ?assertEqual(undefined, erlang:whereis(tflite_beam_private_utils_unicode_data)),
    ?assertEqual(absent, persistent_term:get(Key, absent)),

    ?assertEqual([<<"after">>, <<".">>], tflite_beam_basic_tokenizer:tokenize(<<"after.">>, true)),
    ?assertNotEqual(absent, persistent_term:get(Key, absent)),
    ok = tflite_beam_private_utils_unicode_data:release_memory(),
    ok = tflite_beam_private_utils_unicode_data:release_memory().

%% Each of these used to destructure a line that had no fields to give and take
%% the table's process down, so the caller saw an exit from a process it never
%% started rather than a reason.
a_wrong_unicode_table_is_reported_not_matched_against(Config) ->
    Dir = proplists:get_value(priv_dir, Config),
    Row = <<"0021;EXCLAMATION MARK;Po;0;ON;;;;;N;;;;;\n">>,
    Check =
        fun(Name, Content, Expected) ->
            File = filename:join(Dir, Name),
            ok = file:write_file(File, Content),
            ok = tflite_beam_private_utils_unicode_data:release_memory(),
            Answer = tflite_beam_private_utils_unicode_data:
                         get_puncuation_list_from_unicode_data(File),
            case Expected of
                {ok, Count} -> ?assertEqual(Count, length(Answer));
                error -> ?assertMatch({error, _}, Answer)
            end,
            ?assertNotEqual(undefined, whereis(tflite_beam_private_utils_unicode_data))
        end,
    %% a blank line is nothing to report, including the one every file ends with
    Check("blank_line.txt", <<Row/binary, "\n", Row/binary>>, {ok, 2}),
    Check("empty.txt", <<>>, {ok, 0}),
    %% content without the fields a row is made of means the table is not the table
    Check("no_semicolons.txt", <<Row/binary, "garbage line here\n">>, error),
    Check("too_few_fields.txt", <<"0021;EXCLAMATION MARK\n">>, error),
    Check("binary_junk.txt", <<0, 1, 2, 3, 255, 254>>, error),
    ok = tflite_beam_private_utils_unicode_data:release_memory(),
    %% and the table that ships still reads
    Shipped = filename:join(code:priv_dir(tflite_beam), "unicode_data.txt"),
    ?assertEqual(842, length(
        tflite_beam_private_utils_unicode_data:get_puncuation_list_from_unicode_data(Shipped))).

%% A tensor with a zero in its shape gets no buffer, and the null data pointer
%% that leaves behind reads exactly like one from an interpreter whose tensors
%% were never allocated. The NIF guessed the second and said so, which sent a
%% caller who had just called allocate_tensors back to call it again.
an_empty_tensor_says_so_rather_than_blaming_allocate_tensors(_Config) ->
    {Builder, Interpreter} = tflite_beam_test_models:builder("add.bin"),
    _ = tflite_beam_interpreter_builder:build(Builder, Interpreter),
    ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
    {ok, [Input | _]} = tflite_beam_interpreter:inputs(Interpreter),

    Readable = tflite_beam_interpreter:tensor(Interpreter, Input),
    ?assert(is_binary(tflite_beam_tensor:to_binary(Readable))),

    ok = tflite_beam_interpreter:resize_input_tensor(Interpreter, Input, [0, 8, 8, 3]),
    ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
    Empty = tflite_beam_interpreter:tensor(Interpreter, Input),
    {error, Reason} = tflite_beam_tensor:to_binary(Empty),
    ?assertNotEqual(nomatch, binary:match(Reason, <<"[0,8,8,3]">>)),
    ?assertEqual(nomatch, binary:match(Reason, <<"Please call">>)),

    %% the same shape without allocate_tensors reaches the same null pointer, and
    %% the answer must not claim to know which of the two it was
    {FreshBuilder, Fresh} = tflite_beam_test_models:builder("add.bin"),
    _ = tflite_beam_interpreter_builder:build(FreshBuilder, Fresh),
    ok = tflite_beam_interpreter:resize_input_tensor(Fresh, Input, [0, 8, 8, 3]),
    {error, Unallocated} =
        tflite_beam_tensor:to_binary(tflite_beam_interpreter:tensor(Fresh, Input)),
    ?assertEqual(nomatch, binary:match(Unallocated, <<"Please call">>)).

%% Chinese is written without spaces, so without this every sentence arrived as
%% one word, ran past wordpiece's two hundred character limit, and came back as
%% [UNK]. A sentence whose every character was in the vocabulary was answered as
%% nothing at all.
ideographs_are_split_the_way_bert_splits_them(_Config) ->
    ?assertEqual([<<"这"/utf8>>, <<"是"/utf8>>, <<"中"/utf8>>, <<"文"/utf8>>],
                 tflite_beam_basic_tokenizer:tokenize(<<"这是中文"/utf8>>, true)),

    %% and it stays split once it is next to words that are already spaced
    ?assertEqual([<<"hello">>, <<"这"/utf8>>, <<"是"/utf8>>, <<"world">>],
                 tflite_beam_basic_tokenizer:tokenize(<<"hello 这是 world"/utf8>>, true)),

    %% kana is not an ideograph and BERT does not split it, so neither do we
    ?assertEqual([<<"日"/utf8>>, <<"本"/utf8>>, <<"語"/utf8>>, <<"のテキスト"/utf8>>,
                  <<"、"/utf8>>, <<"句"/utf8>>, <<"読"/utf8>>, <<"点"/utf8>>, <<"。"/utf8>>],
                 tflite_beam_basic_tokenizer:tokenize(<<"日本語のテキスト、句読点。"/utf8>>, true)),

    %% every character in the vocabulary, so the whole sentence resolves
    Vocabulary = #{<<"这"/utf8>> => 1, <<"是"/utf8>> => 2, <<"中"/utf8>> => 3,
                   <<"文"/utf8>> => 4, <<"[UNK]">> => 0},
    ?assertEqual([<<"这"/utf8>>, <<"是"/utf8>>, <<"中"/utf8>>, <<"文"/utf8>>],
                 tflite_beam_full_tokenizer:tokenize(<<"这是中文"/utf8>>, true, Vocabulary)),

    %% latin text is untouched by any of this
    ?assertEqual([<<"hello">>, <<"world">>, <<"!">>],
                 tflite_beam_basic_tokenizer:tokenize(<<"Hello World!">>, true)).

%% punctuation_set/1 fed the parser's {error, Reason} straight into a list
%% comprehension, so a table that could not be read raised bad_generator instead
%% of naming the file or the reason.
an_unreadable_unicode_table_names_itself(Config) ->
    Dir = proplists:get_value(priv_dir, Config),
    Missing = filename:join(Dir, "not_here.txt"),
    ok = tflite_beam_private_utils_unicode_data:release_memory(),
    ?assertError({unicode_data_unavailable, Missing, _},
                 tflite_beam_private_utils_unicode_data:punctuation_set(fun() -> Missing end)),

    Malformed = filename:join(Dir, "not_a_table.txt"),
    ok = file:write_file(Malformed, <<"garbage line here\n">>),
    ok = tflite_beam_private_utils_unicode_data:release_memory(),
    ?assertError({unicode_data_unavailable, Malformed, _},
                 tflite_beam_private_utils_unicode_data:punctuation_set(fun() -> Malformed end)),

    ok = tflite_beam_private_utils_unicode_data:release_memory(),
    ?assertEqual([<<"ok">>, <<".">>], tflite_beam_basic_tokenizer:tokenize(<<"ok.">>, true)),

    %% both caches are keyed by the file. Answering from either of them whatever
    %% file was named would hand a caller asking for one table the other one.
    OneRow = filename:join(Dir, "one_row.txt"),
    ok = file:write_file(OneRow, <<"0021;EXCLAMATION MARK;Po;0;ON;;;;;N;;;;;\n">>),
    Shipped = filename:join(code:priv_dir(tflite_beam), "unicode_data.txt"),
    ok = tflite_beam_private_utils_unicode_data:release_memory(),
    ?assertEqual(842, maps:size(tflite_beam_private_utils_unicode_data:punctuation_set(
                                   fun() -> Shipped end))),
    ?assertEqual(1, maps:size(tflite_beam_private_utils_unicode_data:punctuation_set(
                                  fun() -> OneRow end))),
    ?assertEqual(842, maps:size(tflite_beam_private_utils_unicode_data:punctuation_set(
                                    fun() -> Shipped end))),
    ok = tflite_beam_private_utils_unicode_data:release_memory().

%% release_memory/0 used to erase and then stop, and the caller did its own put,
%% so a populate that had already read the table could put it back after the
%% erase and after release_memory/0 had answered ok. 26 of 80 rounds brought the
%% cache back that way. The publish now happens inside the process that
%% gen_server:stop/1 drains, and the stop happens first.
releasing_the_table_beats_a_populate_already_running(_Config) ->
    Key = {tflite_beam_private_utils_unicode_data, punctuation_set},
    File = filename:join(code:priv_dir(tflite_beam), "unicode_data.txt"),
    Alive =
        [begin
            ok = tflite_beam_private_utils_unicode_data:release_memory(),
            Populate = spawn(fun() ->
                catch tflite_beam_private_utils_unicode_data:punctuation_set(fun() -> File end)
            end),
            case Round rem 3 of
                0 -> timer:sleep(1);
                _ -> ok
            end,
            ok = tflite_beam_private_utils_unicode_data:release_memory(),
            Present = persistent_term:get(Key, absent) =/= absent,
            exit(Populate, kill),
            Present
         end || Round <- lists:seq(1, 80)],
    ?assertEqual([], [P || P <- Alive, P]),
    ok = tflite_beam_private_utils_unicode_data:release_memory(),
    ?assertEqual([<<"ok">>, <<".">>], tflite_beam_basic_tokenizer:tokenize(<<"ok.">>, true)).

%% Two halves of the same check. inside_cache/1 reads the name and cannot see a
%% symlink; a component with no ".." in it still lands outside when something on
%% the way points there. And the flattening that made the name check work for
%% binaries must not turn into a rule that a filename has to be UTF-8, because
%% on a filesystem that promises no encoding a raw binary is how Erlang spells
%% a perfectly ordinary name.
a_symlink_out_of_the_cache_is_refused_and_a_raw_name_is_not(Config) ->
    Cache = filename:join(proplists:get_value(priv_dir, Config), "cache"),
    ok = filelib:ensure_dir(filename:join(Cache, "keep")),
    Previous = os:getenv("TFLITE_BEAM_CACHE_DIR"),
    true = os:putenv("TFLITE_BEAM_CACHE_DIR", Cache),
    try
        Escape = filename:join(Cache, "escapehatch"),
        _ = file:delete(Escape),
        ok = file:make_symlink("/etc", Escape),
        ?assertMatch({error, _},
                     tflite_beam_utils_downloader:download(
                       "ftp://example.invalid/x", <<"escapehatch">>, <<"passwd">>, false)),
        {error, Reason} = tflite_beam_utils_downloader:download(
                            "ftp://example.invalid/x", <<"escapehatch">>, <<"passwd">>, false),
        ?assertNotEqual(nomatch,
                        binary:match(iolist_to_binary(Reason), <<"outside the cache">>)),

        %% a name that is not UTF-8 is a name, not an escape: it must get past the
        %% check and fail later on the invalid URL like any other ordinary name
        {error, RawReason} = tflite_beam_utils_downloader:download(
                               "ftp://example.invalid/x", <<"models">>, <<"mo", 16#FF, "del">>, false),
        ?assertEqual(nomatch,
                     binary:match(iolist_to_binary(RawReason), <<"outside the cache">>))
    after
        case Previous of
            false -> os:unsetenv("TFLITE_BEAM_CACHE_DIR");
            _ -> os:putenv("TFLITE_BEAM_CACHE_DIR", Previous)
        end
    end.

%% Every name in here comes out of the model file, and an atom is never
%% reclaimed: a node that keeps loading models with names of their own used to
%% grow the atom table until it came down. The neighbours signature_keys/1,
%% signature_inputs/2 and signature_outputs/2 always answered binaries; this one
%% did not, so the same name had two types depending on which call you asked.
signature_defs_names_are_binaries_not_atoms(_Config) ->
    {ok, Interpreter} = tflite_beam_interpreter:new(tflite_beam_test_models:path("add.bin")),
    ok = tflite_beam_interpreter:allocate_tensors(Interpreter),

    {ok, Defs} = tflite_beam_interpreter:get_signature_defs(Interpreter),
    [SignatureName] = maps:keys(Defs),
    ?assert(is_binary(SignatureName)),

    %% and the same name as the neighbour that always answered a binary
    ?assertEqual(tflite_beam_interpreter:signature_keys(Interpreter), [SignatureName]),

    #{inputs := Inputs, outputs := Outputs} = maps:get(SignatureName, Defs),
    ?assertEqual([], [K || K <- maps:keys(Inputs), not is_binary(K)]),
    ?assertEqual([], [K || K <- maps:keys(Outputs), not is_binary(K)]),

    %% reading it again makes no new atoms
    Before = erlang:system_info(atom_count),
    [tflite_beam_interpreter:get_signature_defs(Interpreter) || _ <- lists:seq(1, 500)],
    ?assertEqual(Before, erlang:system_info(atom_count)).

%% Asking for a secure download and getting an unverified one is the failure this
%% guards. The certificate paths are hard coded and none of them is guaranteed to
%% exist on the small systems this library ships binaries for, and what used to
%% happen there was a warning followed by verify_none: the download proceeded,
%% unauthenticated, for a caller who never asked for that. TFLITE_BEAM_UNSAFE_HTTPS
%% is how someone asks.
an_https_download_without_a_ca_store_is_refused_not_downgraded(_Config) ->
    Cacert = os:getenv("TFLITE_BEAM_CACERT"),
    Unsafe = os:getenv("TFLITE_BEAM_UNSAFE_HTTPS"),
    Restore = fun() ->
        case Cacert of false -> os:unsetenv("TFLITE_BEAM_CACERT"); _ -> os:putenv("TFLITE_BEAM_CACERT", Cacert) end,
        case Unsafe of false -> os:unsetenv("TFLITE_BEAM_UNSAFE_HTTPS"); _ -> os:putenv("TFLITE_BEAM_UNSAFE_HTTPS", Unsafe) end
    end,
    try
        true = os:putenv("TFLITE_BEAM_CACERT", "/tflite_beam_no_such_ca_store.pem"),
        os:unsetenv("TFLITE_BEAM_UNSAFE_HTTPS"),
        {error, Reason} = tflite_beam_utils_downloader:download(
                            "https://example.invalid/x", "models", "a.bin", true),
        Flat = iolist_to_binary(Reason),
        ?assertNotEqual(nomatch, binary:match(Flat, <<"cannot be verified">>)),
        %% and it names both ways out rather than only complaining
        ?assertNotEqual(nomatch, binary:match(Flat, <<"TFLITE_BEAM_CACERT">>)),
        ?assertNotEqual(nomatch, binary:match(Flat, <<"TFLITE_BEAM_UNSAFE_HTTPS">>)),

        %% asked for explicitly, it goes ahead and fails on the address instead
        true = os:putenv("TFLITE_BEAM_UNSAFE_HTTPS", "false"),
        {error, Other} = tflite_beam_utils_downloader:download(
                           "https://example.invalid/x", "models", "a.bin", true),
        ?assertEqual(nomatch, binary:match(iolist_to_binary(io_lib:format("~p", [Other])),
                                           <<"cannot be verified">>))
    after
        Restore()
    end.

%% Pinning what the docs now say, because it is the opposite of what a timeout
%% usually suggests: the caller stops waiting, the server does not stop working,
%% and whoever asks next waits out the remainder of a call nobody is listening to.
a_timeout_ends_the_wait_and_not_the_work(_Config) ->
    {ok, Server} = tflite_beam_interpreter_server:start_link(
                     tflite_beam_test_models:path("add.bin"), []),
    Parent = self(),
    Caller = spawn(fun() ->
        Answer = (catch tflite_beam_interpreter_server:with(
                          Server, fun(_) -> timer:sleep(1500), slow end, 200)),
        Parent ! {gave_up, Answer}
    end),
    receive
        {gave_up, GaveUp} ->
            ?assertMatch({'EXIT', {timeout, _}}, GaveUp)
    after 5000 ->
            exit(Caller, kill),
            ct:fail(the_caller_never_gave_up)
    end,

    %% the server is still busy with the call the caller walked away from
    ?assert(is_process_alive(Server)),
    Started = erlang:monotonic_time(millisecond),
    ?assertEqual(second, tflite_beam_interpreter_server:with(Server, fun(_) -> second end, 10000)),
    Waited = erlang:monotonic_time(millisecond) - Started,
    ?assert(Waited > 500,
            lists:flatten(io_lib:format("the next caller waited only ~pms", [Waited]))),
    ok = tflite_beam_interpreter_server:stop(Server).

%% tflite_beam_tensor:shape/1 returns a tuple and dims/1 returns a list, so
%% resizing a tensor to a shape this library itself handed out fed a tuple to a
%% function that had only an is_list clause, and got function_clause. Both
%% shapes have to be accepted.
a_resize_takes_the_shape_this_library_hands_out(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("add.bin"),
    ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
    {ok, [Index | _]} = tflite_beam_interpreter:inputs(Interpreter),
    Tensor = tflite_beam_interpreter:tensor(Interpreter, Index),

    Tuple = tflite_beam_tensor:shape(Tensor),
    List = tflite_beam_tensor:dims(Tensor),
    ?assert(is_tuple(Tuple)),
    ?assertEqual(List, tuple_to_list(Tuple)),

    ?assertEqual(ok, tflite_beam_interpreter:resize_input_tensor(Interpreter, Index, Tuple)),
    ?assertEqual(ok, tflite_beam_interpreter:resize_input_tensor(Interpreter, Index, List)),
    ?assertEqual(ok, tflite_beam_interpreter:resize_input_tensor_strict(Interpreter, Index, Tuple)),
    ?assertEqual(ok, tflite_beam_interpreter:resize_input_tensor_strict(Interpreter, Index, List)),

    %% the signature runner takes a name rather than an index, and a tuple used
    %% to fall past its string clause as well as its binary one
    SigInterpreter = tflite_beam_test_models:interpreter("multi_signature.tflite"),
    ok = tflite_beam_interpreter:allocate_tensors(SigInterpreter),
    {ok, Runner} = tflite_beam_interpreter:get_signature_runner(SigInterpreter, <<"add">>),
    {ok, [Name | _]} = tflite_beam_signature_runner:input_names(Runner),
    RunnerTensor = tflite_beam_interpreter:tensor(SigInterpreter, Index),
    RunnerTuple = tflite_beam_tensor:shape(RunnerTensor),
    ?assertEqual(ok, tflite_beam_signature_runner:resize_input_tensor(Runner, Name, RunnerTuple)),
    ?assertEqual(ok, tflite_beam_signature_runner:resize_input_tensor(
                       Runner, binary_to_list(Name), RunnerTuple)),
    ?assertEqual(ok, tflite_beam_signature_runner:resize_input_tensor_strict(
                       Runner, Name, RunnerTuple)),
    ?assertEqual(ok, tflite_beam_signature_runner:resize_input_tensor_strict(
                       Runner, binary_to_list(Name), RunnerTuple)).

%% unicode:characters_to_nfc_binary/1 answers {error, Done, Rest} rather than
%% raising, and that tuple went straight on to characters_to_list/1, which raised
%% badarg from inside unicode with nothing to say the text was the problem.
text_that_is_not_utf8_is_named_not_raised_from_unicode(_Config) ->
    Good = <<"Hello World">>,
    ?assertEqual([<<"Hello">>, <<"World">>],
                 tflite_beam_basic_tokenizer:tokenize(Good, false)),

    %% every malformed shape reaches the same answer: a lone continuation byte,
    %% and a multi-byte sequence cut short both here and at the very end
    lists:foreach(
        fun(Malformed) ->
            ?assertError({invalid_utf8, _},
                         tflite_beam_basic_tokenizer:tokenize(Malformed, false),
                         Malformed)
        end,
        [
            <<"hello ", 255, 254, " world">>,
            <<"hello ", 16#E4, 16#B8, " world">>,
            <<"hello ", 16#E4, 16#B8>>,
            <<16#C3>>
        ]).

%% characters_to_nfc_binary/1 also documents an {incomplete, _, _}, but nothing
%% reaches it here: a binary or an iolist cut short mid-sequence comes back as
%% {error, _, _}, and dialyzer says the same of the clause that was written for
%% it, so there is no branch left to test.

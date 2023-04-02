%% @doc
%% Runs WordPiece tokenziation.

-module(tflite_beam_wordpiece_tokenizer).
-export([
    tokenize/2
]).

-define(MAX_INPUT_CHARS_PER_WORD, 200).

%% @doc
%% Tokenizes a piece of text into its word pieces.
%%
%% This uses a greedy longest-match-first algorithm to perform tokenization using the given
%% vocabulary.
%%
%% For example:
%%
%% ```
%% Input = "unaffable".
%% Output = ["una", "##ffa", "##ble"].
%% '''
%%
%% ```
%% Input = "unaffableX".
%% Output = ["[UNK]"].
%% '''
%%
%% Related link: https://github.com/tensorflow/examples/blob/master/lite/examples/bert_qa/ios/BertQACore/Models/Tokenizers/WordpieceTokenizer.swift
-spec tokenize(binary(), map()) -> list(binary()).
tokenize(BinaryText, VocabularyID) ->
    SplittedByWhitespace = tflite_beam_basic_tokenizer:split_by_whitespace(BinaryText),
    tokenize_impl(SplittedByWhitespace, VocabularyID, []).

tokenize_impl([], _VocabularyID, OutputTokens) -> lists:flatten(OutputTokens);
tokenize_impl([Token | Rest], VocabularyID, OutputTokens) ->
    TokenLength = byte_size(Token),
    if
        TokenLength > ?MAX_INPUT_CHARS_PER_WORD ->
            tokenize_impl(Rest, VocabularyID, OutputTokens);
        true ->
            Subwords = find_subwords(0, 0, TokenLength, Token, VocabularyID, []),
            tokenize_impl(Rest, VocabularyID, OutputTokens ++ [Subwords])
    end.

find_subwords(_OriginalStart, Start, End, _Token, _VocabularyID, Subwords) when Start >= End ->
    Subwords;
find_subwords(OriginalStart, Start, End, Token, VocabularyID, Subwords) when Start < End ->
    {HasFound, SubwordsFound, UpdatedEnd} = find_subwords_do_find(OriginalStart, Start, End, Token, VocabularyID, Subwords),
    if 
        HasFound ->
            find_subwords(OriginalStart, UpdatedEnd, End, Token, VocabularyID, SubwordsFound);
        true ->
            [<<"[UNK]">>]
    end.

find_subwords_do_find(_OriginalStart, Start, End, _Token, _VocabularyID, Subwords) when Start >= End ->
    {false, Subwords, End};
find_subwords_do_find(OriginalStart, Start, End, Token, VocabularyID, Subwords) when Start < End ->
    Substr = binary:part(Token, {Start, End - Start}),
    TargetSubstr = 
        if
            (Start > OriginalStart) ->
                SS = unicode:characters_to_binary("##"),
                <<SS/binary, Substr/binary>>;
            true ->
                Substr
        end,
    InVocab = maps:is_key(TargetSubstr, VocabularyID),
    if
        InVocab ->
            {true, Subwords ++ [TargetSubstr], End};
        true ->
            find_subwords_do_find(OriginalStart, Start, End - 1, Token, VocabularyID, Subwords)
    end.

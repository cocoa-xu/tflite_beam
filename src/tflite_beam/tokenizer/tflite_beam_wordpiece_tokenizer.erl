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

%% Built back to front. Appending each word's pieces to the end of the
%% accumulator copied everything gathered so far, once per word, which is
%% quadratic in the word count: 16k words took 332ms against 4.7ms for 2k.
tokenize_impl([], _VocabularyID, OutputTokens) -> lists:flatten(lists:reverse(OutputTokens));
tokenize_impl([Token | Rest], VocabularyID, OutputTokens) ->
    TokenLength = byte_size(Token),
    %% The limit counts characters, and this measured bytes, so a word in any
    %% script that does not fit one character to a byte was cut short well before
    %% two hundred of them. An over-long word also used to vanish from the output
    %% entirely, where the reference implementation this follows, and the example
    %% in the doc above, both say [UNK].
    CharacterCount = length(unicode_characters(Token)),
    if
        CharacterCount > ?MAX_INPUT_CHARS_PER_WORD ->
            tokenize_impl(Rest, VocabularyID, [[<<"[UNK]">>] | OutputTokens]);
        true ->
            Subwords = find_subwords(0, 0, TokenLength, Token, VocabularyID, []),
            tokenize_impl(Rest, VocabularyID, [Subwords | OutputTokens])
    end.

unicode_characters(Token) ->
    case unicode:characters_to_list(Token) of
        Characters when is_list(Characters) -> Characters;
        _ -> binary_to_list(Token)
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

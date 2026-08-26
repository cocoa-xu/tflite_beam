%% @doc
%% Runs basic tokenization such as punctuation spliting, lower casing.
%%
%% Related link: https://github.com/tensorflow/examples/blob/master/lite/examples/bert_qa/ios/BertQACore/Models/Tokenizers/BasicTokenizer.swift

-module(tflite_beam_basic_tokenizer).
-export([
    tokenize/2,
    split_by_whitespace/1
]).

-define(APPNAME, tflite_beam).
-define(UNICODE_DATA_FILENAME, "unicode_data.txt").

%% @doc
%% Tokenizes a piece of text.
-spec tokenize(binary() | list(), boolean()) -> list(binary()).
tokenize(Text, IsCaseInsensitive) when is_binary(Text) and is_boolean(IsCaseInsensitive) ->
    CleanedText = clean_text(Text),
    ProcessedText = if 
        IsCaseInsensitive ->
            string:to_lower(CleanedText);
        true ->
            CleanedText
    end,
    ProcessedBinaryText = unicode:characters_to_binary(space_out_ideographs(ProcessedText)),
    SplittedByWhitespace = split_by_whitespace(ProcessedBinaryText),
    PunctuationSet = punctuation_set(),
    TokenizedWithPunctuation = lists:map(
        fun(BinaryText) ->
            lists:map(
                fun(X) ->
                    case is_list(X) of
                        true ->
                            unicode:characters_to_binary(X);
                        false ->
                            unicode:characters_to_binary([X])
                    end
                end,
                tokenized_with_punctuation(BinaryText, PunctuationSet)
            )
        end,
        SplittedByWhitespace
    ),
    lists:flatten(TokenizedWithPunctuation).

%% @doc
%% Normalize string to NFC(Normalization Form Canonical Composition)
-spec normalize_to_nfc(binary() | list()) -> binary().
normalize_to_nfc(Text) when is_binary(Text) or is_list(Text) ->
    unicode:characters_to_nfc_binary(Text).

-spec clean_text(binary() | list()) -> binary().
clean_text(Text) when is_binary(Text) or is_list(Text) ->
    NfcText = normalize_to_nfc(Text),
    UnicodeScalars = unicode:characters_to_list(NfcText),
    flatmap(
        fun(CodePoint) ->
            IsWhitespace = is_whilespace(CodePoint),
            IsControl = is_control(CodePoint) or should_be_removed_for_bert(CodePoint),
            if
                IsWhitespace ->
                    " ";
                IsControl ->
                    "";
                true ->
                    CodePoint
            end
        end,
        UnicodeScalars
    ).

split_by_whitespace(BinaryText) ->
    split_by_whitespace_impl(BinaryText, []).

split_by_whitespace_impl(BinaryText, Acc) ->
    case binary:split(BinaryText, <<" ">>) of
        [Head, Rest] ->
            UpdatedAcc = case Head of
                <<"">> ->
                    Acc;
                _ ->
                    [Head | Acc]
            end,
            split_by_whitespace_impl(Rest, UpdatedAcc);
        [Head] ->
            UpdatedAcc = case Head of
                <<"">> ->
                    Acc;
                _ ->
                    [Head | Acc]
            end,
            lists:reverse(UpdatedAcc)
    end.

%% The table is fetched once for the whole call and carried down. Reaching for
%% it per code point cost a path lookup each time even with the set cached, and
%% that lookup was most of what tokenizing a character cost.
tokenized_with_punctuation(BinaryText, PunctuationSet) ->
    NfcText = normalize_to_nfc(BinaryText),
    UnicodeScalars = unicode:characters_to_list(NfcText),
    tokenized_with_punctuation_impl(UnicodeScalars, [], nil, PunctuationSet).

%% Both accumulators are built back to front. Growing them at the end copied
%% what was already there once per character, so one run of text between two
%% spaces cost time squared in its own length: 8000 characters took 92ms against
%% 1.3ms for 1000. Text without spaces in it is the ordinary case for CJK.
tokenized_with_punctuation_impl([], Tokens, CurrentToken, _PunctuationSet) ->
    lists:reverse(emit(CurrentToken, Tokens));
tokenized_with_punctuation_impl([CodePoint | RestUnicodeScalars], Tokens, CurrentToken, PunctuationSet) ->
    {UpdatedTokens, UpdatedCurrentToken} =
        case is_punctuation(CodePoint, PunctuationSet) of
            true ->
                {[CodePoint | emit(CurrentToken, Tokens)], nil};
            false when CurrentToken =:= nil ->
                {Tokens, [CodePoint]};
            false ->
                {Tokens, [CodePoint | CurrentToken]}
        end,
    tokenized_with_punctuation_impl(RestUnicodeScalars, UpdatedTokens, UpdatedCurrentToken, PunctuationSet).

emit(nil, Tokens) -> Tokens;
emit(CurrentToken, Tokens) -> [lists:reverse(CurrentToken) | Tokens].


%% BERT puts a space either side of every CJK ideograph before splitting on
%% whitespace, because Chinese is written without spaces and would otherwise
%% arrive as one word. Skipping that step did not merely glue the characters
%% together: the joined run runs past wordpiece's two hundred character limit
%% and comes back as [UNK], so a sentence whose every character is in the
%% vocabulary was answered as nothing at all.
%%
%% The ranges are the ones BERT uses, which are the ideographs alone. Kana and
%% Hangul are deliberately not here: BERT treats those as ordinary words, and
%% they are written with spacing that makes that work.
space_out_ideographs(Text) ->
    lists:foldr(
        fun(CodePoint, Acc) ->
            case is_ideograph(CodePoint) of
                true -> [$\s, CodePoint, $\s | Acc];
                false -> [CodePoint | Acc]
            end
        end,
        [],
        unicode:characters_to_list(Text)).

-spec is_ideograph(integer()) -> boolean().
is_ideograph(CodePoint) ->
    (CodePoint >= 16#4E00 andalso CodePoint =< 16#9FFF) orelse
    (CodePoint >= 16#3400 andalso CodePoint =< 16#4DBF) orelse
    (CodePoint >= 16#F900 andalso CodePoint =< 16#FAFF) orelse
    (CodePoint >= 16#20000 andalso CodePoint =< 16#2A6DF) orelse
    (CodePoint >= 16#2A700 andalso CodePoint =< 16#2B73F) orelse
    (CodePoint >= 16#2B740 andalso CodePoint =< 16#2B81F) orelse
    (CodePoint >= 16#2B820 andalso CodePoint =< 16#2CEAF) orelse
    (CodePoint >= 16#2F800 andalso CodePoint =< 16#2FA1F).

is_punctuation(CodePoint, PunctuationSet) ->
    IsASCII = is_ascii(CodePoint),
    IsAlphaNumeric = is_alphanumeric(CodePoint),
    NonAlphaNumericASCII = IsASCII andalso (CodePoint > 32) andalso (not IsAlphaNumeric),
    if 
        NonAlphaNumericASCII ->
            true;
        true ->
            maps:is_key(CodePoint, PunctuationSet)
    end.

is_whilespace(CodePoint) ->
    lists:member(CodePoint, whitespace_list()).

is_control(CodePoint) ->
    IsWhitespace = is_whilespace(CodePoint),
    IsFormat = is_format(CodePoint),
    if 
        IsWhitespace ->
            false;
        (CodePoint >= 16#0000) and (CodePoint =< 16#001F) ->
            true;
        CodePoint == 16#007F ->
            true;
        IsFormat ->
            true;
        true ->
            false
    end.

is_format(CodePoint) ->
    lists:member(CodePoint, format_list()).

should_be_removed_for_bert(CodePoint) ->
    ((CodePoint == 0) or (CodePoint == 16#FFFD)).

punctuation_set() ->
    tflite_beam_private_utils_unicode_data:punctuation_set(fun unicode_data_file/0).

unicode_data_file() ->
    case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?UNICODE_DATA_FILENAME]);
                _ ->
                    filename:join([priv, ?UNICODE_DATA_FILENAME])
            end;
        Dir ->
            filename:join(Dir, ?UNICODE_DATA_FILENAME)
    end.

-spec is_ascii(integer()) -> boolean().
is_ascii(CodePoint) ->
    (CodePoint >= 0) and (CodePoint =< 127).

-spec is_alphanumeric(integer()) -> boolean().
is_alphanumeric(CodePoint) ->
    ((CodePoint >= 16#0041) andalso (CodePoint =< 16#005A)) orelse 
    ((CodePoint >= 16#0061) andalso (CodePoint =< 16#007A)) orelse
    %% $0 is 48 and $9 is 57. This read 49 to 58, which is $1 to $:, so a zero
    %% was split off as punctuation and a colon was kept inside a word.
    ((CodePoint >= $0) andalso (CodePoint =< $9)).

format_list() ->
    [
        16#00AD, %% SOFT HYPHEN
        16#0600, %% ARABIC NUMBER SIGN
        16#0601, %% ARABIC SIGN SANAH
        16#0602, %% ARABIC FOOTNOTE MARKER
        16#0603, %% ARABIC SIGN SAFHA
        16#06DD, %% ARABIC END OF AYAH
        16#070F, %% SYRIAC ABBREVIATION MARK
        16#17B4, %% KHMER VOWEL INHERENT AQ
        16#17B5, %% KHMER VOWEL INHERENT AA
        16#200B, %% ZERO WIDTH SPACE
        16#200C, %% ZERO WIDTH NON-JOINER
        16#200D, %% ZERO WIDTH JOINER
        16#200E, %% LEFT-TO-RIGHT MARK
        16#200F, %% RIGHT-TO-LEFT MARK
        16#2028, %% LINE SEPARATOR
        16#2029, %% PARAGRAPH SEPARATOR
        16#202A, %% LEFT-TO-RIGHT EMBEDDING
        16#202B, %% RIGHT-TO-LEFT EMBEDDING
        16#202C, %% POP DIRECTIONAL FORMATTING
        16#202D, %% LEFT-TO-RIGHT OVERRIDE
        16#202E, %% RIGHT-TO-LEFT OVERRIDE
        16#2060, %% WORD JOINER
        16#2061, %% FUNCTION APPLICATION
        16#2062, %% INVISIBLE TIMES
        16#2063, %% INVISIBLE SEPARATOR
        16#2064, %% INVISIBLE PLUS
        16#2066, %% LEFT-TO-RIGHT ISOLATE
        16#2067, %% RIGHT-TO-LEFT ISOLATE
        16#2068, %% FIRST STRONG ISOLATE
        16#2069, %% POP DIRECTIONAL ISOLATE
        16#206A, %% INHIBIT SYMMETRIC SWAPPING
        16#206B, %% ACTIVATE SYMMETRIC SWAPPING
        16#206C, %% INHIBIT ARABIC FORM SHAPING
        16#206D, %% ACTIVATE ARABIC FORM SHAPING
        16#206E, %% NATIONAL DIGIT SHAPES
        16#206F  %% NOMINAL DIGIT SHAPES
    ].

whitespace_list() ->
    [
        32, 9, 13, 10,  %% " \t\r\n"
        16#00A0,        %% NO-BREAK SPACE
        16#1680,        %% OGHAM SPACE MARK
        16#2000,        %% EN QUAD
        16#2001,        %% EM QUAD
        16#2002,        %% EN SPACE
        16#2003,        %% EM SPACE
        16#2004,        %% THREE-PER-EM SPACE
        16#2005,        %% FOUR-PER-EM SPACE
        16#2006,        %% SIX-PER-EM SPACE
        16#2007,        %% FIGURE SPACE
        16#2008,        %% PUNCTUATION SPACE
        16#2009,        %% THIN SPACE
        16#200A,        %% HAIR SPACE
        16#202F,        %% NARROW NO-BREAK SPACE
        16#205F,        %% MEDIUM MATHEMATICAL SPACE
        16#3000         %% IDEOGRAPHIC SPACE
    ].

flatmap(Fun, List) ->
    lists:flatten(lists:map(Fun, List)).

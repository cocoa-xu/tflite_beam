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
    ProcessedBinaryText = unicode:characters_to_binary(ProcessedText),
    SplittedByWhitespace = split_by_whitespace(ProcessedBinaryText),
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
                tokenized_with_punctuation(BinaryText)
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

tokenized_with_punctuation(BinaryText) ->
    NfcText = normalize_to_nfc(BinaryText),
    UnicodeScalars = unicode:characters_to_list(NfcText),
    tokenized_with_punctuation_impl(UnicodeScalars, [], nil).

tokenized_with_punctuation_impl([], Tokens, CurrentToken) -> 
    case CurrentToken of
        nil ->
            Tokens;
        _ ->
            Tokens ++ [CurrentToken]
    end;
tokenized_with_punctuation_impl([CodePoint | RestUnicodeScalars], Tokens, CurrentToken) ->
    IsPuncuation = is_punctuation(CodePoint),
    {UpdatedTokens, UpdatedCurrentToken} = 
        case {IsPuncuation, CurrentToken} of
            {true, nil} ->
                {Tokens ++ [CodePoint], nil};
            {true, _} ->
                {Tokens ++ [CurrentToken, CodePoint], nil};
            {false, nil} ->
                {Tokens, [CodePoint]};
            {false, _} ->
                {Tokens, CurrentToken ++ [CodePoint]}
        end,
    tokenized_with_punctuation_impl(RestUnicodeScalars, UpdatedTokens, UpdatedCurrentToken).

is_punctuation(CodePoint) ->
    IsASCII = is_ascii(CodePoint),
    IsAlphaNumeric = is_alphanumeric(CodePoint),
    NonAlphaNumericASCII = IsASCII andalso (CodePoint > 32) andalso (not IsAlphaNumeric),
    if 
        NonAlphaNumericASCII ->
            true;
        true ->
            lists:member(CodePoint, punctuation_list())
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

punctuation_list() ->
    tflite_beam_private_utils_unicode_data:get_puncuation_list_from_unicode_data(unicode_data_file()).

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
    ((CodePoint >= 49) andalso (CodePoint =< 58)).

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

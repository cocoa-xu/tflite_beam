defmodule TFLiteBEAM.Tokenizer.WordpieceTokenizer do
  @moduledoc """
  Runs WordPiece tokenziation.
  """

  @doc """
  Tokenizes a piece of text into its word pieces.

  This uses a greedy longest-match-first algorithm to perform tokenization using the given
  vocabulary.

  For example:

  ```
  input = "unaffable".
  output = ["una", "##ffa", "##ble"].
  ```

  ```
  input = "unaffableX".
  output = ["[UNK]"].
  ```

  Related link: https://github.com/tensorflow/examples/blob/master/lite/examples/bert_qa/ios/BertQACore/Models/Tokenizers/WordpieceTokenizer.swift
  """
  @spec tokenize(String.t(), map()) :: [String.t()]
  def tokenize(text, vocab) when is_binary(text) and is_map(vocab) do
    :tflite_beam_wordpiece_tokenizer.tokenize(text, vocab)
  end
end

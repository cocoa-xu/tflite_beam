defmodule TFLiteBeam.Tokenizer.FullTokenizer do
  @moduledoc """
  Runs end-to-end tokenization.

  Related link: [FullTokenizer.swift](https://github.com/tensorflow/examples/blob/master/lite/examples/bert_qa/ios/BertQACore/Models/Tokenizers/FullTokenizer.swift)
  """

  @doc """
  End-to-end tokenization.
  """
  @spec tokenize(String.t(), boolean(), map()) :: [String.t()]
  def tokenize(text, is_case_insensitive, vocab) when is_binary(text) and is_boolean(is_case_insensitive) and is_map(vocab) do
    :tflite_beam_full_tokenizer.tokenize(text, is_case_insensitive, vocab)
  end

  @doc """
  Convert to ID in the vocab
  """
  @spec convert_to_id([String.t()], map()) :: {:ok, [integer()]} | {:error, String.t()}
  def convert_to_id(tokens, vocab) when is_list(tokens) and is_map(vocab) do
    :tflite_beam_full_tokenizer.convert_to_id(tokens, vocab)
  end
end

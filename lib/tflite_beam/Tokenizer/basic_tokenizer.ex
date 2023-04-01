defmodule TFLiteBEAM.Tokenizer.BasicTokenizer do
  @moduledoc """
  Runs basic tokenization such as punctuation spliting, lower casing.
  """

  @doc """
  Tokenizes a piece of text.
  """
  @spec tokenize(String.t(), boolean()) :: [String.t()]
  def tokenize(text, is_case_insensitive) do
    :tflite_beam_basic_tokenizer.tokenize(text, is_case_insensitive)
  end
end

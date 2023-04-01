defmodule TFLiteBEAM.Tokenizer.WordpieceTokenizer.Test do
  use ExUnit.Case

  alias TFLiteBEAM.Tokenizer.WordpieceTokenizer

  @mini_vocab %{
    "hello" => 0,
    "world" => 1,
    "una" => 2,
    "##ffa" => 3,
    "##ble" => 4,
  }

  defp mini_vocab, do: @mini_vocab

  test "wordpiece tokenizer" do
    assert ["una", "##ffa", "##ble"] == WordpieceTokenizer.tokenize("unaffable", mini_vocab())
    assert ["hello", "world"] == WordpieceTokenizer.tokenize("hello world", mini_vocab())
    assert ["[UNK]", "[UNK]"] == WordpieceTokenizer.tokenize("not exists", mini_vocab())
  end

  test "wordpiece tokenizer, more than 200 letters in a single word" do
    assert [] == WordpieceTokenizer.tokenize(String.duplicate("a", 201), mini_vocab())
  end
end

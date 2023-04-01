defmodule TFLiteBeam.Tokenizer.FullTokenizer.Test do
  use ExUnit.Case

  alias TFLiteBeam.Tokenizer.FullTokenizer

  @mini_vocab %{
    "hello" => 0,
    "world" => 1,
    "una" => 2,
    "##ffa" => 3,
    "##ble" => 4,
    "hi" => 5,
    "there" => 6,
    "!" => 7,
    "," => 8,
    "Hello" => 9,
    "World" => 10,
    "Hi" => 11,
    "[UNK]" => 1000
  }

  defp mini_vocab, do: @mini_vocab

  test "full tokenizer, case insensetive" do
    assert ["hello", "world", "!", "hi", ",", "there", "!", "!", "!", "[UNK]"] == FullTokenizer.tokenize("Hello World! Hi,there!!!Nonexistent", true, mini_vocab())
  end

  test "full tokenizer, case sensetive" do
    assert ["Hello", "World", "!", "Hi", ",", "there", "!", "!", "!", "[UNK]"] == FullTokenizer.tokenize("Hello World! Hi,there!!!Nonexistent", false, mini_vocab())
  end

  test "full tokenizer, case insensetive, convert to id" do
    tokens = FullTokenizer.tokenize("Hello World! Hi,there!!!Nonexistent", true, mini_vocab())
    assert {:ok, [0, 1, 7, 5, 8, 6, 7, 7, 7, 1000]} == FullTokenizer.convert_to_id(tokens, mini_vocab())
  end

  test "full tokenizer, case sensetive, convert to id" do
    tokens = FullTokenizer.tokenize("Hello World! Hi,there!!!Nonexistent", false, mini_vocab())
    assert {:ok, [9, 10, 7, 11, 8, 6, 7, 7, 7, 1000]} == FullTokenizer.convert_to_id(tokens, mini_vocab())
  end

  test "full tokenizer, token not in vocab" do
    {:error, reason} = FullTokenizer.convert_to_id(["a"], %{})
    assert "Cannot found token `a` in the given vocabulary map" == reason
  end
end

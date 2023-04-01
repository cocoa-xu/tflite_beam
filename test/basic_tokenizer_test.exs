defmodule TFLiteBEAM.Tokenizer.BasicTokenizer.Test do
  use ExUnit.Case

  alias TFLiteBEAM.Tokenizer.BasicTokenizer

  test "basic tokenizer, case insensetive" do
    assert ["hello", "world", "!"] == BasicTokenizer.tokenize("Hello World!", true)
  end

  test "basic tokenizer, case sensetive" do
    assert ["Hello", "World", "!"] == BasicTokenizer.tokenize("Hello World!", false)
  end

  test "basic tokenizer, multiple consecutive spaces" do
    assert ["Hello", "World", "!"] == BasicTokenizer.tokenize("Hello     World!", false)
  end

  test "basic tokenizer, with control characters" do
    assert ["Hello", "World", "!"] == BasicTokenizer.tokenize("Hello World!\a\b\x7f\r\n", false)
  end

  test "basic tokenizer, with format characters" do
    assert ["Hello", "World", "!"] == BasicTokenizer.tokenize("Hello World!" <> <<0x0600::utf8>>, false)
  end
end

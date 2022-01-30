defmodule TfliteElixirTest do
  use ExUnit.Case
  doctest TfliteElixir

  test "greets the world" do
    assert TfliteElixir.hello() == :world
  end
end

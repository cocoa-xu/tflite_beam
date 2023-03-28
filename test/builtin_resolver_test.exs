defmodule TFLiteBEAM.Ops.Builtin.BuiltinResolver.Test do
  use ExUnit.Case

  alias TFLiteBEAM.Ops.Builtin.BuiltinResolver

  test "new builtin resolver" do
    {:ok, resolver} = BuiltinResolver.new()
    assert is_reference(resolver)

    resolver = BuiltinResolver.new!()
    assert is_reference(resolver)
  end
end

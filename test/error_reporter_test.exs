defmodule TFLiteBEAM.ErrorReporter.Test do
  use ExUnit.Case

  alias TFLiteBEAM.ErrorReporter

  test "get default error reporter" do
    default_error_reporter = ErrorReporter.default_error_reporter()

    assert is_struct(default_error_reporter, ErrorReporter)
    assert is_reference(ErrorReporter.from_struct(default_error_reporter))
    assert is_nil(ErrorReporter.from_struct(nil))
  end
end

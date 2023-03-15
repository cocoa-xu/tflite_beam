defmodule TFLiteElixir.ErrorReporter do
  defstruct [:ref]

  alias __MODULE__, as: T

  @spec default_error_reporter :: %T{} | {:error, String.t()}
  def default_error_reporter do
    with {:ok, error_reporter} <- TFLiteElixir.Nif.errorReporter_DefaultErrorReporter() do
      %T{ref: error_reporter}
    else
      error -> error
    end
  end

  @doc false
  @spec from_struct(nil | %TFLiteElixir.ErrorReporter{:ref => reference()}) :: reference() | nil
  def from_struct(%T{ref: ref}), do: ref
  def from_struct(nil), do: nil
end

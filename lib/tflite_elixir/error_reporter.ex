defmodule TFLiteElixir.ErrorReporter do
  @moduledoc """
  ErrorReporter to provide reporting destinations.
  """
  defstruct [:ref]

  alias __MODULE__, as: T

  @doc """
  Get the default error reporter.

  The default error reporter simply writes the message to stderr.
  """
  @spec default_error_reporter :: %T{} | {:error, String.t()}
  def default_error_reporter do
    with {:ok, error_reporter} <- TFLiteElixir.Nif.error_reporter_default_error_reporter() do
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

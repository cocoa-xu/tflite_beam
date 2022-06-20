defmodule Mix.Tasks.ListEdgetpu do
  @moduledoc """
  List all available Edge Tpu mix task: `mix help list_edgetpu`
  """

  use Mix.Task

  @shortdoc "List all available Edge Tpu"
  def run(_) do
    case TFLiteElixir.Coral.edgeTpuDevices() do
      {:error, error} ->
        IO.puts("Error: #{error}")
      devices ->
        Enum.each(devices, fn name ->
          IO.puts(name)
        end)
    end
  end
end

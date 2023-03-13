defmodule TFLiteElixir.Coral do
  import TFLiteElixir.Errorize

  alias TFLiteElixir.FlatBufferModel, as: FlatBufferModel

  def containsEdgeTpuCustomOp?(%FlatBufferModel{model: model}) do
    TFLiteElixir.Nif.coral_contains_edgetpu_custom_op(model)
  end

  @spec edgeTpuDevices() :: [String.t()]
  def edgeTpuDevices() do
    TFLiteElixir.Nif.coral_edgetpu_devices()
  end

  @doc """
  Returns TPU context or nullptr if requested TPU context is not available.

    Parameter `device`:

    - ""      -- any TPU device
    - "usb"   -- any TPU device on USB bus
    - "pci"   -- any TPU device on PCIe bus
    - ":N"    -- N-th TPU device, e.g. ":0"
    - "usb:N" -- N-th TPU device on USB bus, e.g. "usb:0"
    - "pci:N" -- N-th TPU device on PCIe bus, e.g. "pci:0"

    Parameter `options`:

    - "Performance": ["Low", "Medium", "High", "Max"] (Default is "Max")
    Adjust internal clock rate to achieve different performance / power
    balance.
    - "Usb.AlwaysDfu": ["True", "False"] (Default is "False")
    Always perform device firmware update after reset. DFU is usually only
    necessary after power cycle.
    - "Usb.MaxBulkInQueueLength": ["0",.., "255"] (Default is "32")
    Larger queue length may improve USB performance on the direction from
    device to host.

    All TPUs are always enumerated in the same order assuming hardware
    configuration doesn't change (no added/removed devices between enumerations).
    Under the assumption above, the same index N will always point to the same
    device.

    Consider 2 USB devices and 4 PCIe devices connected to the host. The way to
    reference specifically USB devices:
        "usb:0", "usb:1".
    The way to reference specifically PCIe devices:
        "pci:0", "pci:1", "pci:2", "pci:3".
    The generic way to reference all devices (no assumption about device type):
        ":0", ":1", ":2", ":3", ":4", ":5".
  """
  @spec getEdgeTpuContext() :: {:ok, reference()} | {:error, String.t()}
  def getEdgeTpuContext() do
    TFLiteElixir.Nif.coral_get_edgetpu_context("", %{})
  end

  deferror(getEdgeTpuContext())

  @spec getEdgeTpuContext(String.t()) :: {:ok, reference()} | {:error, String.t()}
  def getEdgeTpuContext(device) do
    TFLiteElixir.Nif.coral_get_edgetpu_context(device, %{})
  end

  deferror(getEdgeTpuContext(device))

  @spec getEdgeTpuContext(String.t(), Map.t()) :: {:ok, reference()} | {:error, String.t()}
  def getEdgeTpuContext(device, options) do
    TFLiteElixir.Nif.coral_get_edgetpu_context(device, options)
  end

  deferror(getEdgeTpuContext(device, options))

  @spec makeEdgeTpuInterpreter(%FlatBufferModel{}, reference()) ::
          {:ok, reference()} | {:error, String.t()}
  def makeEdgeTpuInterpreter(%FlatBufferModel{model: model}, edgetpu_context) do
    TFLiteElixir.Nif.coral_make_edgetpu_interpreter(model, edgetpu_context)
  end

  deferror(makeEdgeTpuInterpreter(model, edgetpu_context))

  def dequantizeTensor(interpreter, tensor_index) do
    TFLiteElixir.Nif.coral_dequantize_tensor(interpreter, tensor_index, nil)
  end

  def dequantizeTensor(interpreter, tensor_index, as_type) do
    as_type = map_type(as_type)
    TFLiteElixir.Nif.coral_dequantize_tensor(interpreter, tensor_index, as_type)
  end

  defp map_type({:f, 32}), do: :f32
  defp map_type({:f, 64}), do: :f64
  defp map_type(:f32), do: :f32
  defp map_type(:f64), do: :f64

  defp map_type({:u, 8}), do: :u8
  defp map_type({:u, 16}), do: :u16
  defp map_type({:u, 32}), do: :u32
  defp map_type({:u, 64}), do: :u64
  defp map_type(:u8), do: :u8
  defp map_type(:u16), do: :u16
  defp map_type(:u32), do: :u32
  defp map_type(:u64), do: :u64

  defp map_type({:s, 8}), do: :s8
  defp map_type({:s, 16}), do: :s16
  defp map_type({:s, 32}), do: :s32
  defp map_type({:s, 64}), do: :s64
  defp map_type(:s8), do: :s8
  defp map_type(:s16), do: :s16
  defp map_type(:s32), do: :s32
  defp map_type(:s64), do: :s64

  defp map_type(nil), do: nil

  defp map_type(not_supported),
    do: raise(ArgumentError, "#{inspect(not_supported)} is not supported")
end

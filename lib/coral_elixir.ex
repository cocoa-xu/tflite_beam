if Application.compile_env(:tflite_elixir, :enable_coral_support, false) do
  defmodule TFLiteElixir.Coral do
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
    @spec getEdgeTpuContext() :: :ok
    def getEdgeTpuContext() do
      TFLiteElixir.Nif.coral_get_edgetpu_context("", %{})
    end

    @spec getEdgeTpuContext(String.t()) :: :ok
    def getEdgeTpuContext(device) do
      TFLiteElixir.Nif.coral_get_edgetpu_context(device, %{})
    end

    @spec getEdgeTpuContext(String.t(), Map.t()) :: :ok
    def getEdgeTpuContext(device, options) do
      TFLiteElixir.Nif.coral_get_edgetpu_context(device, options)
    end
  end
end

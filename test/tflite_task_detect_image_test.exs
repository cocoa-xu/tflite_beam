defmodule TFLiteElixir.Test.DetectImage do
  use ExUnit.Case

  import ExUnit.CaptureIO

  test "Classify Image (CPU)" do
    output =
      capture_io(fn ->
        Mix.Tasks.DetectImage.run(
          OptionParser.to_argv(
            model: "test/test_data/ssd_mobilenet_v2_coco_quant_postprocess.tflite",
            input: "test/test_data/cat.jpeg",
            labels: "test/test_data/coco_labels.txt",
            threshold: 0.4,
            count: 1,
            use_tpu: false,
            tpu: ""
          )
        )
      end)

    """
    cat
      id   : 16
      score: 0.953
      bbox : [3, -1, 294, 240]
    """ =
      String.split(output, "\n")
      |> List.delete_at(0)
      |> List.delete_at(0)
      |> Enum.join("\n")
  end

  @tag :require_tpu
  test "Classify Image (TPU)" do
    Mix.Tasks.DetectImage.run(
      OptionParser.to_argv(
        model: "test/test_data/ssd_mobilenet_v2_coco_quant_postprocess_edgetpu.tflite",
        input: "test/test_data/cat.jpeg",
        labels: "test/test_data/coco_labels.txt",
        threshold: 0.4,
        count: 1,
        use_tpu: true,
        tpu: ""
      )
    )
  end
end

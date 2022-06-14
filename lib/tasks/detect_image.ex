defmodule Mix.Tasks.DetectImage do
  @moduledoc """
  Image classification mix task: `mix help detect_image`

  Command line arguments:

  - `-m`, `--model`: *Required*. File path of .tflite file.
  - `-i`, `--input`: *Required*. Image to process.
  - `-l`, `--labels`: File path of labels file.
  - `-t`, `--threshold`: Default to `0.4`. Score threshold for detected objects.
  - `-c`, `--count`: Default to `1`. Number of times to run inference.

  Code based on [detect_image.py](https://github.com/google-coral/pycoral/blob/master/examples/detect_image.py)
  """

  use Mix.Task

  alias TFLiteElixir.Interpreter, as: Interpreter
  alias TFLiteElixir.InterpreterBuilder, as: InterpreterBuilder
  alias TFLiteElixir.TfLiteTensor, as: TFTensor
  alias TFLiteElixir.FlatBufferModel, as: FlatBufferModel

  @shortdoc "Object Detection"
  def run(argv) do
    {args, _, _} = OptionParser.parse(argv, strict: [
      model: :string,
      input: :string,
      labels: :string,
      threshold: :float,
      count: :integer,
    ], aliases: [
      m: :model,
      i: :input,
      l: :labels,
      t: :threshold,
      c: :count,
    ])

    default_values = [threshold: 0.4, count: 1]
    args = Keyword.merge(args, default_values, fn _k, user, default ->
      if user == nil do
        default
      else
        user
      end
    end)

    model = load_model(args[:model])
    input_image = %StbImage{shape: {h, w, _c}} = load_input(args[:input])
    labels = load_labels(args[:labels])
    interpreter = make_interpreter(model)
    Interpreter.allocateTensors!(interpreter)

    [input_tensor_number | _] = Interpreter.inputs!(interpreter)
    output_tensor_numbers = Interpreter.outputs!(interpreter)
    if Enum.count(output_tensor_numbers) != 4 do
      raise ArgumentError, "Object detection models should have 4 output tensors"
    end

    input_tensor = Interpreter.tensor!(interpreter, input_tensor_number)
    if input_tensor.type != {:u, 8} do
      raise ArgumentError, "Only support uint8 input type."
    end

    {height, width} =
      case input_tensor.shape do
        [_n, height, width, _c] ->
          {height, width}
        [_n, height, width] ->
          {height, width}
        shape ->
          raise RuntimeError, "not sure the input shape, got #{inspect(shape)}"
      end
    scale = min(height / h, width / w)
    {h, w} = {trunc(h * scale), trunc(w * scale)}
    input_image =
      StbImage.resize(input_image, h, w)
      |> StbImage.to_nx()
      |> Nx.new_axis(0)

    Nx.broadcast(0, List.to_tuple(input_tensor.shape))
    |> Nx.as_type(:u8)
    |> Nx.put_slice([0, 0, 0, 0], input_image)
    |> Nx.to_binary()
    |> then(&TFTensor.set_data(input_tensor, &1))

    IO.puts("----INFERENCE TIME----")
    for _ <- 1..args[:count] do
      start_time = :os.system_time(:microsecond)
      Interpreter.invoke!(interpreter)
      end_time = :os.system_time(:microsecond)
      inference_time = (end_time - start_time) / 1000.0
      IO.puts("#{Float.round(inference_time, 1)}ms")
    end

    output_tensor_3 = Interpreter.tensor!(interpreter, Enum.at(output_tensor_numbers, 3))
    {boxes, class_ids, scores, count} =
      if output_tensor_3.shape == [1] do
        boxes =
          Interpreter.tensor!(interpreter, Enum.at(output_tensor_numbers, 0))
          |> TFTensor.to_nx()
          |> take_first_and_reshape()
        class_ids =
          Interpreter.tensor!(interpreter, Enum.at(output_tensor_numbers, 1))
          |> TFTensor.to_nx()
          |> take_first_and_reshape()
        scores =
          Interpreter.tensor!(interpreter, Enum.at(output_tensor_numbers, 2))
          |> TFTensor.to_nx()
          |> take_first_and_reshape()
        count = TFTensor.to_nx(output_tensor_3) |> Nx.to_flat_list() |> hd() |> trunc()
        {boxes, class_ids, scores, count}
      end

    {sx, sy} = {height / scale, width / scale}
    Enum.each(0..count-1, fn index ->
      score =
        Nx.take(scores, index)
        |> Nx.to_flat_list()
        |> hd()
      if score >= args[:threshold] do
        [ymin, xmin, ymax, xmax] =
          Nx.take(boxes, index)
          |> Nx.multiply(scale)
          |> Nx.to_flat_list()
        {xmin, xmax} = {trunc(sx * xmin), trunc(sx * xmax)}
        {ymin, ymax} = {trunc(sy * ymin), trunc(sy * ymax)}

        class_id =
          Nx.take(class_ids, index)
          |> Nx.to_flat_list()
          |> hd()
          |> trunc()
        class_str =
          if labels != nil do
            Enum.at(labels, class_id)
          else
            class_id
          end

        IO.puts("#{class_str}")
        IO.puts("  id   : #{class_id}")
        IO.puts("  score: #{Float.round(score, 3)}")
        IO.puts("  bbox : #{inspect([ymin, xmin, ymax, xmax])}")
      end
    end)
  end

  defp load_model(nil) do
    raise ArgumentError, "empty value for argument '--model'"
  end

  defp load_model(model_path) do
    FlatBufferModel.buildFromBuffer!(File.read!(model_path))
  end

  defp load_input(nil) do
    raise ArgumentError, "empty value for argument '--input'"
  end

  defp load_input(input_path) do
    with {:ok, input_image} <- StbImage.read_file(input_path) do
      input_image
    else
      {:error, error} ->
        raise RuntimeError, error
    end
  end

  defp load_labels(nil), do: nil

  defp load_labels(label_file_path) do
    File.read!(label_file_path)
    |> String.split("\n")
  end

  defp make_interpreter(model) do
    resolver = TFLiteElixir.Ops.Builtin.BuiltinResolver.new!()
    builder = InterpreterBuilder.new!(model, resolver)
    interpreter = Interpreter.new!()
    InterpreterBuilder.setNumThreads!(builder, 2)
    :ok = InterpreterBuilder.build!(builder, interpreter)
    Interpreter.setNumThreads!(interpreter, 2)
    interpreter
  end

  defp take_first_and_reshape(tensor) do
    shape = Tuple.delete_at(Nx.shape(tensor), 0)
    Nx.take(tensor, 0)
    |> Nx.reshape(shape)
  end
end

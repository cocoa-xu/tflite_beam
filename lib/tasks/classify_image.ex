defmodule Mix.Tasks.ClassifyImage do
  @moduledoc """
  Image classification mix task: `mix help classify_image`

  Command line arguments:

  - `-m`, `--model`: *Required*. File path of .tflite file.
  - `-i`, `--input`: *Required*. Image to be classified.
  - `-l`, `--labels`: File path of labels file.
  - `-k`, `--top`: Default to `1`. Max number of classification results.
  - `-t`, `--threshold`: Default to `0.0`. Classification score threshold.
  - `-c`, `--count`: Default to `1`. Number of times to run inference.
  - `-a`, `--mean`: Default to `128.0`. Mean value for input normalization.
  - `-s`, `--std`: Default to `128.0`. STD value for input normalization.

  Code based on [classify_image.py](https://github.com/google-coral/pycoral/blob/master/examples/classify_image.py)
  """

  use Mix.Task

  alias TFLiteElixir.Interpreter, as: Interpreter
  alias TFLiteElixir.InterpreterBuilder, as: InterpreterBuilder
  alias TFLiteElixir.TfLiteTensor, as: TFTensor
  alias TFLiteElixir.FlatBufferModel, as: FlatBufferModel

  @shortdoc "Image Classification"
  def run(argv) do
    {args, _, _} = OptionParser.parse(argv, strict: [
      model: :string,
      input: :string,
      labels: :string,
      top: :integer,
      threshold: :float,
      count: :integer,
      mean: :float,
      std: :float
    ], aliases: [
      m: :model,
      i: :input,
      l: :labels,
      k: :top,
      t: :threshold,
      c: :count,
      a: :mean,
      s: :std
    ])

    default_values = [top: 1, threshold: 0.0, count: 1, mean: 128.0, std: 128.0]
    args = Keyword.merge(args, default_values, fn _k, user, default ->
      if user == nil do
        default
      else
        user
      end
    end)

    model = load_model(args[:model])
    input_image = load_input(args[:input])
    labels = load_labels(args[:labels])
    interpreter = make_interpreter(model)
    Interpreter.allocateTensors!(interpreter)

    [input_tensor_number | _] = Interpreter.inputs!(interpreter)
    [output_tensor_number | _] = Interpreter.outputs!(interpreter)
    input_tensor = Interpreter.tensor!(interpreter, input_tensor_number)
    if input_tensor.type != {:u, 8} do
      raise ArgumentError, "Only support uint8 input type."
    end

    {h, w} =
      case input_tensor.shape do
        [_n, h, w, _c] ->
          {h, w}
        [_n, h, w] ->
          {h, w}
        shape ->
          raise RuntimeError, "not sure the input shape, got #{inspect(shape)}"
      end
    %StbImage{data: input_data} = StbImage.resize(input_image, h, w)

    [scale] = input_tensor.quantization_params.scale
    [zero_point] = input_tensor.quantization_params.zero_point
    mean = args[:mean]
    std = args[:std]

    if abs(scale * std - 1) < 0.00001 and abs(mean - zero_point) < 0.00001 do
      # Input data does not require preprocessing.
      Interpreter.input_tensor!(interpreter, input_tensor_number, input_data)
    end

    IO.puts("----INFERENCE TIME----")
    for _ <- 1..args[:count] do
      start_time = :os.system_time(:microsecond)
      Interpreter.invoke!(interpreter)
      end_time = :os.system_time(:microsecond)
      inference_time = (end_time - start_time) / 1000.0
      IO.puts("#{Float.round(inference_time, 1)}ms")
    end

    output_data = Interpreter.output_tensor!(interpreter, 0)
    output_tensor = Interpreter.tensor!(interpreter, output_tensor_number)
    scores = get_scores(output_data, output_tensor)
    sorted_indices = Nx.argsort(scores, direction: :desc)
    top_k = Nx.take(sorted_indices, Nx.iota({args[:top]}))
    scores = Nx.to_flat_list(Nx.take(scores, top_k))
    top_k = Nx.to_flat_list(top_k)

    IO.puts("-------RESULTS--------")
    if labels != nil do
      Enum.zip(top_k, scores)
      |>  Enum.each(fn {class_id, score} ->
        IO.puts("#{Enum.at(labels, class_id)}: #{Float.round(score, 5)}")
      end)
    else
      Enum.zip(top_k, scores)
      |>  Enum.each(fn {class_id, score} ->
        IO.puts("#{class_id}: #{Float.round(score, 5)}")
      end)
    end
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

  defp get_scores(output_data, %TFTensor{type: dtype={:u, _}}=output_tensor) do
    scale = Nx.tensor(output_tensor.quantization_params.scale)
    zero_point = Nx.tensor(output_tensor.quantization_params.zero_point)
    Nx.from_binary(output_data, dtype)
    |> Nx.as_type({:s, 64})
    |> Nx.subtract(zero_point)
    |> Nx.multiply(scale)
  end

  defp get_scores(output_data, %TFTensor{type: dtype={:s, _}}=output_tensor) do
    [scale] = output_tensor.quantization_params.scale
    [zero_point] = output_tensor.quantization_params.zero_point
    Nx.from_binary(output_data, dtype)
    |> Nx.as_type({:s, 64})
    |> Nx.subtract(zero_point)
    |> Nx.multiply(scale)
  end

  defp get_scores(output_data, %TFTensor{type: dtype}) do
    Nx.from_binary(output_data, dtype)
  end
end

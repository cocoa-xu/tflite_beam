defmodule TFLiteElixir.ImageClassification do
  @moduledoc """
  Experimental image classification module.
  """

  alias TFLiteElixir.Interpreter
  alias TFLiteElixir.InterpreterBuilder
  alias TFLiteElixir.TFLiteTensor
  alias TFLiteElixir.FlatBufferModel

  use GenServer

  @spec start(any, any) :: :ignore | {:error, any} | {:ok, pid}
  def start(model, opts \\ []) do
    GenServer.start(__MODULE__, {model, opts})
  end

  @spec predict(pid(), binary() | %StbImage{} | %Nx.Tensor{}, Keyword.t()) :: term()
  def predict(pid, input_path, opts \\ [])

  def predict(pid, input_path, opts) when is_binary(input_path) and is_list(opts) do
    GenServer.call(pid, {:predict, {:image_path, input_path}, opts})
  end

  def predict(pid, stb_image, opts) when is_struct(stb_image, StbImage) and is_list(opts) do
    GenServer.call(pid, {:predict, {:stb_image, stb_image}, opts})
  end

  def predict(pid, image_data, opts) when is_struct(image_data, Nx.Tensor) and is_list(opts) do
    GenServer.call(pid, {:predict, {:nx_tensor, image_data}, opts})
  end

  @spec set_label(pid, String.t() | [String.t()]) :: :ok
  def set_label(pid, label_file) when is_binary(label_file) do
    GenServer.call(pid, {:set_label, label_file})
  end

  def set_label(pid, labels) when is_list(labels) do
    GenServer.call(pid, {:set_label, labels})
  end

  @spec set_label_from_associated_file(pid(), String.t()) :: :ok | {:error, String.t()}
  def set_label_from_associated_file(pid, associated_filename) when is_binary(associated_filename) do
    GenServer.call(pid, {:set_label, :associated_file, associated_filename})
  end

  @impl true
  def init({model_path, opts}) do
    default_values = [
      top_k: 1,
      threshold: 0.0,
      mean: 128.0,
      std: 128.0,
      jobs: System.schedulers_online(),
      use_tpu: false,
      tpu: ""
    ]

    args =
      Keyword.merge(opts, default_values, fn _k, user, default ->
        if user == nil do
          default
        else
          user
        end
      end)

    model = load_model(model_path)

    tpu_context =
      if args[:use_tpu] do
        TFLiteElixir.Coral.get_edge_tpu_context!(device: args[:tpu])
      else
        nil
      end

    interpreter = make_interpreter(model, args[:jobs], args[:use_tpu], tpu_context)
    :ok = Interpreter.allocate_tensors(interpreter)

    {:ok, %{
      model_path: model_path,
      interpreter: interpreter,
      opts: args,
      labels: nil
    }}
  end

  @impl true
  def handle_call({:predict, {input_type, input_data}, pred_opts}, _from, state=%{interpreter: interpreter, opts: opts, labels: labels}) do
    [input_tensor_number | _] = Interpreter.inputs!(interpreter)
    [output_tensor_number | _] = Interpreter.outputs!(interpreter)
    %TFLiteTensor{} = input_tensor = Interpreter.tensor(interpreter, input_tensor_number)

    if input_tensor.type != {:u, 8} do
      raise ArgumentError, "Only support uint8 input type."
    end

    {h, w} =
      case input_tensor.shape do
        {_n, h, w, _c} ->
          {h, w}

        {_n, h, w} ->
          {h, w}

        shape ->
          raise RuntimeError, "not sure the input shape, got #{inspect(shape)}"
      end

    opts = Keyword.merge(opts, pred_opts, fn _k, _, p -> p end)

    input_image =
      case input_type do
        :image_path ->
          load_input(input_data)
        :stb_image ->
          input_data
        :nx_tensor ->
          StbImage.from_nx(input_data)
      end

    input_image = StbImage.resize(input_image, h, w)

    [scale] = input_tensor.quantization_params.scale
    [zero_point] = input_tensor.quantization_params.zero_point

    mean = opts[:mean]
    std = opts[:std]

    if abs(scale * std - 1) < 0.00001 and abs(mean - zero_point) < 0.00001 do
      # Input data does not require preprocessing.
      %StbImage{data: input_data} = input_image
      input_data
    else
      # Input data requires preprocessing
      StbImage.to_nx(input_image)
      |> Nx.subtract(mean)
      |> Nx.divide(std * scale)
      |> Nx.add(zero_point)
      |> Nx.clip(0, 255)
      |> Nx.as_type(:u8)
      |> Nx.to_binary()
    end
    |> then(&TFLiteTensor.set_data(input_tensor, &1))

    Interpreter.invoke!(interpreter)

    output_data = Interpreter.output_tensor!(interpreter, 0)
    %TFLiteTensor{} = output_tensor = Interpreter.tensor(interpreter, output_tensor_number)
    scores = get_scores(output_data, output_tensor)
    sorted_indices = Nx.argsort(scores, direction: :desc)
    top_k = Nx.take(sorted_indices, Nx.iota({opts[:top_k]}))
    scores = Nx.to_flat_list(Nx.take(scores, top_k))
    top_k = Nx.to_flat_list(top_k)

    results =
      if labels != nil do
        Enum.zip(top_k, scores)
        |> Enum.map(fn {class_id, score} ->
          %{
            class_id: class_id,
            score: score,
            label: Enum.at(labels, class_id)
          }
        end)
      else
        Enum.zip(top_k, scores)
        |> Enum.map(fn {class_id, score} ->
          %{
            class_id: class_id,
            score: score
          }
        end)
      end

    if opts[:top_k] == 1 do
      {:reply, Enum.at(results, 0), state}
    else
      {:reply, results, state}
    end
  end

  @impl true
  def handle_call({:set_label, label_file}, _from, state) when is_binary(label_file) do
    {:reply, :ok, %{state | labels: load_labels(label_file)}}
  end

  @impl true
  def handle_call({:set_label, labels}, _from, state) when is_list(labels) do
    {:reply, :ok, %{state | labels: labels}}
  end

  @impl true
  def handle_call({:set_label, :associated_file, associated_filename}, _from, state=%{model_path: model_path}) when is_binary(associated_filename) do
    case TFLiteElixir.FlatBufferModel.get_associated_file(File.read!(model_path), associated_filename) do
      content when is_binary(content) ->
        labels = String.split(content, "\n")
        {:reply, :ok, %{state | labels: labels}}
      error ->
        {:reply, error, state}
    end
  end

  defp load_model(nil) do
    raise ArgumentError, "empty value for argument 'model'"
  end

  defp load_model(model_path) do
    FlatBufferModel.build_from_buffer(File.read!(model_path))
  end

  defp load_input(nil) do
    raise ArgumentError, "empty value for input"
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

  defp make_interpreter(model, num_jobs, false, _tpu_context) do
    resolver = TFLiteElixir.Ops.Builtin.BuiltinResolver.new!()
    builder = InterpreterBuilder.new!(model, resolver)
    interpreter = Interpreter.new!()
    InterpreterBuilder.set_num_threads!(builder, num_jobs)
    :ok = InterpreterBuilder.build!(builder, interpreter)
    Interpreter.set_num_threads!(interpreter, num_jobs)
    interpreter
  end

  defp make_interpreter(model, _num_jobs, true, tpu_context) do
    TFLiteElixir.Coral.make_edge_tpu_interpreter!(model, tpu_context)
  end

  defp get_scores(output_data, %TFLiteTensor{type: dtype = {:u, _}} = output_tensor) do
    scale = Nx.tensor(output_tensor.quantization_params.scale)
    zero_point = Nx.tensor(output_tensor.quantization_params.zero_point)

    Nx.from_binary(output_data, dtype)
    |> Nx.as_type({:s, 64})
    |> Nx.subtract(zero_point)
    |> Nx.multiply(scale)
  end

  defp get_scores(output_data, %TFLiteTensor{type: dtype = {:s, _}} = output_tensor) do
    [scale] = output_tensor.quantization_params.scale
    [zero_point] = output_tensor.quantization_params.zero_point

    Nx.from_binary(output_data, dtype)
    |> Nx.as_type({:s, 64})
    |> Nx.subtract(zero_point)
    |> Nx.multiply(scale)
  end

  defp get_scores(output_data, %TFLiteTensor{type: dtype}) do
    Nx.from_binary(output_data, dtype)
  end
end

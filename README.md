# TFLite-Elixir [WIP]

There are mainly two reasons why I write this library.

1. `Port` has limited throughput.
2. It would be easier and more flexible to make changes in Elixir if we have all the basic building blocks (i.e., functions)
from TensorFlow Lite available in Elixir.

## Demo code
Model: [mobilenet_v2_1.0_224_inat_bird_quant.tflite](https://github.com/google-coral/edgetpu/blob/master/test_data/mobilenet_v2_1.0_224_inat_bird_quant.tflite)
Input image: [parrot.jpg](https://github.com/google-coral/edgetpu/blob/master/test_data/parrot.jpg)
Labels: [inat_bird_labels.txt](https://github.com/google-coral/edgetpu/blob/master/test_data/inat_bird_labels.txt)

```elixir
# load labels
labels = File.read!("inat_bird_labels.txt") |> String.split("\n")

# load tflite model
filename = "mobilenet_v2_1.0_224_inat_bird_quant.tflite"
{:ok, model} = TFLite.FlatBufferModel.buildFromFile(filename)
{:ok, resolver} = TFLite.Ops.Builtin.BuiltinResolver.new()
{:ok, builder} = TFLite.InterpreterBuilder.new(model, resolver)
{:ok, interpreter} = TFLite.Interpreter.new()
:ok = TFLite.InterpreterBuilder.build(builder, interpreter)
:ok = TFLite.Interpreter.allocateTensors(interpreter)

# verify loaded model, feel free to skip
# {:ok, [0]} = TFLite.Interpreter.inputs(interpreter)
# {:ok, [171]} = TFLite.Interpreter.outputs(interpreter)
# {:ok, "map/TensorArrayStack/TensorArrayGatherV3"} = TFLite.Interpreter.getInputName(interpreter, 0)
# {:ok, "prediction"} = TFLite.Interpreter.getOutputName(interpreter, 0)
# {:ok, input_tensor} = TFLite.Interpreter.tensor(interpreter, 0)
# {:ok, [1, 224, 224, 3]} = TFLite.TfLiteTensor.dims(input_tensor)
# {:u, 8} = TFLite.TfLiteTensor.type(input_tensor)
# {:ok, output_tensor} = TFLite.Interpreter.tensor(interpreter, 171)
# {:ok, [1, 965]} = TFLite.TfLiteTensor.dims(output_tensor)
# {:u, 8} = TFLite.TfLiteTensor.type(output_tensor)

# load image, resize it, covert to RGB and to binary 
{:ok, mat} = OpenCV.imread("parrot.jpg")
{:ok, mat} = OpenCV.resize(mat, [224, 224])
{:ok, mat} = OpenCV.cvtColor(mat, OpenCV.cv_COLOR_BGR2RGB)
{:ok, binary} = OpenCV.Mat.to_binary(mat)

# set input, run forwarding, get output
TFLite.Interpreter.input_tensor(interpreter, 0, binary)
TFLite.Interpreter.invoke(interpreter)
{:ok, output_data} = TFLite.Interpreter.output_tensor(interpreter, 0)

# get predicted label
output_data
|> Nx.from_binary({:u, 8})
|> Nx.argmax()
|> Nx.to_scalar()
|> then(&Enum.at(labels, &1))
```

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `tflite_elixir` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:tflite_elixir, "~> 0.1.0", github: "cocoa-xu/tflite-elixir"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at <https://hexdocs.pm/tflite_elixir>.


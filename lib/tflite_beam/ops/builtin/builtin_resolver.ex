defmodule TFLiteBEAM.Ops.Builtin.BuiltinResolver do
  @moduledoc """
  This built-in op resolver provides a list of TfLite delegates that could be
  applied by TfLite interpreter by default.
  """

  import TFLiteBEAM.Errorize

  @type nif_resource_ok :: {:ok, reference()}
  @type nif_error :: {:error, String.t()}

  @doc """
  New built-in op resolver.

  This resolver provides a list of TfLite delegates that could be
  applied by TfLite interpreter by default. Currently, it includes
  the following opeators.

  - ABS
  - HARD_SWISH
  - RELU
  - RELU_N1_TO_1
  - RELU_0_TO_1
  - RELU6
  - TANH
  - LOGISTIC
  - AVERAGE_POOL_2D
  - MAX_POOL_2D
  - L2_POOL_2D
  - CONV_2D
  - DEPTHWISE_CONV_2D
  - SVDF
  - RNN
  - BIDIRECTIONAL_SEQUENCE_RNN
  - UNIDIRECTIONAL_SEQUENCE_RNN
  - EMBEDDING_LOOKUP
  - EMBEDDING_LOOKUP_SPARSE
  - FULLY_CONNECTED
  - LSH_PROJECTION
  - HASHTABLE_LOOKUP
  - SOFTMAX
  - CONCATENATION
  - ADD
  - SPACE_TO_BATCH_ND
  - BATCH_TO_SPACE_ND
  - MUL
  - L2_NORMALIZATION
  - LOCAL_RESPONSE_NORMALIZATION
  - LSTM
  - BIDIRECTIONAL_SEQUENCE_LSTM
  - UNIDIRECTIONAL_SEQUENCE_LSTM
  - PAD
  - PADV2
  - RESHAPE
  - RESIZE_BILINEAR
  - RESIZE_NEAREST_NEIGHBOR
  - SKIP_GRAM
  - SPACE_TO_DEPTH
  - DEPTH_TO_SPACE
  - GATHER
  - TRANSPOSE
  - MEAN
  - DIV
  - SUB
  - SPLIT
  - SPLIT_V
  - SQUEEZE
  - STRIDED_SLICE
  - EXP
  - TOPK_V2
  - LOG
  - LOG_SOFTMAX
  - CAST
  - DEQUANTIZE
  - PRELU
  - MAXIMUM
  - MINIMUM
  - ARG_MAX
  - ARG_MIN
  - GREATER
  - GREATER_EQUAL
  - LESS
  - LESS_EQUAL
  - FLOOR
  - CEIL
  - ROUND
  - NEG
  - SELECT
  - SELECT_V2
  - SLICE
  - SIN
  - COS
  - TRANSPOSE_CONV
  - TILE
  - SUM
  - REDUCE_PROD
  - REDUCE_MAX
  - REDUCE_MIN
  - REDUCE_ANY
  - REDUCE_ALL
  - EXPAND_DIMS
  - SPARSE_TO_DENSE
  - EQUAL
  - NOT_EQUAL
  - SQRT
  - RSQRT
  - SHAPE
  - RANK
  - POW
  - FAKE_QUANT
  - PACK
  - ONE_HOT
  - LOGICAL_OR
  - LOGICAL_AND
  - LOGICAL_NOT
  - UNPACK
  - FLOOR_DIV
  - SQUARE
  - ZEROS_LIKE
  - FLOOR_MOD
  - RANGE
  - LEAKY_RELU
  - SQUARED_DIFFERENCE
  - FILL
  - MIRROR_PAD
  - UNIQUE
  - REVERSE_V2
  - ADD_N
  - GATHER_ND
  - WHERE
  - ELU
  - REVERSE_SEQUENCE
  - MATRIX_DIAG
  - QUANTIZE
  - MATRIX_SET_DIAG
  - IF
  - WHILE
  - NON_MAX_SUPPRESSION_V4
  - NON_MAX_SUPPRESSION_V5
  - SCATTER_ND
  - DENSIFY
  - SEGMENT_SUM
  - BATCH_MATMUL
  - CUMSUM
  - BROADCAST_TO
  - CALL_ONCE
  - RFFT2D
  - CONV_3D
  - IMAG
  - REAL
  - COMPLEX_ABS
  - BROADCAST_ARGS
  - HASHTABLE
  - HASHTABLE_FIND
  - HASHTABLE_IMPORT
  - HASHTABLE_SIZE
  - CONV_3D_TRANSPOSE
  - VAR_HANDLE
  - READ_VARIABLE
  - ASSIGN_VARIABLE
  - MULTINOMIAL
  - RANDOM_STANDARD_NORMAL
  - BUCKETIZE
  - RANDOM_UNIFORM
  - GELU
  - DYNAMIC_UPDATE_SLICE
  - UNSORTED_SEGMENT_PROD
  - UNSORTED_SEGMENT_MAX
  - UNSORTED_SEGMENT_MIN
  - UNSORTED_SEGMENT_SUM
  - ATAN2
  - SIGN
  - NumericVerify
  - Mfcc
  - AudioSpectrogram
  - TFLite_Detection_PostProcess
  """
  @spec new() :: nif_resource_ok() | nif_error()
  def new() do
    :tflite_beam_nif.ops_builtin_builtin_resolver_new()
  end

  deferror(new())
end

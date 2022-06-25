ExUnit.configure(
  exclude: [
    # exclude all tests that require a physical TPU by default
    require_tpu: true
  ]
)

ExUnit.start()

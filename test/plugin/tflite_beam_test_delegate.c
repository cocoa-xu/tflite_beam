// A delegate plugin that exists so the external-delegate path can be tested
// without hardware. It links nothing: TfLiteDelegate is a plain struct, and a
// Prepare that claims no nodes needs no TfLite symbols at all.
//
// Built only when TFLITE_BEAM_BUILD_TEST_PLUGIN is on, which no released build
// ever sets.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "tensorflow/lite/c/common.h"

static TfLiteStatus prepare_claim_none(TfLiteContext* context, TfLiteDelegate* delegate) {
  return kTfLiteOk;
}

static TfLiteStatus prepare_fail(TfLiteContext* context, TfLiteDelegate* delegate) {
  return kTfLiteError;
}

TfLiteDelegate* tflite_plugin_create_delegate(const char* const* options_keys,
                                              const char* const* options_values,
                                              size_t num_options,
                                              void (*report_error)(const char*)) {
  const char* mode = "claim_none";
  const char* log_path = NULL;

  for (size_t i = 0; i < num_options; i++) {
    if (strcmp(options_keys[i], "mode") == 0) {
      mode = options_values[i];
    } else if (strcmp(options_keys[i], "log_path") == 0) {
      log_path = options_values[i];
    }
  }

  // so a test can read back what actually crossed the boundary
  if (log_path != NULL) {
    FILE* log = fopen(log_path, "a");
    if (log != NULL) {
      for (size_t i = 0; i < num_options; i++) {
        fprintf(log, "%s=%s\n", options_keys[i], options_values[i]);
      }
      fclose(log);
    }
  }

  if (strcmp(mode, "fail_create") == 0) {
    if (report_error != NULL) {
      report_error("the test delegate was asked to fail at create");
    }
    return NULL;
  }

  if (strcmp(mode, "claim_none") != 0 && strcmp(mode, "fail_prepare") != 0) {
    if (report_error != NULL) {
      report_error("unknown mode: expected claim_none, fail_prepare or fail_create");
    }
    return NULL;
  }

  TfLiteDelegate* delegate = (TfLiteDelegate*)calloc(1, sizeof(TfLiteDelegate));
  if (delegate == NULL) {
    return NULL;
  }

  delegate->data_ = NULL;
  delegate->Prepare = strcmp(mode, "fail_prepare") == 0 ? prepare_fail : prepare_claim_none;
  delegate->CopyFromBufferHandle = NULL;
  delegate->CopyToBufferHandle = NULL;
  delegate->FreeBufferHandle = NULL;
  delegate->flags = kTfLiteDelegateFlagsNone;
  return delegate;
}

void tflite_plugin_destroy_delegate(TfLiteDelegate* delegate) {
  free(delegate);
}

#include "helper.h"
#include <erl_nif.h>
#include "tflite/c/c_api.h"
#include "tflite/c/common.h"
#include "nif_utils.hpp"

bool tensor_type_to_erl_term(const TfLiteType in_type, ErlNifEnv *env, ERL_NIF_TERM &out_term) {
    bool ok = true;
    switch (in_type) {
        case kTfLiteNoType:
            out_term = erlang::nif::atom(env, "no_type");
            break;
        case kTfLiteFloat32:
            out_term = enif_make_tuple2(env,
                                        erlang::nif::atom(env, "f"),
                                        enif_make_int(env, 32));
            break;
        case kTfLiteInt32:
            out_term = enif_make_tuple2(env,
                                        erlang::nif::atom(env, "s"),
                                        enif_make_int(env, 32));
            break;
        case kTfLiteUInt8:
            out_term = enif_make_tuple2(env,
                                        erlang::nif::atom(env, "u"),
                                        enif_make_int(env, 8));
            break;
        case kTfLiteInt64:
            out_term = enif_make_tuple2(env,
                                        erlang::nif::atom(env, "s"),
                                        enif_make_int(env, 64));
            break;
        case kTfLiteString:
            out_term = erlang::nif::atom(env, "string");
            break;
        case kTfLiteBool:
            out_term = erlang::nif::atom(env, "bool");
            break;
        case kTfLiteInt16:
            out_term = enif_make_tuple2(env,
                                        erlang::nif::atom(env, "s"),
                                        enif_make_int(env, 16));
            break;
        case kTfLiteComplex64:
            out_term = enif_make_tuple2(env,
                                        erlang::nif::atom(env, "c"),
                                        enif_make_int(env, 64));
            break;
        case kTfLiteInt8:
            out_term = enif_make_tuple2(env,
                                        erlang::nif::atom(env, "s"),
                                        enif_make_int(env, 8));
            break;
        case kTfLiteFloat16:
            out_term = enif_make_tuple2(env,
                                        erlang::nif::atom(env, "f"),
                                        enif_make_int(env, 16));
            break;
        case kTfLiteFloat64:
            out_term = enif_make_tuple2(env,
                                        erlang::nif::atom(env, "f"),
                                        enif_make_int(env, 64));
            break;
        case kTfLiteComplex128:
            out_term = enif_make_tuple2(env,
                                        erlang::nif::atom(env, "c"),
                                        enif_make_int(env, 128));
            break;
        case kTfLiteUInt64:
            out_term = enif_make_tuple2(env,
                                        erlang::nif::atom(env, "u"),
                                        enif_make_int(env, 64));
            break;
        case kTfLiteResource:
            out_term = erlang::nif::atom(env, "resource");
            break;
        case kTfLiteVariant:
            out_term = erlang::nif::atom(env, "variant");
            break;
        case kTfLiteUInt32:
            out_term = enif_make_tuple2(env,
                                        erlang::nif::atom(env, "u"),
                                        enif_make_int(env, 32));
            break;
        case kTfLiteUInt16:
            out_term = enif_make_tuple2(env,
                                        erlang::nif::atom(env, "u"),
                                        enif_make_int(env, 16));
            break;
        case kTfLiteBFloat16:
            // Sixteen bits holding the top half of a float32. Nx spells this
            // {bf, 16} and stores it the same way, so the bytes carry across
            // unchanged. Erlang encodes one by truncating rather than rounding
            // to nearest, so a value written from Erlang can sit one ulp from
            // what a C encoder would have produced. A rounding difference, not
            // a misread.
            out_term = enif_make_tuple2(env,
                                        erlang::nif::atom(env, "bf"),
                                        enif_make_int(env, 16));
            break;
        // One byte each, and both are 8 bit floats, but they split that byte
        // differently: E5M2 has five exponent bits and infinities, E4M3FN has
        // four and none, which is what the FN stands for. The width alone does
        // not say which, and reading one as the other does not fail, it answers
        // a different number: 0x78 is 32768 under E5M2 and 256 under E4M3FN.
        //
        // The names here are Nx's, as every other type in this function already
        // is. Nx means E5M2 by the plain {f, 8}, which its infinity_binary of
        // 0x7C settles because E4M3FN has no infinity to encode, and it spells
        // the other {f8_e4m3fn, 8}. Following that spares everything downstream
        // a translation table, and a translation table is somewhere for the two
        // to get swapped.
        case kTfLiteFloat8E5M2:
            out_term = enif_make_tuple2(env,
                                        erlang::nif::atom(env, "f"),
                                        enif_make_int(env, 8));
            break;
        case kTfLiteFloat8E4M3FN:
            out_term = enif_make_tuple2(env,
                                        erlang::nif::atom(env, "f8_e4m3fn"),
                                        enif_make_int(env, 8));
            break;
        // Packed: two values to a byte at four bits, four at two bits, and
        // tensor->bytes counts the packed size. Handing that out as though it
        // were one value per byte fuses pairs of them together, so these are
        // refused until something unpacks them.
        case kTfLiteInt4:
        case kTfLiteUInt4:
        case kTfLiteInt2:
            ok = false;
            break;
        // No default, deliberately. Every member of TfLiteType is named above,
        // so an addition upstream is a warning here rather than a value that
        // quietly falls through. The catch-all that used to be here is why five
        // additions went unnoticed between March 2023 and now.
    }
    return ok;
}

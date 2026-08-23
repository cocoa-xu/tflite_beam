#ifndef TFLITE_BEAM_XNNPACK_LIMITS_H
#define TFLITE_BEAM_XNNPACK_LIMITS_H

#pragma once

#if defined(TFLITE_BEAM_XNNPACK_ENABLED) && defined(TFLITE_BEAM_MAX_DELEGATED_RANK)
// XNNPACK describes a tensor with a fixed-width array of this many dimensions.
// The count is bounded once, when the delegate decides whether to take a graph,
// and nothing rechecks it on the reshape a resize reaches, so growing a
// delegated tensor across this boundary writes past the end of that array with
// the dimensions the caller supplied. CMake lifts the number straight out of
// XNNPACK's header, so it cannot drift from what the delegate was built with.
static constexpr int kMaxDelegatedRank = TFLITE_BEAM_MAX_DELEGATED_RANK;
#else
// No XNNPACK in this build, so nothing imposes the limit and a resize of any
// rank is the interpreter's business alone. Two of the precompiled targets,
// armv6 and armv7l, are built this way.
static constexpr int kMaxDelegatedRank = -1;
#endif

#endif  // TFLITE_BEAM_XNNPACK_LIMITS_H

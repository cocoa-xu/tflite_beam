#ifndef TFLITE_BEAM_TSAN_ANNOTATIONS_H
#define TFLITE_BEAM_TSAN_ANNOTATIONS_H

// A resource is filled in on one thread and torn down on whichever thread drops
// the last reference. That ordering is real and the emulator guarantees it, but
// the guarantee lives in erts, which is not built with ThreadSanitizer, so the
// edge is invisible and every resource in the library reports as a race.
//
// The fix is to state the edge rather than to suppress the reports. Suppressing
// them would also hide a teardown genuinely overlapping a use, which is the one
// thing worth having the tool for.
//
// Compiled out entirely unless the build is a ThreadSanitizer build.

#if defined(__has_feature)
#  if __has_feature(thread_sanitizer)
#    define TFLITE_BEAM_TSAN_ENABLED 1
#  endif
#endif
#if !defined(TFLITE_BEAM_TSAN_ENABLED) && defined(__SANITIZE_THREAD__)
#  define TFLITE_BEAM_TSAN_ENABLED 1
#endif

#ifdef TFLITE_BEAM_TSAN_ENABLED
extern "C" void __tsan_acquire(void * addr);
extern "C" void __tsan_release(void * addr);
// publishing: everything written before this is visible to whoever acquires
#  define TFLITE_BEAM_TSAN_PUBLISH(p) __tsan_release((void *)(p))
// taking delivery: orders this thread after the publisher
#  define TFLITE_BEAM_TSAN_TAKE(p)    __tsan_acquire((void *)(p))
#else
#  define TFLITE_BEAM_TSAN_PUBLISH(p) ((void)0)
#  define TFLITE_BEAM_TSAN_TAKE(p)    ((void)0)
#endif

#endif  // TFLITE_BEAM_TSAN_ANNOTATIONS_H

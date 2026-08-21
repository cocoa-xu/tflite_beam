#ifndef TFLITE_BEAM_NIF_GUARD_HPP
#define TFLITE_BEAM_NIF_GUARD_HPP

#include <erl_nif.h>

#include <exception>
#include <new>

#include "nif_utils.hpp"

// A NIF runs inside the emulator, so an exception that escapes one reaches
// std::terminate and takes the whole VM down with it. Erlang's own words for
// this are that "a crash in a NIF brings the emulator down too".
//
// The only exception this library can produce is std::bad_alloc: the TFLite and
// flatbuffers headers it links against have no throw on any path it calls, and
// nothing here parses numbers or indexes containers in a way that could raise
// something else. So this turns a failed allocation into the same
// {error, Reason} the hand-written enif_alloc checks already return, rather
// than into a dead node.
//
// Wrap only the entry points that allocate. Guarding one that cannot throw
// costs nothing at runtime but says something untrue about the function.
namespace erlang {
namespace nif {

template <ERL_NIF_TERM (*Fn)(ErlNifEnv *, int, const ERL_NIF_TERM[])>
ERL_NIF_TERM guarded(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    try {
        return Fn(env, argc, argv);
    } catch (const std::bad_alloc &) {
        return erlang::nif::error(env, "out of memory");
    } catch (const std::exception &e) {
        // Keep what the exception said. A message we cannot anticipate is worth
        // more to whoever has to diagnose it than a generic one.
        return erlang::nif::error(env, e.what());
    } catch (...) {
        return erlang::nif::error(env, "unknown C++ exception escaped the NIF");
    }
}

}  // namespace nif
}  // namespace erlang

#endif  // TFLITE_BEAM_NIF_GUARD_HPP

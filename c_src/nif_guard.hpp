#ifndef TFLITE_BEAM_NIF_GUARD_HPP
#define TFLITE_BEAM_NIF_GUARD_HPP

#include <erl_nif.h>

#include <exception>
#include <new>

#include "nif_utils.hpp"

// A NIF runs inside the emulator, so an exception that escapes one reaches
// std::terminate and takes the whole VM down with it. Erlang's own words for
// this are that "a crash in a NIF brings the emulator down too". This turns a
// failed allocation into the same {error, Reason} the hand-written enif_alloc
// checks already return, rather than into a dead node.
//
// Every exported entry point gets one. An earlier version wrapped only the ones
// that were seen to allocate, which meant keeping a claim about the other
// sixty-four true forever: not just their own bodies but every helper, every
// std::string built from an argument, every vector grown by a loop, and every
// upstream header they call into. The claim was already false when it was
// written. A try block a throw never reaches costs nothing to run, and the cost
// of getting the list wrong is the whole node.
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

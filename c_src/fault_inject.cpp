#include <erl_nif.h>

#include <string>

#include "fault_inject.hpp"
#include "nif_utils.hpp"

namespace erlang {
namespace nif {

std::atomic<int> armed_fault_point{kFaultNone};

}  // namespace nif
}  // namespace erlang

// Arms the next call through one of the points named in fault_inject.hpp to
// fail as an allocation failure would. It exists so the test suite can reach the
// windows between taking a reference and recording it, which are not otherwise
// reachable: asking a machine to run out of memory at one exact line is not
// something a test can do.
//
// Arming is one shot. `none` disarms.
ERL_NIF_TERM nif_arm_fault(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    std::string name;
    if (!erlang::nif::get_atom(env, argv[0], name)) {
        return erlang::nif::error(env, "expecting the fault point to be an atom");
    }

    int point = erlang::nif::fault_point_from_name(name.c_str());
    if (point < 0) {
        return erlang::nif::error(env, "no such fault point");
    }

    erlang::nif::armed_fault_point.store(point, std::memory_order_relaxed);
    return erlang::nif::atom(env, "ok");
}

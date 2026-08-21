#ifndef TFLITE_BEAM_FAULT_INJECT_HPP
#define TFLITE_BEAM_FAULT_INJECT_HPP

#include <atomic>
#include <cstring>
#include <new>

// Reference bookkeeping around a resource is written across two statements: the
// retain on one line, the container that records it on the next. An allocation
// failure in between leaves a reference nothing will ever give back, and a
// failure while a lock is held leaves the lock held. Those windows are real and
// none of them can be reached by asking the machine to run out of memory, so the
// test suite arms one of these points and the next call through it throws
// instead.
//
// Armed by tflite_beam_nif:arm_fault/1, which exists for the test suite and is
// documented as such. Disarmed, a point costs one relaxed load of an int that is
// already in cache, on paths that were about to allocate anyway.
namespace erlang {
namespace nif {

enum FaultPoint {
    kFaultNone = 0,
    kFaultRunnerRegistry,
    kFaultAddDelegateRegistry,
    kFaultDelegateTransfer,
    kFaultInterpreterContainers,
    kFaultBuilderContainers,
};

extern std::atomic<int> armed_fault_point;

// Names as the test suite spells them. Index by FaultPoint.
inline const char * const * fault_point_names() {
    static const char * const names[] = {
        "none",
        "runner_registry",
        "add_delegate_registry",
        "delegate_transfer",
        "interpreter_containers",
        "builder_containers",
    };
    return names;
}

inline int fault_point_from_name(const char * name) {
    const char * const * names = fault_point_names();
    for (int i = kFaultNone; i <= kFaultBuilderContainers; i++) {
        if (std::strcmp(names[i], name) == 0) return i;
    }
    return -1;
}

// One shot: the point disarms itself as it fires, so a test arms it, makes the
// one call it wants to fail, and leaves nothing behind for the next case.
inline void fault_point(FaultPoint point) {
    if (armed_fault_point.load(std::memory_order_relaxed) != point) return;
    int expected = point;
    if (armed_fault_point.compare_exchange_strong(expected, kFaultNone, std::memory_order_relaxed)) {
        throw std::bad_alloc();
    }
}

}  // namespace nif
}  // namespace erlang

#endif  // TFLITE_BEAM_FAULT_INJECT_HPP

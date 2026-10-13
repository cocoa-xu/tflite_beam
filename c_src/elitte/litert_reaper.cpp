#ifdef TFLITE_BEAM_LITERT_API_ENABLED

#include "litert_reaper.h"
#include "../fault_inject.hpp"

#include <atomic>
#include <deque>

// Destroying a compiled model is not cheap and it is not bounded by anything a
// caller controls: measured on an M4 Max, a 13MB mobilenet takes about 0.5ms
// and a 49MB model about 16ms, and it grows faster than the model does. A
// resource destructor runs wherever the last reference happened to be dropped,
// which is normally an ordinary scheduler, and 16ms there stalls every process
// that scheduler is running. So the destructor gives the work away and returns.
//
// One thread is enough. Teardown is not something a caller waits on, so the
// only thing that matters is that it happens, and doing it in submission order
// on a single thread keeps that easy to reason about.

namespace {

std::atomic<ErlNifMutex *> queue_lock{nullptr};
ErlNifCond * queue_ready = nullptr;
ErlNifTid reaper_tid;
bool reaper_running = false;
bool accepting = false;
std::deque<LiteRtTeardown> * queue = nullptr;

void * reaper_main(void *) {
    ErlNifMutex * lock = queue_lock.load(std::memory_order_acquire);
    for (;;) {
        LiteRtTeardown job;
        enif_mutex_lock(lock);
        while (queue->empty() && accepting) {
            enif_cond_wait(queue_ready, lock);
        }
        if (queue->empty() && !accepting) {
            enif_mutex_unlock(lock);
            return nullptr;
        }
        job = queue->front();
        queue->pop_front();
        enif_mutex_unlock(lock);

        litert_teardown_now(job);
    }
}

}  // namespace

void litert_teardown_now(const LiteRtTeardown & job) {
    // Buffers first: they name the compiled model's tensors, so they cannot
    // outlive it. The memory behind them belongs to LiteRT, which allocated it
    // from the tensor's requirements and may not have put it in host memory at
    // all, so destroying the buffer is the whole of releasing it.
    auto destroy = [](std::vector<LiteRtTensorBuffer> * bufs) {
        if (!bufs) return;
        for (auto b : *bufs) if (b) LiteRtDestroyTensorBuffer(b);
        delete bufs;
    };
    destroy(job.inputs);
    destroy(job.outputs);
    delete job.input_sizes;
    delete job.output_sizes;

    if (job.compiled) LiteRtDestroyCompiledModel(job.compiled);
    if (job.options)  LiteRtDestroyOptions(job.options);
    if (job.model)    LiteRtDestroyModel(job.model);

    // safe from any thread, and it is what lets the environment outlive the
    // model that was pointing at it
    if (job.environment) enif_release_resource(job.environment);

    if (job.lock) enif_mutex_destroy(job.lock);
}

void litert_reaper_submit(const LiteRtTeardown & job) {
    ErlNifMutex * lock = queue_lock.load(std::memory_order_acquire);
    if (lock == nullptr) {
        litert_teardown_now(job);
        return;
    }
    // Whether there is anyone to hand it to has to be read under the lock, or a
    // stop running alongside this decides one way and the queue goes the other.
    enif_mutex_lock(lock);
    if (!accepting) {
        enif_mutex_unlock(lock);
        litert_teardown_now(job);
        return;
    }
    try {
        erlang::nif::fault_point(erlang::nif::kFaultLiteRtReaperSubmit);
        queue->push_back(job);
    } catch (const std::bad_alloc &) {
        enif_mutex_unlock(lock);
        litert_teardown_now(job);
        return;
    }
    enif_cond_signal(queue_ready);
    enif_mutex_unlock(lock);
}

int litert_reaper_start() {
    if (reaper_running) return 0;

    ErlNifMutex * lock = queue_lock.load(std::memory_order_acquire);
    if (lock == nullptr) {
        lock = enif_mutex_create(const_cast<char *>("tflite_beam_litert_reaper"));
        ErlNifCond * ready = enif_cond_create(const_cast<char *>("tflite_beam_litert_reaper"));
        std::deque<LiteRtTeardown> * jobs = nullptr;
        try {
            jobs = new std::deque<LiteRtTeardown>();
        } catch (const std::bad_alloc &) {
        }
        if (lock == nullptr || ready == nullptr || jobs == nullptr) {
            delete jobs;
            if (ready) enif_cond_destroy(ready);
            if (lock) enif_mutex_destroy(lock);
            return -1;
        }
        queue_ready = ready;
        queue = jobs;
        queue_lock.store(lock, std::memory_order_release);
    }

    enif_mutex_lock(lock);
    accepting = true;
    enif_mutex_unlock(lock);
    if (enif_thread_create(const_cast<char *>("tflite_beam_litert_reaper"),
                           &reaper_tid, reaper_main, nullptr, nullptr) != 0) {
        // submit() falls back to the calling thread, so this is slow, not broken
        litert_reaper_stop();
        return -1;
    }
    reaper_running = true;
    return 0;
}

void litert_reaper_stop() {
    ErlNifMutex * lock = queue_lock.load(std::memory_order_acquire);
    if (lock == nullptr) return;

    enif_mutex_lock(lock);
    accepting = false;
    if (queue_ready) enif_cond_signal(queue_ready);
    enif_mutex_unlock(lock);

    if (reaper_running) {
        enif_thread_join(reaper_tid, nullptr);
        reaper_running = false;
    }

    // A destructor can have run between the reaper leaving and this line, and
    // anything still queued has to be finished before this library goes away.
    for (;;) {
        LiteRtTeardown job;
        enif_mutex_lock(lock);
        if (queue->empty()) { enif_mutex_unlock(lock); break; }
        job = queue->front();
        queue->pop_front();
        enif_mutex_unlock(lock);
        litert_teardown_now(job);
    }

    // The queue, the mutex and the condition are deliberately not freed. A
    // destructor arriving after this point has to find something valid to take
    // the lock on and be told to tear down where it stands; a freed mutex would
    // be the alternative, and a few bytes at unload is the cheaper mistake.
}

#endif  // TFLITE_BEAM_LITERT_API_ENABLED

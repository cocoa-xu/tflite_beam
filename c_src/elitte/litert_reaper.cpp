#ifdef TFLITE_BEAM_LITERT_API_ENABLED

#include "litert_reaper.h"

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

ErlNifMutex * queue_lock = nullptr;
ErlNifCond * queue_ready = nullptr;
ErlNifTid reaper_tid;
bool reaper_running = false;
bool accepting = false;
std::deque<LiteRtTeardown> * queue = nullptr;

void * reaper_main(void *) {
    for (;;) {
        LiteRtTeardown job;
        enif_mutex_lock(queue_lock);
        while (queue->empty() && accepting) {
            enif_cond_wait(queue_ready, queue_lock);
        }
        if (queue->empty() && !accepting) {
            enif_mutex_unlock(queue_lock);
            return nullptr;
        }
        job = queue->front();
        queue->pop_front();
        enif_mutex_unlock(queue_lock);

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
    if (queue_lock == nullptr) {
        litert_teardown_now(job);
        return;
    }
    // Whether there is anyone to hand it to has to be read under the lock, or a
    // stop running alongside this decides one way and the queue goes the other.
    enif_mutex_lock(queue_lock);
    if (!accepting) {
        enif_mutex_unlock(queue_lock);
        litert_teardown_now(job);
        return;
    }
    queue->push_back(job);
    enif_cond_signal(queue_ready);
    enif_mutex_unlock(queue_lock);
}

int litert_reaper_start() {
    if (reaper_running) return 0;

    queue_lock = enif_mutex_create(const_cast<char *>("tflite_beam_litert_reaper"));
    queue_ready = enif_cond_create(const_cast<char *>("tflite_beam_litert_reaper"));
    queue = new (std::nothrow) std::deque<LiteRtTeardown>();
    if (queue_lock == nullptr || queue_ready == nullptr || queue == nullptr) {
        litert_reaper_stop();
        return -1;
    }


    accepting = true;
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
    if (queue_lock == nullptr) return;

    enif_mutex_lock(queue_lock);
    accepting = false;
    if (queue_ready) enif_cond_signal(queue_ready);
    enif_mutex_unlock(queue_lock);

    if (reaper_running) {
        enif_thread_join(reaper_tid, nullptr);
        reaper_running = false;
    }

    // A destructor can have run between the reaper leaving and this line, and
    // anything still queued has to be finished before this library goes away.
    for (;;) {
        LiteRtTeardown job;
        enif_mutex_lock(queue_lock);
        if (queue->empty()) { enif_mutex_unlock(queue_lock); break; }
        job = queue->front();
        queue->pop_front();
        enif_mutex_unlock(queue_lock);
        litert_teardown_now(job);
    }

    // The queue, the mutex and the condition are deliberately not freed. A
    // destructor arriving after this point has to find something valid to take
    // the lock on and be told to tear down where it stands; a freed mutex would
    // be the alternative, and a few bytes at unload is the cheaper mistake.
}

#endif  // TFLITE_BEAM_LITERT_API_ENABLED

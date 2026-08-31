# Changelog

## v1.0.0-rc4 (2026-08-31)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v1.0.0-rc4) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v1.0.0-rc4)

One fix, found by writing the test that should have come with the feature.

**`with/2` on an isolated model could not run most callbacks.** It sends the
function to the node that owns the model and applies it there, which needs the
module that function belongs to to exist on that node. A capture of a compiled
function, `fun mod:f/1` or `&Mod.f/1`, always did. A function written inline in
a test case or a script did not: the compiler keeps such a module in memory,
there is no file for the peer to load, and the call came back as a bare `undef`
naming a fun with no context. The module is now sent over when there is object
code to send, which covers every function that lives in a compiled module, and
when there is none the answer says which module and why rather than passing the
`undef` on.

It was written on the reasoning that the peer starts with this node's code path,
so any function would resolve there. That is true of named modules and of
nothing else, and no test had been written that would notice.

## v1.0.0-rc3 (2026-08-30)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v1.0.0-rc3) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v1.0.0-rc3)

This release adds LiteRT's compiled model, which is a second way into the same
runtime: accelerators asked for by name and answered honestly, and a per-operator
profiler that says where the time went. Nothing about the existing interpreter
API changed.

**Three layers, because sharing one model is the thing that goes wrong.**
`tflite_beam_litert_compiled_model` is the direct API and refuses a second
concurrent caller rather than letting two into LiteRT at once, which LiteRT does
not promise is safe and whose profile buffer says outright that it is not.
`..._server` puts one model in a process so several callers can share it, with a
bounded queue that refuses work past 64 pending calls instead of growing a
mailbox until the node dies. `..._isolated` puts the model on a node of its own,
for callers who cannot afford a segmentation fault in native code to be fatal;
when that node dies the calls answer `{error, Binary}` and the process stays up
as a handle, so recovering is a decision rather than something that happens to
the caller.

**`fully_accelerated/1`, because a partial answer is worse than no answer.** An
accelerator that takes half the graph pays for every crossing back to the CPU and
is often slower than the CPU alone. Whether it took the whole graph is not
otherwise visible.

**Two precompiled binaries per target now.** The LiteRT API is a build option and
is off by default, so a single published binary would have meant that everything
above was unreachable for anyone installing the ordinary way. Every target ships
a plain tarball and, where LiteRT can work, a second one with it compiled in.
`TFLITE_BEAM_ENABLE_LITERT_API` picks between them at install time and is the same
variable that turns it on in a source build, so asking for the LiteRT API means
the same thing however the library arrives.

**armv6 and armv7l get no LiteRT variant, and say so.** LiteRT's CPU accelerator
*is* XNNPACK: `RegisterCpuAccelerator` is defined in
`litert/runtime/accelerators/xnnpack/xnnpack_accelerator.cc`, which its CMake
build compiles unconditionally. Those two targets build with XNNPACK off, so the
library links with `TfLiteXNNPackDelegate*` undefined and fails to load, which was
found by shipping one to a Pi Zero W and running it. Asking for the LiteRT API on
such a target is now a configure error naming the reason, rather than a 20MB
artifact that cannot be loaded.

**The GPU accelerator follows the LiteRT API rather than being a second thing to
ask for.** With it off, `RegisterGpuAccelerator` compiles to a stub and a caller
who names the GPU is silently given the CPU with nothing to explain it.

**Tearing a compiled model down no longer stalls a scheduler.** A resource
destructor runs wherever the last reference was dropped, normally an ordinary
scheduler, and ERTS expects anything there to return within about a millisecond.
Measured: a 13MB mobilenet took 491us to destroy and a 49MB model took 15.8ms,
growing faster than the model does. The destructor now hands the pieces to one
reaper thread and returns in under 5us.

**`pending_events/1`, because the cost of reading a profile is not the number
asked for.** LiteRT will not hand over part of a backlog, so `profile/2` copies
whatever is waiting whatever the limit says: 104 bytes an event, twice over while
LiteRT builds its own copy, which is about 109MiB for a full buffer. Nothing on a
workstation and fatal on a board with 256MB.

**Two answers changed shape.** `profile/1`'s `type` and `source` were LiteRT's own
enumeration numbers and are now named against its constants, so an upstream
renumbering cannot quietly change what one means; a type this build has no name
for still arrives as its number. `summarise_profile/1` returned anonymous
positional 4-tuples and now returns maps of `tag`, `kind`, `count` and `us`, so a
field can be added later without breaking every caller that matched on position.

**What the review found, in the parts that were already here.** Three data races
in the interpreter's ownership state, which 155 passing test cases had nothing to
say about: the setter, the getter, and an unlocked check inside `get_resource`
whose own comment claimed it was a safety measure. `std::atomic` members were
being used in storage `enif_alloc_resource` returns without running a
constructor. Model-sized traversals and flatbuffer verification were on ordinary
schedulers. `on_upgrade` did not open the resource types.

**The sanitizers run in CI, on macOS and Linux.** `scripts/run_sanitizer.sh`
builds under ThreadSanitizer or AddressSanitizer and drives the NIF; it is what
found the races above, and it is checked against a deliberately reintroduced race
so that a clean run means something. Where TSan cannot see the emulator's own
synchronisation the edges are stated with `__tsan_release`/`__tsan_acquire` rather
than suppressed, because a suppression that hides an artifact today hides a real
bug tomorrow with no signal when it starts doing so.

**Three checks that CI did not have.** dialyzer, which had eight standing
warnings including one wrong spec and one unreachable clause. A NIF table balance
check, because a function declared in Erlang and registered in only one build
shape is valid C++ in both and raises `{not_loaded, ...}` on the day someone calls
it. And the eight-shape preprocessor check now works on a clean tree; it had been
passing on LiteRT rows only because an earlier build had left a generated header
behind.

## v1.0.0-rc2 (2026-08-26)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v1.0.0-rc2) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v1.0.0-rc2)

Everything here came out of going back over rc1 looking for defects rather than
waiting for them to arrive. Nothing in it changes what the runtime computes.

**One answer changed shape.** `get_signature_defs/1` returned the names it read
out of the model as atoms. Atoms are never reclaimed, so a node that kept loading
models with names of their own grew the atom table until it came down. They are
binaries now, which is what `signature_keys/1`, `signature_inputs/2` and
`signature_outputs/2` next door have always answered. Code that matched
`#{serving_default := _}` wants `#{<<"serving_default">> := _}`.

**Chinese was being thrown away.** BERT spaces out CJK ideographs before
splitting on whitespace. The Swift file this tokenizer was ported from skips
that step, which it can afford to because it serves one English model. Here a
whole sentence arrived as a single word, ran past wordpiece's two hundred
character limit and came back as `[UNK]`: a sentence whose every character was in
the vocabulary was answered as nothing at all. `tflite_beam_basic_tokenizer` now
does what BERT does, kana and hangul left alone for BERT's stated reason.

**Tokenizing was paying for a process round trip per character.** `is_punctuation/1`
called into the table's process for every code point, and that call plus the path
lookup feeding it was 5.6us of the 5.6us a character cost: the tokenizing itself
did not register. The table is read once per call now, from a `persistent_term`.
Ordinary text went from 5.56us to 0.21us a character; the wordpiece accumulator
and the two in the basic tokenizer were quadratic in their input and are not.
24,000 characters of Chinese went from 923ms and one wrong token to 13ms and
24,000 right ones.

**Five ways to end a process that should have been answers.** A malformed unicode
table, a `predict/2` input that was not binary data, an input element inside a
list or map that was not, a table the punctuation set could not read, and an
interpreter server that could not allocate: each of these matched against data it
had not checked and took the caller, or the server, with it.

**Four native guards that were being skipped.** `get_signature_defs/1` read the
signature vectors without the in-use lock every other accessor takes. Three call
sites reached past `get_resource` to `enif_get_resource` and so skipped the
ownership check with it, one of them the rebuild that deletes and replaces the
whole interpreter. Both buffer model constructors copied into whatever
`enif_alloc` answered without looking at it, and it answers null. A rebuild that
threw partway left the resource holding an interpreter LiteRT had already
destroyed.

**Downloads.** A path component that arrived as a binary, which is every
component arriving from Elixir, was not checked for `..` at all: the comparison
was against a string and `filename:split/1` keeps the representation it is given.
A component that lands outside the cache through a symlink is refused too, and a
name that is not UTF-8 is accepted, because on a filesystem that promises no
encoding that is a name and not an escape. An https download that cannot be
verified is now refused rather than made without verifying: `TFLITE_BEAM_CACERT`
names a store and is honoured or reported, and `TFLITE_BEAM_UNSAFE_HTTPS` is how
someone asks for the old behaviour.

**Edge TPU options were read and discarded.** `get_edge_tpu_context/1` accepted
the options map and never looked at it, so performance, DFU and queue length all
took their defaults while the caller was handed a context and an `ok`. They are
read and checked now, and `coral_get_edgetpu_context_options/1` reads back what
the device was actually asked for. libcoral only forwards them for a device named
with an index, so `"usb:0"` carries options where `""` does not.

The suite went from 113 cases to 131. Every fix here has one, and every one of
them was checked by putting the defect back.

## v1.0.0-rc1 (2026-08-26)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v1.0.0-rc1) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v1.0.0-rc1)

The runtime now comes from LiteRT rather than from TensorFlow. That is the whole
of why this is 1.0.0 and not 0.4.0: `~>` treats a two part 0.x requirement as
everything below 1.0.0, so shipping this as 0.4.0 would have moved every existing
`~> 0.3` user onto a different upstream on their next `deps.update`, without
their asking. Under 1.0.0 they stay where they are until they say otherwise. The
0.4.0 release candidates carried the memory safety work and no 0.4.0 final was
published; everything in them is here.

Nothing in the Erlang API was removed or renamed, the seven precompiled targets
are the same seven, and each one asks for exactly the glibc it asked for in
v0.3.12. What did change is one answer, described first below.

### Changed
- `tflite_version/0` answers LiteRT's version, which is `<<"2.2.0">>`, where it
  used to answer TensorFlow's `<<"2.21.0">>`. The two are separate version lines
  and the numbers are not comparable: LiteRT's 2.2.0 is newer than TensorFlow's
  2.21.0, not older. **A delegate plugin loaded through
  `tflite_beam_delegate:external/1` must match this number**, and upstream
  offers no binary stable delegate interface, so a mismatch is undefined
  behaviour rather than an error. Rebuild plugins against LiteRT 2.2.0.
- TfLite is built from `tflite/` in the LiteRT tree instead of `tensorflow/lite/`
  in TensorFlow's. TensorFlow is still fetched, because LiteRT's own build reaches
  into it for `compiler/mlir/lite`, TSL and XLA, but it is a build dependency now
  rather than the source of the runtime. See `tensorflow_version/0` for which
  release that is.

### Added
- `tflite_beam:source_tree/0` answers `litert`. It exists because nothing about a
  shared object says which sources it came from, and the ways to end up holding
  the wrong one are quiet: a precompiled artifact fetched because `priv/` happened
  to be empty, a stale copy in `_build`, a local build whose includes resolved
  against TensorFlow because that tree is on the path for LiteRT's own reasons.
  Each of those links, builds, and passes most of a test suite. The C++ behind
  this function names a type only LiteRT's schema defines, so a binary built from
  anything else does not compile, and a release from before the move has no such
  function to ask.
- `tflite_beam:tensorflow_version/0` answers the TensorFlow release the build
  pulled in, `<<"2.21.0-rc0">>`. Worth having when something reads wrong, not for
  matching a plugin against.
- The two 8 bit float types are reported, under the names Nx uses: `{f, 8}` for
  E5M2 and `{f8_e4m3fn, 8}` for E4M3FN. Neither goes out as a bare width, because
  the width is the one thing they share: reading E4M3FN bytes as E5M2 does not
  fail, it answers a different number, and `16#78` is 32768 under one and 256
  under the other.

### Fixed
- A model path that is not there says so. Routing the loader through the
  verifier in v0.4.0-rc6 made every failure report "not a valid flatbuffer",
  including a path with a typo in it, which is both the commonest way to get
  here and the least accurate thing to say about it. A missing or unreadable
  file now names the reason the operating system gave, and the test that covers
  it no longer settles for `{error, _}`.

## v0.4.0-rc6 (2026-08-24)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.4.0-rc6) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.4.0-rc6)

Eleven faults on the path a caller actually takes, three of which end the
emulator rather than return an error, and one of those writes integers of the
caller's choosing past the end of a stack buffer. Four more answered with the
wrong value or quietly dropped one. All were found by auditing the binding
against TfLite's own contracts rather than by a crash report, and each is held by
a test that fails without its fix.

### Added
- Model metadata now reports `custom_metadata`. The field has been on every
  subgraph in the schema all along and nothing here ever read it, so whatever a
  model author put there was unreachable.
- `tflite_beam:xnnpack_max_tensor_dims/0` reports the widest tensor the delegate
  in this build can describe, or `nil` where no delegate imposes a width, as the
  armv6 and armv7l targets do. It is the number the resize guard below enforces,
  so the rule is one a caller can ask about rather than discover by being
  refused. CMake lifts the value out of XNNPACK's own header at configure time,
  which keeps the guard, this function and the delegate from ever disagreeing.

### Fixed
- Resizing an input tensor past the delegate's width is refused. XNNPACK holds a
  tensor's dimensions in a `std::array<size_t, XNN_MAX_TENSOR_DIMS>` and bounds
  the count only when it first decides to take the graph. Nothing rechecks it on
  the reshape that `resize_input_tensor/3` reaches, so every dimension past the
  sixth was written off the end of that array, and what it wrote were the
  integers passed in from Erlang. Rank 7 and 8 tripped the stack protector, rank
  10 took SIGBUS, and a dimension of `16#12345678` reached SIGSEGV. Only the
  unsafe transition is refused: a tensor already wider than the bound was never
  delegated and can still be reshaped, and a build without the delegate refuses
  nothing. The upstream code is unchanged in LiteRT, so the guard stays after the
  source tree moves.
- Reading a tensor by index no longer depends on it having a name. TfLite leaves
  the name null on the scratch tensors an op allocates for itself, and the name
  helper ran `strlen` on that null, so walking a graph took the emulator down on
  any model with one. A detection model reaches it at index 261.
- A truncated or corrupt model is refused instead of walked. `build_from_buffer`
  and `build_from_file` used the constructors that do not verify, so a model cut
  short segfaulted inside the NIF before returning anything. Both verify now, at
  the cost of a linear scan next to a copy the loader already made.
- A tensor whose content is `FeatureProperties` keeps its metadata. That table
  is an empty marker in the schema, so there is nothing in it that can fail, but
  reading one was treated as a failure and discarded the whole content map built
  around it, `content_properties_type` and `range` included.
- A score thresholding unit reports `global_score_threshold` under its own name.
  It was reported as `default_score`, which is a real and different field on the
  `ScoreCalibrationOptions` beside it, so the name did not merely read oddly, it
  named something else.
- An absent optional file no longer empties a tensor's `process_units`. The
  tokenizer options treated a missing `vocab_file` as a failure, and the loop
  over the units gives up on the first failure, so a SentencePiece tokenizer
  shipping only its model discarded every unit on that tensor, unrelated ones
  included. The Bert and regex options had the same shape.
- `set_num_threads/2` accepts the values TfLite documents. Its own contract is
  ">= 0, or just -1 to let TFLite runtime set the value", and the interpreter
  refused everything below 1, so it turned away the one value that asks TfLite
  to choose. `tflite_beam_interpreter_builder:set_num_threads/2` next to it
  always passed the integer straight through, and its spec has been widened from
  `pos_integer()` to match.
- The model cache creates nested directories. Every HuggingFace repository id is
  `owner/name` and goes in as the cache subdirectory, but the cache called the
  non recursive `file:make_dir/1`, which fails when the parent is missing. All 88
  models in the contrib catalogue were undownloadable.
- Writing to a tensor takes exactly its size. A short binary used to be copied as
  far as it went and reported as success, leaving the rest of the tensor holding
  whatever the arena held before and producing an answer computed partly from
  that. A long one was truncated just as quietly. Both are refused now, and the
  error names both sizes.
- `predict/2` reports a bad input instead of crashing. The code that collected
  what went wrong while filling the inputs appended each failure with `R/binary`,
  which raises `badarg` on the `{error, Binary}` that `set_data/2` actually
  returns. So the one path that had a real reason to report crashed instead of
  reporting it, and it crashed hardest where it mattered most, since every
  refusal from the interpreter guard arrives there. Inside
  `tflite_beam_interpreter_server` that `badarg` took the whole process with it,
  so one malformed request from one client destroyed the served model for
  everybody.
- `predict/2` no longer answers from a failed invoke. It discarded what `invoke/1`
  returned and read the output tensors regardless, so a refused or failed run
  handed back the previous one's answer. Concurrently that is the fault the
  interpreter guard exists to close, arriving by a different door: 14 wrong
  answers in 400 calls before, 6 after. The rest is the gap between feeding,
  running and reading, which no per-call guard can close and
  `tflite_beam_interpreter_server` can, measured at 400 correct in 400.

## v0.4.0-rc5 (2026-08-22)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.4.0-rc5) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.4.0-rc5)

A NIF fault takes the emulator with it, and this release is almost entirely about
the places where one could. Every defect below was found by reading the binding
against TfLite's own contracts and then proving it with a test that fails without
the fix, rather than by waiting for a crash report.

### Fixed
- A tensor handle now keeps its interpreter alive. It holds a pointer into that
  interpreter's arena and nothing kept the interpreter around on its behalf, so a
  handle could outlive what it pointed at and read freed memory. Erlang gives no
  warning before that happens: the compiler stops counting a variable as live at
  its last mention, so an interpreter someone fetched a tensor from and then never
  named again is collectable while the tensor is still in use.
- Handles are retired when what they borrow moves. `allocate_tensors/1`, both
  resizes and a second `build/2` all relocate tensors, and TfLite says plainly
  that `Invoke` may too. A handle taken before any of those now reports that it
  has been retired instead of reading through a stale pointer. Invoke is checked
  rather than assumed: only handles whose index no longer resolves to the pointer
  they hold are retired, so the ordinary fetch, set, invoke, read sequence still
  works.
- Every entry point that touches an interpreter takes the same guard, not only the
  ones that write. Sixteen read-only calls held nothing while `build/2` was free to
  delete the interpreter underneath them. Both handle types also read their
  liveness before taking that guard and never again, which left a window a rebuild
  fitted into exactly.
- Cancelling still works during an invoke, which is the only time it is worth
  anything, but can no longer run while a rebuild is deleting the interpreter it
  is about to reach into.
- Three places took a reference on a resource and then ran an allocating step
  before anything recorded it. A failure in between stranded the reference, and in
  one case left a mutex locked so that every later reader of that registry waited
  forever.
- Every exported entry point is now behind an exception guard. It had been put on
  the twenty that were seen to allocate, and that claim was already false for the
  other sixty-four.
- `error_reporter_default_error_reporter/0` wrote through a null resource on the
  one path that already knew it was out of memory.
- Six smaller ones: a byte written past the end of every error binary, five places
  the binding could read or write out of bounds, model metadata walked without
  being verified first, a leak in the signature runner registry, an error reporter
  a model could outlive, and `get_associated_file/2` calling `map:from_list` when
  the module is `maps`.

### Changed
- `tflitetensor_to_binary` and `tflitetensor_set_data` moved to a dirty scheduler.
  Both copy the whole tensor, so their cost belongs to the model rather than being
  fixed: a 64 MB tensor takes 3.85 ms to read and 1.57 ms to write, well past what
  a normal scheduler should be holding. `get_signature_runner` and
  `read_all_metadata` moved for the same reason.
- `scripts/generate_checksums.sh` now names every target it expects rather than
  counting them, and refuses to write a manifest that is missing one or that
  contains a tarball from another release. A manifest covering six of the seven
  targets is worse than none: the seventh refuses to install and the six that
  verify give no hint why, which is the shape of the 0.4.0-rc4 slip. The installer
  still refuses a file the manifest does not name, because the alternative would
  disable verification for a whole release at once.

### Added
- `tflite_beam_lifetime_SUITE`, seventeen cases that each stand for a defect that
  reached the repository. Every one was checked against a build without its fix:
  four of them abort the node there, one deadlocks, and the rest report the wrong
  answer or leak between five and seven megabytes.
- A fault-injection facility for the windows that only open when an allocation
  fails, which no test can otherwise reach. It refuses to arm unless
  `TFLITE_BEAM_ENABLE_FAULT_INJECTION` is set in the environment before the node
  starts, so it is not something an application can reach by accident.

## v0.4.0-rc4 (2026-08-21)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.4.0-rc4) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.4.0-rc4)

Two processes sharing one interpreter used to get each other's answers -- silently,
without a crash, 147 times in 400 on a real model. Most of this release is about that,
and about checking that a downloaded binary is the one we published.

### Added
- `tflite_beam_interpreter_server`, an interpreter that lives inside a process so that
  feeding it, running it and reading the result back is one step nothing can
  interleave with. Concurrent callers are serialised by the process and each gets the
  answer to its own input.

  The direct API mirrors TfLite's C API faithfully, and that is the problem it is
  answering: nothing in the C API says those three calls have to be treated as one
  operation. Two processes taking turns badly get each other's results -- measured on
  a real model, 147 wrong answers in 400 calls, silently and without a crash. The
  direct API is unchanged for callers who would rather serialise access themselves.
- `tflite_beam_interpreter:controlling_process/1,2`, following
  `gen_tcp:controlling_process/2`: while an interpreter belongs to nobody any process
  may take it, and once it belongs to someone only that process may hand it on. Every
  other process is then refused. A controlling process that dies releases it, since an
  interpreter has no equivalent of a socket being closed. Interpreters start out
  belonging to nobody, which is how they have always behaved.

### Changed
- Calls into one interpreter that genuinely overlap in time are now refused instead of
  being allowed to race. Two processes sharing an interpreter used to reach TfLite on
  two OS threads at once with nothing in the way; the second one is now told. This is
  the only change here that alters existing behaviour, and only for code that was
  already racing.

### Security
- Precompiled tarballs are checked against a sha256 manifest before being unpacked.
  They were written to disk and extracted unverified, while every comparable BEAM
  package -- evision, xla, emlx -- verifies. The manifest, `checksum.term`, ships
  inside the package, because a checksum fetched alongside the thing it vouches for
  vouches for nothing.

  A tarball that does not match is deleted and the build fails, rather than being
  left in the cache to fail identically forever. The cached path is checked too: a
  tarball that was already on disk has no more claim to being the right one than a
  freshly fetched one. A checkout with no manifest -- a git tag, whose tarballs are
  built after it exists -- says so loudly and carries on, since the manifest is the
  trust root rather than something to fetch.

## v0.4.0-rc3 (2026-08-19)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.4.0-rc3) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.4.0-rc3)

Delegates. A delegate is now a thing you can hold, configure and attach, rather than
something TfLite did to your model without telling you -- and any vendor's delegate
library can be loaded at runtime, the Edge TPU included.

Still a release candidate: the one behaviour change here, XNNPACK moving from TfLite's
invisible lazy application to an explicit attachment at build time, is worth having in
the open before it becomes 0.4.0.

### Added
- `tflite_beam_interpreter_builder:add_delegate/2,3`, and the delegate resource behind
  it. This is the attachment point rather than a usable feature yet: nothing in this
  release constructs a delegate, so the constructors arrive with the delegate kinds
  themselves. A delegate is kept alive by the builder and by every interpreter built
  from it, for as long as either needs it, which is why there is no way to detach or
  free one -- an early release is exactly the use-after-free class that 0.4.0-rc2 spent
  its time removing.
- `add_delegate/3` takes `#{on_decline => error | fallback}`. TfLite reports a delegate
  that cannot take the graph, but leaves it runnable, as `kTfLiteApplicationError`, and
  then discards the whole interpreter -- so without this a delegate that merely does not
  fit turns `build/2` into an error with no interpreter at all, where a C++ caller would
  still hold a working CPU one. `error`, the default, keeps that loud. `fallback` builds
  again without the delegates that were added with it and answers
  `{ok, delegate_declined}`; nothing else is retried.
- `tflite_beam_delegate:available/0`, reporting which delegate kinds were compiled into
  this build. It answers "was it compiled in", not "is a device present" -- those have
  different answers on the same binary. It lists `xnnpack` everywhere except armv6 and
  armv7l, where XNNPACK is not compiled in at all, and `external` on every target,
  since loading a plugin needs nothing but the dynamic loader.
- `tflite_beam_delegate:xnnpack/0,1`, with `num_threads`, `flags` and
  `weight_cache_file_path`. Flags are atoms mapped by name -- `qs8`, `force_fp16`,
  `disable_subgraph_reshaping` and the rest -- and are added to XNNPACK's defaults
  rather than replacing them, because TfLite spells turning a default off as its own
  flag. Nothing positional would be right in any case: one bit in the middle of the
  range is unassigned.
- `tflite_beam_coral:edge_tpu_delegate/0,1`, which reaches an Edge TPU the same way
  as any other delegate. libedgetpu has always been a TfLite delegate plugin -- the
  bundled runtime exports `tflite_plugin_create_delegate` and
  `tflite_plugin_destroy_delegate` -- so this is `external/2` pointed at it, plus a
  default path to the copy in `priv/libedgetpu`. Pass `lib_path` to name a runtime
  installed elsewhere, which is how a build made without Coral support can still
  reach a device.

  What it buys over `make_edge_tpu_interpreter/2`, which is unchanged and still
  works: that function builds its own interpreter internally, so nothing set on a
  builder ever reaches it -- neither `set_num_threads/2` nor any other delegate.
  Going through the plugin puts an Edge TPU interpreter on the ordinary builder
  path. Both routes were checked to produce byte-identical output on a USB Coral
  accelerator with libedgetpu 0.1.14 on macOS arm64, and asking for a device that is
  not attached is an ordinary error rather than a crash.
- `tflite_beam_delegate:external/1,2`, which loads a delegate out of any shared
  library implementing TfLite's plugin interface -- Edge TPU, a GPU delegate built
  elsewhere, a vendor delegate this library has never heard of. Options are handed
  over as strings, since that is the whole of the plugin ABI, so atoms and integers
  are converted and at most 256 pairs fit.

  It does not go through `TfLiteExternalDelegateCreate`. That function returns a
  pointer into a wrapper whose delegate it fills in only when the library loaded
  *and* the plugin returned a delegate, so a missing file, a library that is not a
  plugin, or a plugin that declines -- no device attached, say -- all hand back a
  non-null delegate whose `Prepare` is indeterminate. Attaching one of those jumps
  through a wild function pointer and takes the emulator with it. The plugin is
  loaded here instead, which has no such gap and gives every failure a name,
  including the plugin's own explanation of why it refused.
- `tflite_beam_ops_builtin_builtin_resolver:new/1` takes
  `#{apply_default_delegates => boolean()}`, deciding whether TfLite may apply its own
  delegates lazily inside `allocate_tensors/1`.

### Changed
- **XNNPACK is now attached explicitly, by `tflite_beam_interpreter_builder:build/2`,
  instead of being applied invisibly by TfLite inside `allocate_tensors/1`.** The
  acceleration is the same and so is the output; what changes is that the delegation is
  visible in the execution plan as soon as `build/2` returns rather than only after
  allocation, and that it can be configured or declined at all. `set_num_threads/2`
  still reaches XNNPACK: the delegate is built with the builder's thread count, or with
  one thread when it was never set, which is what TfLite's own default has always been.

  Attach your own delegate and the default is not added. Ask the resolver for
  `#{apply_default_delegates => true}` and TfLite goes back to delegating by itself. On
  armv6 and armv7l, where XNNPACK is not compiled in, nothing is attached and nothing
  errors.

- `tflite_beam_interpreter_builder:build/2` and `tflite_beam_interpreter:allocate_tensors/1`
  now run on a dirty CPU scheduler. Every delegate's `Prepare` and all of TfLite's graph
  partitioning happen inside those two, which is more than a regular scheduler should be
  asked to hold. `coral_make_edgetpu_interpreter/2`, which does build, delegate and
  allocate in one call, was already classified this way.

### Documented
- An interpreter, and any delegate attached to it, belongs to one process at a time.
  This was already true -- `invoke/1` has run on a dirty scheduler for a long time, and
  there is no lock anywhere in the bindings -- it was simply never written down.

## v0.4.0-rc2 (2026-08-19)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.4.0-rc2) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.4.0-rc2)

Bug fixes only, no new API. Three of these interlock: a build that fails without saying
so produces an empty interpreter, a guard that was supposed to reject empty interpreters
waves it through, and the next accessor call takes the VM down with it. None of the
three needs anything unusual to reach.

### Changed
- `tflite_beam_interpreter_builder:build/2` returns `{error, Reason}` when the build
  fails. It returned `ok` unconditionally and discarded the status TFLite handed it, so
  a model that could not be built reported success and left an empty interpreter
  behind. Code that matched `ok = build(...)` on a model that was quietly failing will
  now fail at that match, which is the point.

  In `tflite_elixir` this reaches `TFLiteElixir.InterpreterBuilder.build!/2`, which
  starts raising through `deferror` where callers used to meet a `MatchError` further
  down. That suite has no negative test for `build/2` -- every call site in it is a
  happy path -- so nothing there will notice the difference.

### Fixed
- Reaching into an interpreter that a failed `build/2` had emptied killed the VM with
  SIGSEGV. Every resource accessor set an error term when it found a null value and
  then returned the resource anyway, while every caller tests only the returned
  pointer, so the guard passed and the next line dereferenced null. All eight of them
  now return nothing, and the calls that used to crash return `{error, Reason}`.
- `build/2` no longer leaves previously fetched tensors pointing into freed memory.
  TFLite destroys the interpreter it is building into on the way in -- before it can
  fail, so this applies to failed builds too -- but the tensor handles cached by
  `tflite_beam_interpreter:tensor/2` were never cleared. Fetching a tensor and then
  building again was a use-after-free.
- Tensor handles now report that their interpreter has gone instead of reading freed
  memory. The interpreter marked each cached tensor when it was torn down, but nothing
  ever read that mark: all six NIFs taking a tensor checked only that its pointer was
  non-null, which a dangling pointer is.

  This is visible in one more place than the two above: a handle does not keep its
  interpreter alive, so reading through one whose interpreter has already been
  collected now returns `{error, Reason}' where it used to return whatever was left in
  the freed memory. Keep the interpreter reachable for as long as its tensors are in
  use -- which is what the code doing this correctly already does, or it would have
  been crashing.

### Added
- A test suite, `rebar3 ct`, covering model loading, the builder, interpreters,
  tensors, invocation and signature runners, along with the failure cases above. It
  runs in CI on Linux x86_64 and macOS arm64. The four model fixtures it uses come from
  TensorFlow's own testdata and live in `test/`, which is not part of the published
  package.

## v0.4.0-rc1 (2026-08-19)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.4.0-rc1) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.4.0-rc1)

A release candidate. Everything here is new surface rather than changed behaviour, but
it is a lot of it at once, so it is worth a look before it becomes 0.4.0. Depend on it
explicitly -- a pre-release is not picked up by a requirement like `"~> 0.3"`.

### Added
- Signature runners. A model's signatures could be listed with
  `interpreter:signature_keys/1` and `get_signature_defs/1`, but there was no way to
  run one, so tensors still had to be addressed by index and the order of a model's
  outputs guessed at. `tflite_beam_interpreter:get_signature_runner/2` now returns a
  runner, and `tflite_beam_signature_runner` drives it: names and counts of its inputs
  and outputs, reading and writing them by name, resizing them, allocating, invoking
  and cancelling.

  Passing `nil` as the key asks for the primary subgraph, which works on models that
  declare no signatures at all, so this is usable with older exports too.

  A runner belongs to the interpreter that handed it out and holds a reference to it,
  so it stays usable even after the interpreter's own term is collected. Like the
  interpreter it is not safe to use from several processes at once.
- `tflite_beam_interpreter:enable_cancellation/1` and `cancel/1`. An invocation runs on
  a dirty scheduler and could not be interrupted; `cancel/1` does not block and is safe
  to call from another process, so a long inference can now be given up on. Without
  `enable_cancellation/1` beforehand, cancelling is an error.
- `tflite_beam_interpreter:release_non_persistent_memory/1`, which hands back the memory
  that is only needed while invoking. Invoking again reallocates it, trading time for
  memory on devices short of the latter.
- `tflite_beam_interpreter:reset_variable_tensors/1`, resetting all of a model's
  variable tensors. Only a single-tensor version existed.
- `tflite_beam_interpreter:get_allow_fp16_precision_for_fp32/1` and
  `set_allow_fp16_precision_for_fp32/2`.
- `tflite_beam_interpreter:signature_inputs/2`, `signature_outputs/2`,
  `get_subgraph_index_from_signature/2` and `subgraphs_size/1`, which describe a
  model's signatures and subgraphs without having to build a runner.
- `tflite_beam_interpreter:resize_input_tensor/3` and `resize_input_tensor_strict/3`.
  Input shapes could not be changed at all before, so a model with a variable
  dimension could only ever be fed whatever shape it was exported with. Call
  `allocate_tensors/1` again afterwards. The strict variant only touches dimensions
  the model left unknown.
- `tflite_beam_flatbuffer_model:verify_and_build_from_buffer/1,2`. A verifying
  counterpart existed for files but not for buffers, so a model already in memory
  could only be built unchecked.

### Fixed
- The `minimum_runtime` field of the `tflite_beam_flatbuffer_model` record held a
  boolean. Three of the four places that fill the record asked
  `flatbuffer_model_initialized` for it, so anyone reading the field to decide
  whether a runtime is new enough was reading `true`.
- Building a model from a buffer no longer leaks the copy of that buffer when the
  model turns out not to parse.

## v0.3.12 (2026-08-19)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.3.12) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.3.12)

### Fixed
- Interpreters, interpreter builders and the resources they borrow (the op resolver
  and the flatbuffer model) now hold real references to each other. Previously only a
  hand-rolled counter recorded the link, so the VM was free to collect a resource that
  was still in use, and the destructor's decrement landed in whatever resource had been
  given that memory next. On arm64 this brought down the emulator with SIGBUS.
- Resources are no longer read uninitialised. `enif_alloc_resource` hands back raw
  memory, so fields that looked initialised in the struct definition were not, and
  `NifResTfLiteTensor` could reach `delete` on a wild pointer.
- Every `tflite::FlatBufferModel` and every `tflite::Interpreter` was leaked; both are
  now released with the resource that owns them.
- Creating an Edge TPU interpreter no longer leaks its resource when the interpreter
  cannot be built or its tensors cannot be allocated.
- Tensor resources are no longer leaked. Each one was created with a reference that
  nobody ever gave back, on top of the one the interpreter's cache holds, so none of
  them could be freed. Failing partway through reading a tensor leaked one as well.
- Edge TPU context resources are no longer leaked, for the same reason: the reference
  from `enif_alloc_resource` was never released.
- `allocate_tensors` no longer reports `unknown error` for three of the statuses
  TFLite can return. A model carrying ops the interpreter cannot resolve -- an Edge
  TPU model given to a plain builtin resolver, say -- now says `UnresolvedOps`
  instead. The mapping lived in two places, one of which had drifted; there is now
  only one.
- The Edge TPU itself is handed back when nothing is using it any more. Contexts were
  parked in a global map that was written to and never read, purely so their
  `shared_ptr` could not run out, which held the device until the VM exited. Each
  context resource now owns its share directly, and an Edge TPU interpreter holds a
  reference to the context it delegates to, so the device outlives every interpreter
  built on it and is released once the last one is gone.

## v0.3.11 (2026-08-15)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.3.11) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.3.11)

### Fixed
- Platform-specific binaries now go to the consuming app's `_build/<target>_<env>/lib/tflite_beam/priv`
  instead of `deps/tflite_beam/priv`, so switching `MIX_TARGET` no longer picks up
  another target's `tflite_beam.so`. `rm -rf deps/tflite_beam` is no longer needed
  when cross-compiling ([#73](https://github.com/cocoa-xu/tflite_beam/issues/73)).
- Building from source no longer fails on hosts that have gflags installed
  system-wide (e.g. `brew install gflags`). glog resolved gflags through
  `find_package`, which picked up the system copy and collided with the targets
  the bundled gflags had already defined.

## v0.3.10 (2026-06-30)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.3.10) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.3.10)

### Changed
- [deps] Use libedgetpu v0.1.14.
- Use tensorflow v2.21.0.

## v0.3.9 (2025-04-03)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.3.9) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.3.9)

### Changed
- [deps] Use libedgetpu v0.1.12.
- Use tensorflow v2.19.0.

## v0.3.8 (2025-02-10)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.3.8) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.3.8)

### Changed
- [deps] Use libedgetpu v0.1.10.
- Use tensorflow v2.18.0.

## v0.3.7 (2024-09-03)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.3.7) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.3.7)

### Fixed

- fixed project build directory

## v0.3.6 (2024-03-17)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.3.6) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.3.6)

### Changed
- [deps] Use libedgetpu v0.1.9.
- Use tensorflow v2.16.1.
- Use libusb v1.0.27.
- Use Erlang/OTP 25.x for precompiled binaries. This unified the required Erlang/OTP NIF version to `2.16` for precompiled binaries.
- Detect and use `HTTP_PROXY`, `HTTPS_PROXY`, `http_proxy` and `https_proxy` when fetch preocmpiled binary from GitHub.

## v0.3.5 (2024-01-24)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.3.5) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.3.5)

### Changed
- Precompiled version for armv6 devices.
- Removed `TFBEAM_XNNPACK_ENABLE_ARM_I8MM` option as it should work as long as a newer C compiler is used.
- Updated metadata_schema to 1.5.0

## v0.3.4 (2024-01-23)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.3.4) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.3.4)

### Changed
- [deps] Use libedgetpu v0.1.8.
- Use tensorflow v2.15.0.

## v0.3.3 (2023-07-21)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.3.3) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.3.3)

### Changed
- [deps] Use libedgetpu v0.1.7.
- Use tensorflow v2.13.0.

## v0.3.2 (2023-04-03)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.3.2) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.3.2)

### Fixed
- [precompiled-nerves] Guess correct `TARGET_ARCH` from `TARGET_CPU`.

## v0.3.1 (2023-04-03)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.3.1) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.3.1)

### Fixed
- [deps] Use libedgetpu v0.1.6.

### Changed
- [examples] Examples moved to [cocoa-xu/tflite_elixir](https://github.com/cocoa-xu/tflite_elixir).

## v0.3.0 (2023-04-02)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.3.0) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.3.0)

### Breaking Change
- This repo will now be the TensorFlow Lite Erlang bindings. For Elixir bindings, please visit [cocoa-xu/tflite_elixir](https://github.com/cocoa-xu/tflite_elixir).

### Fixed
- [erlang] Generate correct error message from a list of errors.
- [c_src] Initialize resource pointers with `nullptr`.
- Implemented tokenizers for MobileBERT (#57) by @cocoa-xu.
- [make] Ensure priv dir exist.

## v0.2.1 (2023-04-02)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.2.1) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.2.1)

### Changed
- [deps] Use TensorFlow Lite version 2.11.1.

### Fixed
- [erlang] Generate correct error message from a list of errors.
- [c_src] Initialize resource pointers with `nullptr`.
- Implemented tokenizers for MobileBERT (#57) by @cocoa-xu.
- [make] Ensure priv dir exist.

## v0.2.0 (2023-03-30)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.2.0) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.2.0)

### Breaking Changes
- Renamed root namespace from `TFLiteElixir` to `TFLiteBEAM`

### Changes
- `buffer` will be copied and managed when using `TFLiteBEAM.FlatBufferModel.build_from_buffer/1`.
- `TFLiteBEAM.TFLiteTensor.dims/1` returns a list (following TensorFlow Lite's C++ API convention) while `TFLiteBEAM.TFLiteTensor.shape/1` returns a tuple (folllowing `nx`'s convention.)

### Added
- Erlang support.
- [example] added pose estimation example (#43) by @mnishiguchi
- [example] use thunder model instead of lightning in pose estimation (#45) by @mnishiguchi
- [example] added audio classification example
- Experimental high-level module `TFLiteBEAM.ImageClassification`.

  ```elixir
  iex> alias TFLiteBEAM.ImageClassification
  iex> {:ok, pid} = ImageClassification.start("test/test_data/mobilenet_v2_1.0_224_inat_bird_quant.tflite")
  iex> ImageClassification.predict(pid, "test/test_data/parrot.jpeg")
  %{class_id: 923, score: 0.70703125}
  iex> ImageClassification.set_label_from_associated_file(pid, "inat_bird_labels.txt")
  :ok
  iex> ImageClassification.predict(pid, "test/test_data/parrot.jpeg")
  %{class_id: 923, label: "Ara macao (Scarlet Macaw)", score: 0.70703125}
  iex> ImageClassification.predict(pid, "test/test_data/parrot.jpeg", top_k: 3)
  [
    %{class_id: 923, label: "Ara macao (Scarlet Macaw)", score: 0.70703125},
    %{
      class_id: 837,
      label: "Platycercus elegans (Crimson Rosella)",
      score: 0.078125
    },
    %{
      class_id: 245,
      label: "Coracias caudatus (Lilac-breasted Roller)",
      score: 0.01953125
    }
  ]
  ```

## v0.1.7 (2023-03-22)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.1.7) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.1.7)

### Breaking Changes
- Deprecated `TFLiteElixir.Interpreter.allocate_tensors!/1`
- Deprecated Access behaviour for `TFLiteElixir.FlatBufferModel`

### Fixed
- Properly implemented `TFLiteElixir.FlatBufferModel.read_all_metadata/1`.

  ```elixir
  iex> filename = Path.join([__DIR__, "test", "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
  iex> %FlatBufferModel{} = model = FlatBufferModel.build_from_buffer(File.read!(filename))
  iex> TFLiteElixir.FlatBufferModel.read_all_metadata(model)
  %{
    TFLITE_METADATA: %{
      description:
        "Identify the most prominent object in the image from a known set of categories.",
      min_parser_version: "1.0.0",
      name: "ImageClassifier",
      subgraph_metadata: [
        %{
          input_tensor_metadata: [
            %{
              content: %{
                content_properties: %{color_space: "RGB"},
                content_properties_type: "ImageProperties"
              },
              description: "Input image to be classified.",
              name: "image",
              process_units: [
                %{
                  options: %{mean: [127.5], std: [127.5]},
                  options_type: "NormalizationOptions"
                }
              ],
              stats: %{max: [255.0], min: [0.0]}
            }
          ],
          output_tensor_metadata: [
            %{
              associated_files: [
                %{
                  description: "Labels for categories that the model can recognize.",
                  name: "inat_bird_labels.txt",
                  type: "TENSOR_AXIS_LABELS"
                }
              ],
              description: "Probabilities of the labels respectively.",
              name: "probability",
              stats: %{max: [255.0], min: [0.0]}
            }
          ]
        }
      ]
    },
    min_runtime_version: "1.5.0"
  }
  ```

### Changed
- Improve `TFLiteElixir.TFLiteTensor.to_nx/2` (#33) by @cocoa-xu
- [doc] Improve doc for to_nx (#31) by @mnishiguchi

### Added
- Implemented 
  - `FlatBufferModel.{list_associated_files/1,get_associated_file/2}`
  - `TFLiteElixir.Interpreter.signature_keys/1`
  - `TFLiteElixir.Interpreter.execution_plan/1`
  - `TFLiteElixir.Interpreter.new_from_buffer/1`
  - `TFLiteElixir.Interpreter.tensors_size/1`
  - `TFLiteElixir.Interpreter.variables/1`
  - `TFLiteElixir.Interpreter.set_variables/2`
  - `TFLiteElixir.Interpreter.set_inputs/2`
  - `TFLiteElixir.Interpreter.set_outputs/2`
- [example] object detection example (#40) by @mnishiguchi

## v0.1.6 (2023-03-19)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.1.6) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.1.6)

### Fixed
- [edgetpu] Improved edgetpu context handling, and bumped libedgetpu_runtime_version to v0.1.5. Fixed [#30](https://github.com/cocoa-xu/tflite_beam/issues/30)

### Added
- [example] artistic-style-transfer example (#27) @mnishiguchi

## v0.1.5 (2023-03-18)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.1.5) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.1.5)

### Breaking Changes
- Deprecated functions:
  - `TFLiteElixir.FlatBufferModel.initialized!/1`
  - `TFLiteElixir.FlatBufferModel.get_minimum_runtime!/1`
  - `TFLiteElixir.TFLiteTensor.tensor!`
  - `TFLiteElixir.TFLiteTensor.to_nx!`
  - `TFLiteElixir.TFLiteTensor.to_binary!`
  - `TFLiteElixir.FlatBufferModel.build_from_buffer!`
  - `TFLiteElixir.FlatBufferModel.get_full_signature_list`
- `TFLiteElixir.Coral.get_edge_tpu_context/1` now takes keyword options.

### Changes
- [example] Improve Inference on TPU notebook (#15) @mnishiguchi
- [example] Improve Inference on TPU notebook (#16) @mnishiguchi
- Alias modules in tflite_interpreter (#17) @mnishiguchi
- Rename elixir files based on module names (#18) @mnishiguchi
- add moduledocs (#19) @mnishiguchi

### Fixed
- Fixed a few places that could lead to segmentation fault.
- [example] Fixed broken ESRGAN link, Visualize the result section in the "Super Resolution" notebook. Lock down `tflite_elixir` and `evision` version (#29) @mnishiguchi.
- [typespec] Fixed typespec for `TFLiteElixir.Coral.edge_tpu_devices/0` (#22) @mnishiguchi.

### Added
- [test] Unit tests for `TFLiteElixir.Interpreter`, `TFLiteElixir.InterpreterBuilder` and `TFLiteElixir.Ops.Builtin.BuiltinResolver`.
- [example] Added intro text to super_resolution_example. (#26) @mnishiguchi.
- `TFLiteElixir.FlatBufferModel.error_reporter/1`.
- `TFLiteElixir.FlatBufferModel.verify_and_build_from_file/2`

## v0.1.4 (2023-03-14)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.1.4) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.1.4)

### Breaking Changes
- Snake case functions (#21) @mnishiguchi

### Changes
- [example] Improve Inference on TPU notebook (#15) @mnishiguchi
- [example] Improve Inference on TPU notebook (#16) @mnishiguchi
- Alias modules in tflite_interpreter (#17) @mnishiguchi
- Rename elixir files based on module names (#18) @mnishiguchi
- add moduledocs (#19) @mnishiguchi

### Fixed
- Fix compilation logic when not using precompiled binaries.

### Added
- Implemented `TFLiteElixir.reset_variable_tensor/1`.
- Add support for armv6.

### Misc
- Simple workaround for cortex-a53 and cortex-a57, `vcvtaq_s32_f32`.

## v0.1.3 (2023-03-09)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.1.3) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.1.3)

### Changes
- Bump TFLite version to [v2.11.0](https://github.com/tensorflow/tensorflow/tree/v2.11.0).

## v0.1.2 (2023-03-08)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.1.2) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.1.2)

First release on hex.pm.

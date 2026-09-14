# Claustro-Topo parallel baseline

Workflow: Codex edits diagnostic code; the user runs all Lisp commands and
returns results. No automatic solves, including on loading the helper.
Changes to search behavior follow measured evidence, one factor at a time.

First run, in the Wouldwork package from the repository directory:

```lisp
(stage claustro-topo)
(ww-set *algorithm* depth-first)
(ww-set *threads* 0)
(ww-set *debug* 0)
(ww-set *randomize-search* nil)
(load "src/ww-parallel-baseline.lisp")
(run-parallel-baseline "claustro-serial-01")
```

Stage and parameter changes may compile code; those costs are outside the
measurement. Thread changes must use WW-SET: crossing the serial/parallel
boundary rebuilds synchronization-sensitive tables. Do not dynamically bind
the thread count around a solve.

Return the ordinary search summary, SBCL TIME report, replay output, and the
BASELINE RESULT block. The returned Lisp plist repeats the printed block;
only one copy is needed. Results also remain in *parallel-baseline-results*.
An interrupted/error run is not a completed baseline; send the error instead.

The helper measures the WW-SOLVE envelope (initialization, search, reporting),
not just expansion. Replay occurs afterward. CPU and allocation are process
measurements; GC time is SBCL's runtime counter, not a worker-wait measurement.
Existing parallel phase and shard reports remain available on parallel runs.
Their finalization timer is narrower than the full solve envelope.

Keep the same Lisp process, memory limit, source snapshot, and reporting
configuration across comparisons. Avoid unrelated work in that process.
Record any restart or source edit between runs. Do not run PROFILE during
baseline timing; remove any manually installed profiling instrumentation first.
No forced GC is added to the baseline.

Expected reference: complete graph/min-length search at cutoff 34; shortest
recorded depth 33; successful replay. Prior user serial reference: 99 seconds,
7,714,094 generated states, 1,877,372 cycles, 7,044,735 repeated states.
Counts can change with exploration order; they are measurements, not assertions.

After reviewing serial results, run the one-worker parallel baseline, then
two workers. Compare elapsed time AND total work, not elapsed time alone.
Further instrumentation should target the first observed scaling limitation.
The problem specification and search hot paths are unchanged in this step.

## Serial result: claustro-serial-01

User-reported completed run on SBCL 2.6.8: 99.05524 seconds wall time,
98.78125 seconds process CPU, 58,815,851,808 bytes allocated, 3.203125 seconds
GC CPU time (TIME reports 3.235 seconds GC wall time), 77,876.69 states/second.
States, cycles, duplicates and canonical symmetry counts exactly match the
prior reference. Depth 33 replay succeeded; outcome exhausted-with-solutions.

The original helper incorrectly reported SYMMETRY-PRUNED as zero by reading
the local symmetry counter. The search report correctly gives 623,614 canonical
duplicates. The helper now selects the counter the ordinary report uses.
This reporting correction does not require another serial run.

Next run in the same REPL, without restaging:

```lisp
(ww-set *threads* 1)
(load "D:/quicklisp/local-projects/wouldwork/src/ww-parallel-baseline.lisp")
(run-parallel-baseline "claustro-parallel-1-01")
```

Return task-generation messages, phase timing, worker and shard reports,
search summary, TIME report, replay success/failure, and one BASELINE RESULT
block. The per-action state listing and repeated REPL return value can be omitted.

## One-worker result: claustro-parallel-1-01

Completed with depth 33 and successful replay. Wall time 97.10532 seconds,
process CPU 96.78125 seconds, allocation 37,824,696,832 bytes, GC CPU time
2.890625 seconds (GC wall time 2.925 seconds). Generated 6,200,489 states,
5,629,269 duplicates, 425,074 canonical symmetry duplicates; throughput
63,853.234 states/second. Worker cycles reported: 987,354.

Initial splitting generated 343 tasks through depth 15 in 29 ms, contributing
2,072 generated states. No donations. Final closed entries: 570,749 across
64 shards; size skew 1.05. This measures entry distribution, not access contention.

Compared with serial: 19.6% fewer states, 18.0% lower states/second, 2.0% less
elapsed time. Work order and state mix differ; this is not an isolated locking
overhead measurement. GC and initial splitting are small in this run.

Reporting anomalies retained for follow-up: progress says four solutions while
the final report and worker count say two (progress adds the shared solution
list length to worker solution counts). Final average branching factor is 0.0;
do not use it. The printed 100% search efficiency describes phase timing, not
parallel speedup. Do not treat serial/worker cycle counts as equivalent until
their counting points have been reconciled.

Next: same REPL, no restaging or profiling, `(ww-set *threads* 2)` then
`(run-parallel-baseline "claustro-parallel-2-01")`. Keep the implementation
unchanged through this initial comparison.

## Two-worker result: claustro-parallel-2-01

User-reported complete search, depth 33, successful replay. Wall time
115.920166 seconds; process CPU 224.45313 seconds (193.63% utilization).
Generated 6,219,321 states, 990,014 worker cycles, 5,637,222 duplicates,
425,471 canonical symmetry duplicates. Throughput 53,651.76 states/second.
Allocation 37,947,876,160 bytes; GC CPU 3.703125 seconds, GC wall 3.712 seconds.
Initial generation: 343 tasks, 28 ms. Closed entries 576,797; shard size skew
1.05. Workers generated 3,024,222 and 3,193,027 states; no donations.

Versus one worker: about 0.3% more states but 19.4% more wall time and 2.32x
process CPU. Aggregate throughput fell 16.0%. Worker state totals split
48.6%/51.4%; totals do not establish equal task durations or exclude a tail.
GC wall increase is under one second, so does not explain the 18.8-second
elapsed increase. Splitting cost is negligible. Suspects include synchronization,
shared cache traffic, and hardware resource sharing; none is yet proven.

Next control: return to one worker in the same REPL and run
`(run-parallel-baseline "claustro-parallel-1-02")` after `(ww-set *threads* 1)`.
This brackets the slowdown with a repeat before introducing instrumentation.
If reproducible, instrument worker phase times and sampled synchronization
costs; do not jump to more workers or tune donation based on totals alone.

## One-worker repeat and phase probe

claustro-parallel-1-02: 96.894394 seconds wall, 96.828125 CPU,
37,823,490,288 allocated bytes, 3.003 seconds GC wall (3.0 CPU).
States/cycles/duplicates/symmetry exactly match parallel-1-01. Replay passed
at depth 33. The 0.22% timing difference supports proceeding to instrumentation.

Load src/ww-parallel-phase-probe.lisp explicitly after the baseline helper.
Run (run-parallel-phase-probe "claustro-phase-1-01") with one worker first.
It temporarily wraps worker entry, EXPAND, successor processing and queue pop;
definitions are restored on exit. Do not reload code or run another search or
PROFILE concurrently. No engine or problem changes are made by this probe.

Expansion and successor wall times are sampled every 1024 calls. Raw ticks,
sample counts, call counts and clock frequency are retained. Periodic samples
can be biased; phase estimates are diagnostic, not exact accounting. Wrapper
overhead affects all calls, so compare instrumented total time to the baseline.
Queue timing includes terminal waits and is not a pure starvation measure.
Worker duration includes queue waits and does not measure worker CPU time.
Nonzero sample counts must be verified before interpreting the measurements.
Keep post-search replay; it independently checks the path and is outside timing.

## One-worker phase result: claustro-phase-1-01

Completed at depth 33 with successful replay and identical one-worker search
counts. Wall time 97.334496 seconds, CPU 97.09375 seconds, allocation
37,823,533,168 bytes; GC wall 3.620 seconds. Observed wall time is 0.24-0.45%
above the two uninstrumented one-worker runs; this is not a precise overhead
estimate because GC and run-to-run variation also differ.

Probe intercepted 987,354 calls each to expansion and successor processing,
with 964 samples each. Sample wall totals: 76,960 and 15,098 microseconds,
respectively. Mean sampled calls: 79.83 and 15.66 microseconds; extrapolated
phase totals approximately 78.83 and 15.46 seconds, not exact accounting.
Worker wall time 97.294304 seconds. All 344 queue calls totaled 56 microseconds.
The sample suggests expansion dominates one-worker cost; it does not establish
which phase causes the two-worker slowdown. Periodic-sampling bias and GC
overlap remain possible.

Next, same REPL: `(ww-set *threads* 2)` followed by
`(run-parallel-phase-probe "claustro-phase-2-01")`. No reload or restaging.
Return TIME, BASELINE RESULT, and PHASE PROBE blocks. Compare sampled mean
call costs and queue times across worker counts before narrowing instrumentation.

## Two-worker phase result: claustro-phase-2-01

Complete, depth 33, replay passed. Wall 116.47213 seconds, process CPU
225.29688 seconds, GC wall 3.628 seconds, allocation 37,927,922,800 bytes.
Generated 6,215,508 states and 989,392 worker cycles. Runtime is close to the
uninstrumented two-worker result (115.920166 seconds).

Worker 0: 479,362 expansion/successor calls, 468 samples each; expansion
99,602 microseconds sampled, successors 8,124. Queue 3.432084 seconds.
Worker 1: 510,030 calls, 498 samples each; expansion 110,264 microseconds,
successors 8,333. Queue 29 microseconds. Each worker lifetime about 116.432 s.

Mean expansion call rises from 79.83 us at one worker to 212.82/221.41 us;
successor handling rises from 15.66 us to 17.36/16.73 us. Primary sampled
inflation is inside expansion, not closed-table successor processing.
Queue wait is modest and includes terminal waiting; do not label it all
starvation. High CPU does not distinguish spinning from memory stalls.

Next detailed probe adds optional wrappers for GET-NEW-STATES, COPY-IDB, and
PROCESS-FOLLOWUPS. These are inclusive, overlapping costs, not additive phases.
First measure one worker with :detail t, then two. Check nonzero calls/samples;
zero can mean the function was not used or interception missed compiled calls.
The broader EXPAND wrapper remains to expose effects of added instrumentation.

## One-worker detail result: claustro-detail-1-01

Complete, depth 33, successful replay; identical one-worker search counts.
Wall 98.4398 seconds, CPU 98.171875 seconds, GC wall 3.694 seconds,
allocation 37,823,436,944 bytes. Runtime is 1.1 seconds above the coarse probe
and 1.3-1.5 seconds above uninstrumented runs; variation is not isolated overhead.

Calls / samples / sampled microseconds:
- EXPAND: 987354 / 964 / 77409 (80.30 us mean).
- Successor processing: 987354 / 964 / 15073.
- GET-NEW-STATES: 1614343 / 1576 / 52626 (33.39 us mean).
- COPY-IDB: 12396834 / 12106 / 3929 (0.325 us mean).
- PROCESS-FOLLOWUPS: 6198417 / 6053 / 52967 (8.75 us mean).
- Queue: 344 calls, 78 us total; worker duration 98.393212 seconds.

All detailed wrappers intercepted calls. COPY-IDB averages below one clock
tick; treat it as a coarse indication, not precise per-call latency. Independent
periodic samples and overlapping scopes prevent subtracting estimated nested
totals to infer exclusive time. No scaling culprit identified from this run alone.

Next same REPL: `(ww-set *threads* 2)` then
`(run-parallel-phase-probe "claustro-detail-2-01" :detail t)`.

## Two-worker detail result after Lisp restart

claustro-detail-2-01: user confirmed Lisp restarted before this run.
Wall 116.56119 seconds, CPU 225.78125 seconds, GC wall 3.363 seconds.
6,223,026 states; depth 33 and successful replay. Allocation 37,967,403,136
bytes in helper. Runtime agrees closely with earlier two-worker observations.

Sample means in microseconds (worker 0 / worker 1): expansion 206.64/214.22;
state construction 108.51/102.64; followups 27.79/25.97; copying 0.330/0.348.
One-worker means were 80.30, 33.39, 8.75, and 0.325 respectively.
This localizes substantial inflation to followups within state construction.
Copying is not the leading observed contributor. These inclusive estimates
cannot be added or subtracted as exact accounting, especially across restart.

Source review: PROCESS-FOLLOWUPS invokes update callbacks, including the
PROPAGATE-CHANGES! convergence loop; its passes call the generated
PROPAGATE-CONSEQUENCES! driver. Extend detailed sampling to both entry points
to distinguish more propagation passes from more expensive passes. Verify
interception counts; callbacks can retain function objects rather than symbols.
Next run stays at two workers, reloads the helper, and uses label
claustro-propagation-2-01 with :detail t. Then compare one worker with the same
instrumentation. No production search behavior changed.

## Two-worker propagation result: claustro-propagation-2-01

Complete search with depth 33 and successful replay. Wall 118.19695 seconds,
CPU 229.25 seconds, GC wall 3.276 seconds. Generated 6,216,819 states,
989,646 worker cycles, allocation 37,932,671,328 bytes. About 1.64 seconds
above the preceding detailed two-worker run; instrumentation and variation
are not separated.

Both new wrappers intercepted calls. Worker 0 propagation: 13,208,522 calls,
12,898 samples, 85,744 sampled microseconds; passes 13,745,553 calls,
13,423 samples, 85,400 sampled microseconds. Worker 1 propagation:
12,609,610 calls, 12,314 samples, 84,788 sampled microseconds; passes
13,086,625 calls, 12,779 samples, 85,216 sampled microseconds.

Combined 25,818,132 propagation invocations and 26,832,178 passes: 1.039
passes per invocation. Mean propagation sample 6.65/6.89 us; mean pass
6.36/6.67 us. This argues against long convergence loops in this run, but
requires the corresponding one-worker counts/timings for a scaling comparison.
About 4.15 propagation invocations per worker-generated child; this is not
evidence of redundant calls without inspecting the action/update call sites.
Queue wait about 3.003 seconds on one worker, negligible on the other.

Next: same process, `(ww-set *threads* 1)` and
`(run-parallel-phase-probe "claustro-propagation-1-01" :detail t)`.
No reload or restaging. Retain this matched instrumentation before attributing
the slowdown to individual propagation passes or changing propagation behavior.

## Matched one-worker propagation result

claustro-propagation-1-01 completed with depth 33 and replay success.
Wall 103.272255 seconds; CPU 102.984375; states 6,200,489, cycles 987,354.
Propagation calls 25,748,271, samples 25,144, sampled us 60,462.
Pass calls 26,762,185, samples 26,134, sampled us 58,278.
Mean pass time 2.23 us versus 6.36-6.67 us with two workers. Both average
about 1.039 passes/invocation. Two-worker pass count is only about 0.26% higher.
Evidence favors cost per pass, not extra convergence iterations. Underlying
shared-resource cause remains unproven. Expanded instrumentation adds roughly
six seconds versus the original one-worker baseline; keep absolute overhead
and periodic sampling in mind. Pause whole solves to inspect the generated
propagation driver before adding more wrappers.

## Generated driver and update breakdown

User REPL driver body confirms this order: UPDATE-PLATE-STATUS!,
UPDATE-RECEIVER-STATUS!, UPDATE-GATE-STATUS!, UPDATE-BLOWER-STATUS!,
ENFORCE-THREAT-SAFETY!, under a local change-detection binding.
Source review: these respectively derive occupancy, beam activation, gate
control/jamming, blower status, and unsafe-location rejection. Common lower
level routines access proposition metadata and integer mappings. Source shows
synchronization in some mapping paths, but its contribution is not measured.

Optional :updates t samples the five update entry points. Use without :detail
to avoid stacking all earlier wrappers. These functions run many times, so
check overall overhead and interception counts before interpreting timings.
Run one worker first with label claustro-updates-1-01, then two with the same
probe configuration. Do not remove apparently irrelevant updates or alter
shared-table synchronization before measuring and checking their semantics.

## One-worker update result: claustro-updates-1-01

Complete, depth 33, successful replay, unchanged one-worker search counts.
Wall 115.118256 seconds, CPU 114.953125 seconds, GC wall 3.767 seconds.
All five updates: 26,762,185 intercepted calls and 26,134 samples apiece.
Sampled microseconds: plate 10,934; receiver 28,483; gate 28,056;
blower 4,377; threat 4,988. Corresponding means about 0.418, 1.090,
1.074, 0.167, 0.191 us. Receiver and gate dominate sampled update time.
Sub-microsecond values are below individual timer resolution and include
measurement effects; do not interpret as precise production costs.

This wrapper configuration is intrusive: about 18 seconds above the original
one-worker baseline, across roughly 134 million wrapped update calls. Keep
the matching two-worker measurement diagnostic, not a speedup benchmark.
Zero detail counters are expected because :detail was omitted. Next run:
`(ww-set *threads* 2)` then
`(run-parallel-phase-probe "claustro-updates-2-01" :updates t)`.
After this comparison, prefer narrowly scoped probes over further all-update
wrappers; do not use these absolute timings to estimate uninstrumented speedup.

## Two-worker update result: claustro-updates-2-01

Complete, depth 33, replay passed. Wall 123.75927 seconds, CPU 239.25,
GC wall 3.477; 6,227,217 states, 991,099 cycles. Five update call counts
13,031,072 / 13,856,010 per worker, samples 12,725 / 13,531 each.
Sampled us by worker: plate 7487/7893; receiver 41019/42085;
gate 37476/38296; blower 2116/2314; threat 2601/2831.
Receiver mean 3.11-3.22 us versus 1.09 serial-worker; gate 2.83-2.95
versus 1.07. Plate 0.58-0.59 versus 0.42; blower/threat largely unchanged.
Receiver and gate dominate observed inflation. Broad wrapper overhead remains
material; use these as diagnostic samples, not a production speedup comparison.

Source review identifies shared static fact and metadata tables as candidates:
gate CONTROL-ON reads static controller wiring; receiver queries read static
beam apparatus/geometry. Several shared tables are declared synchronized in
parallel mode. Verify live synchronization flags before any controlled change;
declaration defaults alone do not establish live settings or contention.

User live-table inspection: STATIC-IDB synchronized T/1221 entries;
STATIC-DB NIL/1201; RELATIONS T/16; STATIC-RELATIONS T/84;
CONSTANT-INTEGERS NIL/148; INTEGER-CONSTANTS T/148; PROP-KEY-CACHE T/0.
These flags identify candidates, not measured contention. REGISTER-DYNAMIC-OBJECT
can write STATIC-IDB, so no production synchronization change is justified yet.

Next: explicitly load src/ww-table-read-probe.lisp and run
(run-static-table-read-probe). No solve or planner-table mutation. It copies
STATIC-IDB into synchronized/unsynchronized tables of matched settings, then
performs fixed total successful key lookups using one/two workers. Two rounds
reverse case order. Thread startup is timed in all cases; no synchronized start
barrier is imposed. This artificial read workload tests the mechanism, not
end-to-end speedup or safe removal of synchronization from production tables.

## 2026-09-13: read-path/lifetime audit and controlled lookup probe

Source inspection only; no Lisp was launched. Working tree initially contained
only the five untracked diagnostic/handoff files listed in the handoff; preserved.
The baseline and phase helpers remain unchanged.

Read paths:
- ww-translator.lisp routes static predicates to STATIC-DB; SUBST-INT-CODE
  (ww-converter.lisp) rewrites generated list-key GETHASH forms to STATIC-IDB.
  COMPILE-ALL-FUNCTIONS applies this rewrite to stored query/update lambdas.
- CONVERT-PROP-LIST folds literal object codes during generation, but emits
  CONSTANT-INTEGERS lookups for variable arguments before integer arithmetic.
  Thus a final static lookup is not the complete generated query cost.
- CONTROL-ON reads CONTROLS wiring then calls ENERGIZED for each controller.
  UPDATE-RECEIVER-STATUS! calls BEAM-REACHES-RECEIVER and its direct/relay hooks.
  The generated forms in the user's image still need inspection; source
  translation does not establish the frequency of each live table access.

Ownership/lifetime:
- These tables are shared globals, not worker-owned snapshots. Problem reset in
  ww-preliminaries.lisp recreates synchronization-sensitive tables. Installation
  populates relation metadata; DO-INTEGER-CONVERSION populates integer mappings,
  static facts and compiled functions. Conversion explicitly permits repeated use.
- REGISTER-DYNAMIC-OBJECT writes both constant maps and STATIC-IDB. Its only
  technology call found is in -beam-crossing-coordinates.lisp. Claustro-Topo
  explicitly includes beam-direct, not beam-crossing; this narrows the suspected
  path but is not a proof of an immutable search lifetime.
- CONVERT-TO-INTEGER and CONVERT-FLUENTLESS-PROP-TO-INTEGER can allocate missing
  object codes at runtime. Their fallback writes both maps under INTEGER-LOCK,
  while initial reads of CONSTANT-INTEGERS occur outside that lock. Do not infer
  general concurrent-read safety from the registration function's docstring.
- CONVERT-TO-INTEGER-MEMOIZED writes PROP-KEY-CACHE on misses; initialization
  and search startup clear it. An observed zero count is not a lifetime contract.
- No production synchronization was changed. Still unresolved: a complete
  reachable-writer audit for the staged image, including mutation of table values
  and publication/reset boundaries. A private benchmark snapshot proves none of
  those production properties.

V2 probe replaces the short trial in src/ww-table-read-probe.lisp. It uses a
locally declared speed 3 / safety 1 / debug 1 lookup kernel, per-worker warmup,
semaphore readiness/start coordination, and calibration by doubling even passes
until the plain one-worker control lasts at least one second. All measured cases
then use that same total work. Three rounds rotate/reverse six cases: one worker,
two sharing a table, and two with separate tables, for both synchronization flags.
Copies are made from the same source with matching hash settings and insertion
order. Separate copies isolate table sharing, but still share key/value objects
and the key vector. All workers scan the same key order; no distribution claim.

Thread creation/warmup/copying are excluded. Wall time includes release, wakeup
and join; raw worker start/end ticks expose start skew and active duration.
Process CPU includes any other activity in the Lisp process. Successful-hit
counts are asserted. Worker errors are returned to the caller and cleanup releases
waiting workers and joins created threads. Calibration and timings are not yet
user-tested; static delimiter checking cannot establish Lisp compilation/runtime
correctness. Loading the source runs no benchmark. Explicitly compile the kernel
in the REPL before running, to avoid depending on LOAD compilation behavior.

Hardware query via CIM was denied by the environment; no SBCL process affinity
was obtained. The benchmark records a user context string and does not change
placement. Physical-core vs SMT placement, CPU frequency and background load
remain uncontrolled. Give known processor/affinity context if available, otherwise
retain the explicit unknown default. Do not infer a locking-only cause even if
separate synchronized tables scale better.

Next REPL commands (existing staged image, idle, no PROFILE):

```lisp
(load "D:/quicklisp/local-projects/wouldwork/src/ww-table-read-probe.lisp")
(compile 'table-read-probe-loop)
(dolist (name '(control-on energized update-gate-status!
                update-receiver-status! direct-beam-reaches-receiver))
  (format t "~&GENERATED ~S~%" name)
  (pprint (subst-int-code (symbol-value name))))
(run-static-table-read-probe :target-seconds 1d0 :rounds 3)
```

Return warnings/errors, generated forms, calibration rows, header and all 18
case rows. No WW-SET, staging, or solve is required in the existing image.
Allow several minutes: synchronized cases may be much slower than calibration.
The next decision is whether the negative scaling survives longer coordinated
trials and whether it depends on sharing the same table. Review these results
before selecting another experiment or requesting any full solve.

## 2026-09-13: user-reported V2 results

All 18 cases completed with 320,077,824 successful lookups each (262,144
passes over 1,221 keys). SBCL 2.6.8, EQL tables, size 1,305; affinity/core
placement unrecorded. Calibration doubled from 2,048 to 262,144 passes,
ending at 1.352885 seconds. Generated query forms and explicit COMPILE output
were not included in this paste; the printed policy describes the helper's
kernel declaration, not independent evidence of the user's compilation command.

| Case | Round 1 wall s | Round 2 wall s | Round 3 wall s | Median s |
| --- | ---: | ---: | ---: | ---: |
| synchronized, one worker | 6.802205 | 6.819254 | 6.793801 | 6.802205 |
| synchronized, shared, two | 27.872015 | 28.200723 | 28.501156 | 28.200723 |
| synchronized, separate, two | 3.426193 | 3.404523 | 3.411536 | 3.411536 |
| plain, one worker | 1.361848 | 1.360141 | 1.348476 | 1.360141 |
| plain, shared, two | 1.909513 | 1.905811 | 1.913018 | 1.909513 |
| plain, separate, two | 0.710437 | 0.685358 | 0.681129 | 0.685358 |

Median fixed-work ratios: shared synchronized two-worker wall time is 4.146x
one worker; shared plain is 1.404x. Separate tables yield 1.994x synchronized
and 1.985x plain speedup. Synchronization adds about 5x one-worker elapsed
cost in this kernel. These are isolated lookup ratios, not planner speedups.

Worker release-to-start delays are at most 83 microseconds; all two-worker
cases have heavily overlapping active intervals. Two-worker process CPU is
approximately twice wall time, including the slow shared cases. Startup timing
cannot plausibly account for the seconds of slowdown observed here. Separate
copies demonstrate near-ideal scaling on the available machine in these trials,
so a general inability to execute this lookup kernel in parallel is inadequate
as an explanation. Sharing the table object/backing storage is strongly
implicated for this workload, even with synchronization disabled. Keys/vector
remain shared in the separate-table cases. The precise mechanism remains open:
internal lookup metadata writes/cache coherence are candidates, not findings.
Do not label unsynchronized GETHASH a physically read-only operation without
inspecting this SBCL version's implementation. Placement/frequency still were
not controlled and copy identity/layout are not independently crossed over.

Next recommended step: inspect local SBCL 2.6.8 GETHASH implementation and/or
user-returned disassembly for internal writes/lookup caching, and obtain the
previously requested generated query forms. This is more discriminating than
another solve or merely longer repetition of V2. Keep the production ownership
and mutation audit separate; results do not authorize disabling synchronization
or introducing worker snapshots into the planner. No diagnostic code changes
or Lisp execution were made while interpreting this report.

## 2026-09-13: SBCL lookup cache identified in matching upstream source

The installed SBCL directory contains binaries/contribs but no implementation
source. Retrieved the official sbcl-2.6.8 tagged target-hash-table.lisp using
HTTPS for inspection (no Lisp execution). Source:
https://github.com/sbcl/sbcl/blob/sbcl-2.6.8/src/code/target-hash-table.lisp

DEFINE-HT-GETTER, lines 2289-2388, first checks HASH-TABLE-CACHE and returns
immediately for a cached EQ key match. On a successful chain search it writes
KEY-INDEX to HASH-TABLE-CACHE (line 2358). GETHASH/EQL-HASH uses this macro
(line 2549). MAKE-SYNCHRONIZED-TABLE-METHODS wraps the getter in a recursive
system lock (2599-2635). EQL selection also has a flat-table implementation
(2702-2709); therefore do not assume the ordinary getter is the active one
without inspecting the user's actual table. This source is version-matched
upstream evidence, not verification of the installed binary's exact build.

The shared cache store is a concrete candidate for the unsynchronized penalty:
logical fact reads can write implementation metadata, causing coherence traffic
between cores. Separate tables separate that cache too. Source plus V2 strongly
supports this mechanism, but does not measure its exclusive contribution or
prove a production benefit. Neither locks nor cache stores were modified.

Added INSPECT-STATIC-TABLE-READ-PATH to the existing helper. It reports live
getter identities and synchronization flags for six relevant tables, then
observes cache-before/after around 16 successful lookups on each of two private
STATIC-IDB copies (plain/synchronized), disassembles their selected getters,
and prints five generated query/update forms. It reads SBCL internal accessors
verified in the matching source; this is a version-specific diagnostic, not
production API. The helper is statically checked, awaiting user compilation/run.

Next, existing idle Wouldwork REPL:

```lisp
(load "D:/quicklisp/local-projects/wouldwork/src/ww-table-read-probe.lisp")
(inspect-static-table-read-path)
```

Return the whole report or first error. No benchmark or solve is needed. The
report will verify whether the installed getters exhibit the cache write and
show generated static/mapping accesses before deciding on a further experiment.

## 2026-09-13: installed lookup cache writes confirmed by user

INSPECT-STATIC-TABLE-READ-PATH output confirms the plain private STATIC-IDB
copy selects SB-IMPL::GETHASH/EQL-HASH. Sixteen successful lookups change the
cache slot through 2,4,6,8,10,12,14,16 twice. The synchronized private copy
exhibits the same transitions. Installed EQL disassembly shows the successful
lookup store at offset 588, MOV [R15+45], RAX; the entry reads the same slot
through RSI at offset 2D8. This verifies physical metadata writes in the
installed lookup implementation, not just matching upstream source.

Live STATIC-IDB and INTEGER-CONSTANTS select GETHASH/LOCK; RELATIONS and
STATIC-RELATIONS select GETHASH/STATE/LOCK. CONSTANT-INTEGERS selects
GETHASH/EQUAL without synchronization. FLUENT-RELATION-INDICES selects
GETHASH/EQ-HASH/FLAT without synchronization. The synchronized private getter's
disassembly explicitly calls CALL-WITH-RECURSIVE-SYSTEM-LOCK. Do not generalize
the ordinary EQL cache behavior to the flat getter. The constant-map EQUAL
getter remains a candidate, not separately timed or cache-observed here.

Combined with V2's shared/separate scaling, cache coherence is now a concrete,
strongly supported explanation for the plain shared-table penalty, but its
exclusive timing contribution is not isolated. Production cache writes were
not directly sampled; private copies and production getter identities are the
evidence. No production table entries, synchronization or getters were changed.

The pasted report ends exactly at GENERATED CONTROL-ON. No generated form,
error, debugger text or final REPL prompt follows. Therefore the generated-query
portion is NOT confirmed complete. Next obtain the remainder of that output or
the first error, without rerunning timings/disassembly. Do not infer whether it
was truncated, interrupted, or failed. No new solve is justified by this report.

## 2026-09-13: generated forms received; read paths confirmed

User supplied the remainder of the report: CONTROL-ON, ENERGIZED,
UPDATE-GATE-STATUS!, UPDATE-RECEIVER-STATUS!, and
DIRECT-BEAM-REACHES-RECEIVER. The previously missing generated-form output
is now available; no error was reported.

CONTROL-ON performs one CONSTANT-INTEGERS lookup to encode the device and
one STATIC-IDB lookup for its controller wiring. If wiring exists, it calls
ENERGIZED under short-circuit OR-of-AND controller loops. Each ENERGIZED
invocation performs one or two static type tests, depending on short-circuit
results; each test re-reads the controller code from CONSTANT-INTEGERS.
Each reached dynamic status test additionally reads that code and state IDB.
It does not read RELATIONS or STATIC-RELATIONS in this generated body.

UPDATE-GATE-STATUS! enumerates nine literal gates. For each it checks up to
two jammers via state IDB (one constant-code lookup per jammer), calls
CONTROL-ON only if no jammer matched, and encodes the gate again for the
OPEN add/delete. Gate enumeration itself uses a literal list, not a type-table
lookup. Static controller/type tests are therefore nested in the update,
not evidence of relation metadata iteration on every gate.

UPDATE-RECEIVER-STATUS! enumerates the single literal receiver, calls
BEAM-REACHES-RECEIVER and encodes the receiver for ACTIVE add/delete.
DIRECT-BEAM-REACHES-RECEIVER enumerates one literal transmitter. If the tests
are reached, its visible prefix performs three STATIC-IDB reads and four
CONSTANT-INTEGERS reads: transmitter/receiver pair plus both chromas. It then
calls FIXED-BEAM-CORRIDOR-CLEAR and BEAM-CUT, whose generated bodies are not
in this report. Prefix counts are conditional, not per-pass measured totals.

This links the observed hot updates to both shared STATIC-IDB (locked) and
CONSTANT-INTEGERS (unlocked EQUAL getter). Unsynchronized constant-code reads
must not be assumed free of implementation cache traffic. No runtime access
counts or per-table causal attribution have been measured. Source-level
repetition alone does not establish the best optimization.

Recommended next bounded diagnostic: replay a fixed collection of already
validated states through the gate/receiver updates using diagnostic-only
compiled variants with explicit private table arguments. Cross shared/private
STATIC-IDB and shared/private CONSTANT-INTEGERS independently while keeping
synchronization flags unchanged; copy mutable state outside each timed batch
and compare resulting state facts/change flags against original functions.
Before implementing, inspect the full reachable query closure and reject any
writer to the proposed snapshot tables. These tables are DEFGLOBALs: ordinary
LET binding is not an appropriate worker-local substitution mechanism. Avoid
production getter patching or a full solve. This is a proposed diagnostic,
not implemented or validated by receipt of these forms.

## 2026-09-13: gate factorial diagnostic prepared (not Lisp-tested)

Added src/ww-gate-table-probe.lisp. Scope is deliberately gate-only for the
first actual-update experiment. Receiver closure inspection found additional
memoized helpers: VERTICAL-TYPE-ENTRY writes VERTICAL-TYPE-CACHE on a miss;
LOCATION-ELEVATION-VALUE writes LOCATION-ELEVATION-CACHE and calls the staged
LOCATION-LEVEL function indirectly. These are in tech/-vertical.lisp. Receiver
snapshot coverage therefore requires more than replacing two globals in its
immediate generated body. No receiver timing is claimed or requested yet.

The gate closure is UPDATE-GATE-STATUS!, CONTROL-ON, ENERGIZED, plus state IDB
access and ADD/DEL-INT-PROP-KEY. Source audit of those mutation helpers shows
that with hash accumulators dynamically NIL they only maintain the supplied
state IDB and the dynamically bound propagated-change flag. No object-code
registration/conversion fallback occurs in these generated gate forms. Printing
must be off. The clone preparation rejects additional WW function dependencies.
This guard complements inspection; it is not a general Lisp effect checker.

The diagnostic compiles three functions under fresh uninterned names, passes
static/code tables explicitly, redirects their mutual calls, and cleans up their
function bindings on exit. Quoted data remains unchanged. Production functions
are neither replaced nor rebound, and DEFGLOBAL tables are not LET-bound.
Copies preserve the original synchronization flags. Both one-worker and
shared two-worker cases use snapshots, not production tables, during timing.

Uses copies of existing start and depth-33 solution endpoint states plus two
synthetic copies with OPEN facts inverted. These synthetic inputs are update
checks, not validated reachable search states. It compares two successive
original/clone updates on each input for equal state IDB and propagated-change
flags, before timing each case. After timing it checks state IDBs against the
original update result and checks snapshot table contents against production.
Hash folding is disabled consistently; this is not a full effect-state test.

Warmup settles the four input states; measured repetitions are settled gate
updates. This deliberately measures repeated gate decision reads, not transition
frequency or the full expansion distribution. No path replay or solve is invoked;
the existing retained solution endpoint is required. A missing endpoint causes
an assertion, not an automatic solve. Preparation/state copies/validation are
outside timing. Successful fixed work is reported in the shared timer's LOOKUPS
field, explicitly tagged :UNIT :GATE-UPDATE-CALLS (not actual hash lookup count).

The existing table timer gained an optional KERNEL argument with its previous
lookup kernel as default. Worker warmup and timed calls use that same kernel.
One-worker calibration doubles even pass counts until >=1 second. Three rotated/
reversed rounds then run five cases with identical total gate-update calls:
one worker; two sharing both; private static only; private code map only; both
private. Table identity/allocation and unrecorded core placement remain limitations.
No production synchronization settings change. Delimiter checks passed for both
helper files; compilation, correctness assertions and timings await user REPL.

Next commands in existing idle WW image, no PROFILE or concurrent work:

```lisp
(load "D:/quicklisp/local-projects/wouldwork/src/ww-table-read-probe.lisp")
(load "D:/quicklisp/local-projects/wouldwork/src/ww-gate-table-probe.lisp")
(compile 'gate-probe-kernel)
(run-gate-table-probe :target-seconds 1d0 :rounds 3)
```

Return compiler warnings/errors, calibration, header, and all 15 GATE RESULT
rows. On an error return it; do not stage or solve to bypass an assertion.
The question is whether independent table sharing affects actual gate-update
cost while lock policy stays fixed. Review that before extending to receiver
helpers or any full-solve experiment.

## Gate diagnostic prerequisite correction

User's first RUN-GATE-TABLE-PROBE stopped at (ASSERT *SOLUTION-PATHS*) in
GATE-PROBE-SEEDS. No timing or clone compilation was reached. A retained
solution is unnecessary for the sharing comparison, so the helper now always
uses the staged start state and includes a retained depth-33 endpoint only if
available. Each input still gets a synthetic inverted-OPEN counterpart.
The header reports :SEED-SOURCE :START-ONLY (two inputs) or
:START-AND-SOLUTION (four inputs). Start-only coverage is narrower; timings
from different corpora must not be treated as matched comparisons. Correctness
checks are unchanged. This supersedes the earlier mandatory-endpoint requirement.

Exit this debugger with ABORT (restart 1), reload ww-gate-table-probe.lisp,
compile GATE-PROBE-KERNEL again, and rerun the same benchmark command. No solve,
restaging or production change required. Correction is statically checked only;
all Lisp execution remains with the user.

## 2026-09-13: gate factorial results, start-only corpus

User completed all 15 cases. Every case reports verification passed, covering
pre-timing original/clone IDB and change-flag comparisons, final IDB comparisons,
and snapshot table-content checks. Fixed work: 1,048,576 gate updates per case;
524,288 passes over two start-derived inputs. Warmup settles both inputs, so
the synthetic OPEN inversion extends correctness coverage, not timed state
variety. STATIC synchronization T, code-map synchronization NIL, hash folding
NIL, SBCL 2.6.8. These are diagnostic clones with explicit table arguments,
not timings of the original production functions or search.

| Workers | Private static | Private codes | Round 1 s | Round 2 s | Round 3 s | Median s |
| ---: | --- | --- | ---: | ---: | ---: | ---: |
| 1 | no | no | 1.106171 | 1.103990 | 1.122357 | 1.106171 |
| 2 | no | no | 2.645354 | 2.766397 | 2.762102 | 2.762102 |
| 2 | yes | no | 1.204582 | 1.260531 | 1.271104 | 1.260531 |
| 2 | no | yes | 2.887382 | 2.617643 | 2.521441 | 2.617643 |
| 2 | yes | yes | 0.550347 | 0.555030 | 0.554015 | 0.554015 |

Shared both is 2.497x slower than one worker. Private both gives 1.997x
speedup over one worker, and 4.986x over shared-both two workers. Private static
alone removes much of the slowdown but remains slower than one worker. Once
static is private, making codes private reduces median time another 2.275x.
Codes-only does not consistently improve over shared both across rounds; their
ranges overlap. This demonstrates interacting costs, not additive percentages
attributable independently to each table. In this gate workload, preserving
locks while separating both tables is sufficient to recover nearly ideal scaling.

The 32 compiler messages are optimization NOTES about unknown lookup-result
numeric types and generic arithmetic, not compilation failures. They apply to
the same cloned functions in all factorial cases and are outside timing. Do not
add numeric declarations mid-comparison or infer their cost from compiler cost
labels. Their presence reinforces that absolute clone timings are not a proven
production-equivalent baseline.

This is stronger evidence than raw lookups: actual generated gate decision
logic reproduces the sharing penalty with successful correctness checks.
It does not establish a fivefold full-solve improvement, safe general snapshot
ownership, receiver behavior, or coverage of the searched state distribution.
No production implementation changes were made to obtain or interpret results.

Recommended next: complete receiver helper/cache write-lifetime audit and prepare
an isolated receiver comparison that includes every relevant shared read/cache
path explicitly. No further gate repetition or full solve is currently needed.
Only after those measurements and safe publication/mutation rules are established
should a narrowly scoped production experiment be proposed.

## Receiver audit and diagnostic prepared (awaiting user Lisp execution)

Source inspected: tech/beam-direct.lisp, -beam-substrate.lisp, visibility.lisp,
-vertical.lisp, -gate.lisp, -recording-shadow-policy.lisp, -beam-occlusion.lisp,
and -beam-los-coordinates.lisp. Receiver propagation follows the direct corridor,
recorded barrier elevations and authored location occlusion; BASE/TOP are mutually
recursive. Claustro's relay and beam-cut hooks are neutral. The generated closure
is discovered from the user's staged forms and printed; names outside the audited
set, unknown ordinary WW calls and indirect FUNCALL/APPLY paths stop preparation.
This is a scoped guard, not a general Lisp effect analysis.

Clarification of the previous wider-cache concern: current BASE calls FIXED-BASE
directly for locations, not LOCATION-ELEVATION. LOCATION-ELEVATION-VALUE's memo
and indirect LOCATION-LEVEL call are therefore not admitted by this diagnostic.
If the live closure reaches them, preparation fails rather than silently leaving
that path shared. VERTICAL-TYPE-ENTRY and VERTICAL-AXIS-P are admitted ordinary
helpers: the former reads a per-object memo, resolving misses through TYPES and
VERTICAL-TYPE-CONSTANTS. BEAM-COORDINATES-PROJECTION-PARAMETER is pure arithmetic.

VERTICAL-TYPE-CACHE and LOCATION-ELEVATION-CACHE are DEFPARAMETER tables reset
on technology reload; both are unsynchronized EQ tables with lazy writes. Their
contracts describe instance-static type membership and location levels, but no
source-backed rule ensures all worker lookups are prefilled before production
search. This remains a production ownership issue; nothing was changed there.
The new experiment binds only VERTICAL-TYPE-CACHE (a special variable) to a
prefilled private table in each worker/reference check, outside timed setup.
All vertical objects are prefilled through the original helper. The post-run
check rejects a change in memo contents. It does NOT measure production sharing
of this memo; keeping it private is an explicit control for the two-table test.

Added src/ww-receiver-table-probe.lisp. Like the gate experiment it compiles
uninterned query clones with explicit STATIC-IDB/CONSTANT-INTEGERS parameters,
redirects recursive query calls, leaves production definitions untouched and
unbinds clone functions on exit. It uses the existing table timer; gate code
was not changed. Static and code snapshots retain synchronization settings.
A one-worker reference and four two-worker sharing combinations use fixed work,
calibration, warmup, three rotated/reversed rounds and raw worker timings.

Inputs are four copies derived from the staged start state: all gates closed,
all gates open, and corresponding ACTIVE-inverted copies. These are synthetic
update inputs, not claims of reachable puzzle states. They require no retained
solution. Original receiver ACTIVE results are printed before timing to expose
whether the chosen corpus actually covers both outcomes. ACTIVE inversions
extend change-flag checking but converge under warmup. Timing is settled receiver
updates with hash folding off, not search or propagation convergence.

Every case compares original/clone IDB and change flags for two passes, checks
final state IDBs, checks static/code snapshot contents and vertical memo contents.
Reference checks also use a private vertical memo. Compilation, copying, prefill
and verification are outside timings. Delimiter review passed; no Lisp was run.

Next existing idle WW REPL:

```lisp
(load "D:/quicklisp/local-projects/wouldwork/src/ww-table-read-probe.lisp")
(load "D:/quicklisp/local-projects/wouldwork/src/ww-receiver-table-probe.lisp")
(compile 'receiver-probe-kernel)
(run-receiver-table-probe :target-seconds 1d0 :rounds 3)
```

Return errors/warnings, RECEIVER CLOSURE, seed ACTIVE results, calibration,
header and 15 RECEIVER RESULT rows. Optimization notes may be omitted if no
warnings/errors occur. No solve or production synchronization change requested.

## Receiver factorial results received

All 15 user cases report VERIFICATION PASSED. Fixed total work is 2,097,152
receiver updates, 524,288 passes over four synthetic inputs. Seed ACTIVE results
(NIL T NIL T) confirm both blocked and active outcomes; ACTIVE-inverted partners
converge during warmup. STATIC synchronization T, code-map synchronization NIL,
vertical memo private/prefilled in every case, hash folding NIL, SBCL 2.6.8.

| Workers | Private static | Private codes | Round 1 s | Round 2 s | Round 3 s | Median s |
| ---: | --- | --- | ---: | ---: | ---: | ---: |
| 1 | no | no | 1.726698 | 1.731391 | 1.729518 | 1.729518 |
| 2 | no | no | 4.713612 | 5.024951 | 4.838301 | 4.838301 |
| 2 | yes | no | 1.813662 | 1.818136 | 2.148568 | 1.818136 |
| 2 | no | yes | 5.106949 | 5.264112 | 4.699381 | 5.106949 |
| 2 | yes | yes | 0.863636 | 0.878458 | 0.869065 | 0.869065 |

Shared both gives 2.797x one-worker elapsed time. Both private gives 1.990x
speedup over one worker and 5.567x versus shared-both two workers. Static-only
privacy removes much of the slowdown but does not yield parallel speedup;
codes-only is not consistently better than shared both (median is worse).
The interaction matches the gate result: both tables must be separated in
these tested configurations to recover near-ideal scaling. This is an observed
sufficiency for these workloads, not a proof that snapshots are the only remedy.

Pasted STYLE-WARNINGs are for unused PROBE-STATIC/PROBE-CODES arguments in
clones that do not require both tables (e.g. neutral hooks); these are harmless
interface artifacts, not correctness failures. No helper edits or rerun are
needed to interpret the results. The pasted report does not include RECEIVER
CLOSURE, so the exact printed live closure is not retained here. Successful
completion implies the helper's closure guard accepted the staged functions.

The receiver and gate experiments now independently connect the sampled hot
updates to shared fact/code-table costs. Do not extrapolate their fivefold
shared/private ratios to a full solve: corpus, clone calling convention, hash
folding and receiver memo ownership differ from production. In particular the
receiver experiment deliberately does not measure the vertical cache's shared
production behavior. General snapshot validity during dynamic registration and
problem reset remains unresolved.

Recommended next: prepare a concrete ownership/publication design and narrow
opt-in integration proposal, with explicit handling of object-code growth,
static-fact writes, mutable values and memo caches. Preserve synchronization;
no production implementation or full solve is authorized by the benchmark
result alone. Further repetitions of these isolated benchmarks are not needed
without a new unresolved question. A later full-solve comparison should answer
whether the approved integration improves end-to-end runtime while preserving
work accounting and depth-33 replay.

## Ownership/integration proposal prepared

See parallel-snapshot-proposal.md for the opt-in Claustro-only design. Publication
must follow GENERATE-ROOT-TASKS, with explicit frozen lifetime and worker-owned
read views for generated static/code reads. Preserve synchronization. Writer
inventory includes both runtime code-allocation paths, static conversion and
supported reset/installation paths. Existing worker cleanup begins after thread
creation and needs to cover partial startup before a frozen lifetime is sound.
Structured static values/key semantics and complete worker-path mutation audit
remain prerequisites, not established facts. Proposal includes private vertical/
location memos, focused lifecycle/hash correctness tests, and a separately
approved full-solve comparison. No production code or Lisp execution this step.


## 2026-09-13: approved opt-in implementation, focused tests ready

The user approved implementation, explicitly retaining the user-only Lisp
workflow and separate full-solve authorization. Initial status contained eight
untracked investigation files and no tracked edits; all are preserved. No Lisp
or solve was launched. The new implementation and complete bounded worker-path
and ownership audit are in parallel-snapshot-audit.md; the proposal now records
approval and the implementation resolution.

Source audit found two additional unsynchronized lazy movement writers:
MOBILITY-ROUTE-KEYS and TRAVERSAL-CANONICAL-FAMILIES. They receive fresh private
worker tables, as do the proposed vertical/location memos. Synchronized
traversal caches remain shared with their existing read-only payload contract.
No synchronization flag was disabled. Structured key/payload copying has an
explicit finite cons/string grammar; identity-sensitive/unsupported data stops
preparation. Worker read views are published after root generation and are
never reused across worker groups. Missing-code and static/lifecycle writers
signal before table/index mutation; teardown covers preparation failure,
partial startup, worker error, coordinator error/nonlocal exit and cancellation.

Actual generated read routing now has selection overhead even with the option
off. Static-update guards also run in ordinary update paths. Any later on/off
performance comparison must use the same rebuilt code and include snapshot
setup, verification, cleanup and cold private memos. The isolated near-2x
results do not establish this integration's end-to-end performance or safety.
The baseline helper now includes *worker-read-snapshots* in its captured settings;
no timing run is requested yet.

Focused commands in an idle WW REPL, with no PROFILE or overlapping work:

```lisp
(asdf:load-system :wouldwork :force t)
(in-package :ww)
(stage claustro-topo)
(ww-set *threads* 2)
(load "D:/quicklisp/local-projects/wouldwork/src/ww-worker-read-snapshot-tests.lisp")
(run-worker-read-snapshot-tests)
```

The helper temporarily enables its experiment scope and returns with the user's
setting unchanged (NIL after STAGE). It expands only the start and at most six
first-level children, never calls SOLVE/DFS/root-task generation, and uses
bounded worker harness tasks for lifecycle tests. It checks standard and
canonical hash folding, then concurrent production successor generation.
It does not validate a complete solution. Its synthetic update seeds are not
asserted reachable. Expected output includes two SNAPSHOT READS rows, one
SNAPSHOT CONCURRENT READS row, 15 SNAPSHOT LIFECYCLE rows (off/on plus preparation
failure), and SNAPSHOT FOCUSED TESTS PASS. No timing comparison is requested.

If that passes, validate reset and a fresh second run:

```lisp
(stage claustro-topo)
(ww-set *threads* 2)
(load "D:/quicklisp/local-projects/wouldwork/src/ww-worker-read-snapshot-tests.lisp")
(assert (not *worker-read-snapshots*))
(assert (not *worker-read-phase*))
(assert (not *parallel-search-active*))
(run-worker-read-snapshot-tests)
```

Return any compilation warning/error and all SNAPSHOT result lines from both
runs, or stop at the first failure and return its condition/backtrace. Static
delimiter/list-structure and diff review cannot substitute for these Lisp tests.
Do not stage or solve as a workaround for a failed assertion. If all focused
checks pass, the next decision is a separately authorized uninstrumented
one/two-worker on/off full-search comparison, with the specific question of
net end-to-end benefit and depth-33 replay/work preservation. No full solve is
requested or authorized here.


## User compile failure: CRLF repair (2026-09-13)

The user's WW-RESET stopped compiling ww-searcher.lisp with FORMAT errors:
"Unknown directive (character: Return)" in INITIALIZE-HYBRID-MODE. Byte inspection
confirmed my continuation edits had rewritten seven tracked Lisp files with
CRLF endings although HEAD uses LF. The affected FORMAT continuation is
literally tilde, carriage return, newline. This is an editing regression, not
evidence of accumulated instrumentation or an unsafe snapshot read.

Restored LF bytes in all affected edited Lisp files while preserving their
source changes, and checked the other implementation/edited diagnostic sources
for carriage returns. Earlier delimiter/diff checks did not detect this issue;
byte-level line-ending inspection is now included in this investigation's
static checks. No Lisp compilation or execution was performed by Codex.

Keep the diagnostic files and evidence. They are explicitly loaded helpers,
not ASDF components; the phase probe restores wrapped functions on unwind.
A fresh SBCL image is useful to exclude lingering interactive instrumentation,
but a source revert is not required for this failure. WW-RESET resets staged
problem/settings and reloads defaults; it does not restore Git files or remove
all arbitrary REPL definitions. Abort the failed compilation rather than ACCEPT
it, then rebuild corrected source before resuming the focused sequence above.


## First focused snapshot run passed (user report)

The user returned RUN-WORKER-READ-SNAPSHOT-TESTS completing with T and
SNAPSHOT FOCUSED TESTS PASS. Both read-view comparisons report three seeds,
ten successors and canonical hashing. The concurrent read comparison passed,
including unchanged canonical sources for private memos and hash checks.
The helper also executes its standard-hash update checks, copy/key admission,
compiled routing, missing-code/fact, writer-guard and early-exit assertions
before reporting overall success; those checks have no separate printed rows.

All 15 lifecycle rows passed: NORMAL, WORKER-ERROR, PARTIAL-START,
COORDINATOR-ERROR, NONLOCAL-EXIT, CANCEL and a second NORMAL group, each with
snapshots disabled and enabled; plus enabled PREPARATION-ERROR. Each normal or
error/cancellation group joined two workers, partial startup joined one, and
preparation failure started/joined zero. No solve was run. This establishes the
reported bounded checks, not full worker DFS coverage, full-search speedup or
general production snapshot safety. No source changes or rerun of isolated
benchmarks is needed to interpret this result.

Only one run is reported; a fresh STAGE followed by the default-off/freeze/active
assertions and repeated focused test remains pending. Use the second command
block above. After that passes, review a separately authorized end-to-end
one/two-worker on/off comparison with setup/cleanup included and depth-33 replay.
No full solve is requested yet. The previous CRLF compile blocker is superseded
by this successful user test execution; the precise rebuild/restart sequence
was not included in the report.


## Second focused run and reset assertions passed (user report)

Following the requested restage/repeat sequence, the user returned successful
ASSERT results for NIL WORKER-READ-SNAPSHOTS, WORKER-READ-PHASE and
PARALLEL-SEARCH-ACTIVE. ASSERT returns NIL on success; these are not failures.
The staging commands themselves were not included in the paste.

The complete focused suite again returned T. Both SNAPSHOT READS rows report
three seeds, ten successors and canonical hashing. Concurrent reads/private
memos/hashes passed. All 15 lifecycle cases passed with the same joined counts:
two normally, one after partial startup, zero after preparation failure. The
unprinted ownership, compiled routing, writer guards, standard-hash updates and
early-exit checks also completed. No solve was run. Two successful bounded runs
and the clear-state assertions satisfy the focused validation gate; they do not
establish full-search speedup, depth-33 replay under snapshots or general safety.

### Proposed next experiment -- separate full-solve approval pending

Question: does the complete worker ownership policy improve end-to-end
Claustro-Topo time and one-to-two-worker scaling, including snapshot preparation,
cold private memos, verification and cleanup, while preserving depth-33 replay
and comparable work accounting?

Propose four user-run, uninstrumented full solves, reviewing each result before
continuing: one worker OFF, one worker ON, two workers OFF, two workers ON. Use
the same rebuilt source and fresh Claustro staging before every case, so the
focused tests and preceding solves do not leave unequal warmed technology caches.
Configure threads via WW-SET after staging; set the snapshot flag afterward
because STAGE resets it. Keep graph/min-length, cutoff 34, symmetry on, random
off, debug 0 and all branches fixed. No phase wrappers, isolated probes or PROFILE.

Use the baseline helper, whose settings include the snapshot flag and whose
elapsed time covers WW-SOLVE; best-path replay is outside timing. Retain total
wall/CPU/allocation, generated states and worker counts, depth and replay result,
and the ordinary worker-phase report. Stop for any guard error or failed replay.
The four cases are an initial comparison, not statistical proof; repeat only if
variation or changed work leaves a specific question unresolved. No full solve
has been authorized by this successful focused-test report. Obtain explicit
approval before supplying the first execution step. No source changes this turn.


## End-to-end comparison approved, one run at a time

User approved the four-case comparison and will post each result before the next
run. First case: freshly staged Claustro-Topo, one parallel worker, snapshots OFF,
label claustro-snapshot-off-1-01. Use the uninstrumented baseline helper, all
branches, DFS graph/min-length cutoff 34, symmetry on, randomization off, debug 0.
First full-search result received below; the remaining cases are pending.


## First end-to-end case: one worker, snapshots OFF -- PASS

User reported `claustro-snapshot-off-1-01` on SBCL 2.6.8, X86-64.
Effective settings match the planned controls: DFS graph/min-length, cutoff 34,
one worker, snapshots NIL, debug 0, randomization NIL, symmetry T, branch -1.
343 root tasks were generated at depths 0-15; task generation took 30 ms.

- Outcome: :EXHAUSTED-WITH-SOLUTIONS, reason :COMPLETE.
- Wall: 100.391365 s; process CPU: 100.234375 s; GC counter: 3.0 s.
- Allocation: 37,828,027,200 bytes; throughput: 61,763.17 states/s.
- Total states: 6,200,489; cycles: 987,354; duplicates: 5,629,269.
- Canonical symmetry duplicates: 425,074; closed entries: 570,749.
- Worker 0: 6,198,417 states, 987,354 cycles, 5,628,030 duplicates;
  no donations. Total state count includes coordinator work.
- Best depth: 33; all 33 replay actions succeeded and goal satisfied;
  :REPLAY-VALID T. Replay was outside timing.

This establishes the current-source OFF reference, not snapshot benefit.
Next authorized case only: fresh stage, one worker, snapshots ON,
`claustro-snapshot-on-1-01`. Review that result before the two-worker OFF case.
No source edits or Lisp execution by Codex.


## Second end-to-end case: one worker, snapshots ON -- PASS

User reported `claustro-snapshot-on-1-01`; planned settings match the OFF case
except snapshots T. Outcome :EXHAUSTED-WITH-SOLUTIONS / :COMPLETE.
Wall 100.814285 s; process CPU 100.65625 s; GC counter 3.703125 s;
allocation 37,828,804,688 bytes; throughput 61,504.07 states/s.
Total states 6,200,489; cycles 987,354; duplicates 5,629,269;
symmetry-pruned 425,074. Worker counts exactly match the OFF case:
6,198,417 states, 987,354 cycles, 5,628,030 duplicates, no donations.
Best depth 33; all replay actions succeeded and goal satisfied (:REPLAY-VALID T).

ON minus OFF: +0.422920 s wall (+0.4213%), +0.421875 s CPU,
+777,488 allocated bytes, +0.703125 s GC counter. This single pair shows
essentially unchanged one-worker elapsed time and identical reported work;
it cannot separate snapshot overhead from run-to-run/GC variation.
The first full snapshot-enabled solve and replay passed; two-worker benefit
remains untested. Next authorized case only: freshly staged two workers,
snapshots OFF, label `claustro-snapshot-off-2-01`.
No source edits or Lisp execution by Codex.


## Third end-to-end case: two workers, snapshots OFF -- PASS

User reported `claustro-snapshot-off-2-01`; planned controls confirmed.
Outcome :EXHAUSTED-WITH-SOLUTIONS / :COMPLETE, best depth 33, replay T:
all 33 actions succeeded and goal satisfied.
Wall 122.35287 s; process CPU 238.04688 s; GC counter 3.15625 s;
allocation 37,941,598,128 bytes; throughput 50,814.664 states/s.
Total states 6,217,320; cycles 989,739; duplicates 5,636,033;
symmetry-pruned 425,161. Worker 0: 3,020,952 states / 475,117 cycles /
2,753,822 duplicates. Worker 1: 3,194,296 states / 514,622 cycles /
2,880,972 duplicates. Neither worker donated work.

Versus one-worker OFF: wall time increased about 21.88%, while states increased
only 0.271% and cycles 0.242%. The two-worker slowdown persists on current
source; this does not by itself identify its cause. Snapshot ON at two workers
is the remaining approved case, label `claustro-snapshot-on-2-01`, freshly staged.
No source changes or Lisp execution by Codex.


## Four-case end-to-end comparison complete -- all PASS

Final user result `claustro-snapshot-on-2-01`: :EXHAUSTED-WITH-SOLUTIONS /
:COMPLETE, depth 33, all replay actions succeeded and goal satisfied.
Wall 54.1521 s; CPU 101.75 s; GC counter 3.453125 s; allocation
37,942,690,416 bytes. States 6,217,951; cycles 989,786; duplicates 5,633,374;
symmetry-pruned 424,431; throughput 114,823.82 states/s.
Worker 0: 3,209,751 states / 512,452 cycles / 2,909,847 duplicates.
Worker 1: 3,006,128 states / 477,334 cycles / 2,722,288 duplicates.
No donations. Effective settings match the planned two-worker ON case.

| Workers | Snapshots | Wall seconds | CPU seconds | States | Replay depth 33 |
| --- | --- | ---: | ---: | ---: | --- |
| 1 | OFF | 100.391365 | 100.234375 | 6200489 | PASS |
| 1 | ON | 100.814285 | 100.65625 | 6200489 | PASS |
| 2 | OFF | 122.35287 | 238.04688 | 6217320 | PASS |
| 2 | ON | 54.1521 | 101.75 | 6217951 | PASS |

Two-worker ON is 2.26x faster than two-worker OFF (55.74% less elapsed),
and enabled one-to-two-worker scaling is 1.86x. Two-worker ON processed only
631 more states (0.0101%) and 47 more cycles than OFF. Allocation differs by
1,092,288 bytes. CPU drops from 238.05 to 101.75 s. This is strong initial
end-to-end evidence for the bounded worker-private ownership experiment,
including setup/verification/cleanup, consistent with prior isolated evidence.
It is one run per case, not a repeatability estimate or proof of general-domain
safety; the combined experiment does not isolate each private table's effect.
All four approved solves are complete. Keep the feature default off and preserve
investigation files. Recommended next step: review/consolidate the implementation
and diagnostics for a clean checkpoint, with no further solves or deletions
until their scope is agreed. No source changes or Lisp execution by Codex.


## Reverse-order repeat: two workers ON -- PASS

After checkpoint 4421433, user authorized two further full runs, ON then OFF,
freshly staged and reviewed individually, followed by a continuation prompt.
`claustro-snapshot-on-2-02` completed with :EXHAUSTED-WITH-SOLUTIONS /
:COMPLETE, best depth 33, :REPLAY-VALID T; all actions executed and goal satisfied.
Controls match the prior two-worker ON case.
Wall 55.116688 s; CPU 102.796875 s; GC counter 4.09375 s;
allocated bytes 37,919,650,128; states 6,213,471; cycles 989,069;
duplicates 5,632,798; symmetry-pruned 424,325; throughput 112,733.02 states/s.
Worker 0: 3,204,755 states / 510,667 cycles / 2,919,811 duplicates.
Worker 1: 3,006,644 states / 478,402 cycles / 2,711,748 duplicates.
Neither donated work.

Compared with the first ON run (54.1521 s), elapsed increased 0.964588 s
(1.78%), while states decreased 4,480 (0.072%). The approximately 54-55 s
ON result repeats with valid replay. The OFF half of the reverse-order pair
remains pending; do not claim the full order check is complete yet.
Next authorized run only: claustro-snapshot-off-2-02, freshly staged, two
workers, snapshots NIL. Then record both repeats and deliver the requested
continuation prompt. Four-plus-worker scaling and true-serial overhead remain
unmeasured and are not authorized solves. No source changes or Lisp by Codex.


## Reverse-order repeat complete: two workers OFF -- PASS

claustro-snapshot-off-2-02: :EXHAUSTED-WITH-SOLUTIONS / :COMPLETE,
all 33 actions replayed successfully, goal satisfied, :REPLAY-VALID T.
Planned controls confirmed. Wall 121.2312 s; CPU 234.89063 s;
GC counter 3.609375 s; baseline allocated bytes 37,975,381,120;
states 6,223,965; cycles 990,549; duplicates 5,642,284;
symmetry-pruned 427,803; throughput 51,339.63 states/s.
Worker 0: 3,024,222 states / 478,932 cycles / 2,748,223 duplicates.
Worker 1: 3,197,671 states / 511,617 cycles / 2,892,822 duplicates.
Neither donated work. Baseline allocation is 16 bytes above nested TIME output;
use the baseline envelope consistently for comparisons.

OFF repeat 121.2312 s versus first 122.35287 s (-0.92%).
ON repeat 55.116688 s versus first 54.1521 s (+1.78%).
Reverse pair speedup is 2.20x (54.54% elapsed reduction); states differ by
only 10,494 (0.169%). Across both pairs, OFF remains 121-122 s and ON 54-55 s.
Mean two-worker ON is 54.634394 s versus OFF 121.792035 s (2.23x).
Relative to the one-worker ON reference 100.814285 s, mean enabled speedup
is 1.845x (92.3% efficiency relative to one parallel worker, not true serial).
All six full runs passed depth-33 replay. Reversing order reproduced the large
benefit; two samples per two-worker case are not a statistical characterization.

Authorized repeat pair is complete. No further solves authorized. Next phase:
propose four-worker scaling measurement after inspecting CPU topology; separately
design a true-serial overhead comparison if desired. Default remains OFF,
limited to audited Claustro-Topo mode. Continuation prompt supplied to user.
Source remains at checkpoint 4421433; subsequent documentation is uncommitted.

## Next phase: CPU topology and four-worker proposal (approval pending)

Read-only Windows CIM inspection confirms Intel Core i9-14900K, 24 physical
cores, all 24 enabled, and 32 logical processors. The initial sandboxed CIM
query was denied; the elevated read-only query succeeded. Intel specifies
8 Performance-cores and 16 Efficient-cores for this model:
https://www.intel.com/content/www/us/en/products/sku/236773/intel-core-i9-processor-14900k-36m-cache-up-to-6-00-ghz/specifications.html
No SBCL process was returned by the process-name query, so current REPL process
affinity and actual worker placement were not established. Do not equate four
workers with four dedicated physical cores or predict linear scaling. No
processor affinity, power settings, source, or Lisp runtime was changed.

Proposed scope: ONE user-run full solve, claustro-snapshot-on-4-01, subject to
explicit solve approval before supplying its execution commands. Fresh stage;
WW-SET threads to four and then enable snapshots. Same SBCL build, memory
limit and reporting controls as the completed runs; record any restart or
machine/session change. DFS graph/min-length, cutoff 34, all branches,
symmetry on, randomization off, debug/probe off; no profiling wrappers.
Keep normal scheduling and all split/shard/donation settings unchanged.
With MIN-TASKS=256 and TASKS-PER-THREAD=8, COMPUTE-TARGET-TASKS remains 256
at both two and four workers. Verify effective settings and actual root
frontier in the returned report; do not assume identical subsequent work.

Use the existing explicitly loaded baseline helper. Total timing includes
initialization, copying, cold private memos, search, verification and cleanup;
replay stays outside timing. Return settings, task/phase and worker reports,
wall/process CPU/GC/allocation totals, states/cycles/duplicates/symmetry,
outcome/depth and replay validity. Stop at any error or failed replay and
review that result before any further solve. No automatic repeat or OFF run.
This bounds the number/configuration of searches, not their wall time; no
new timeout mechanism is introduced.

Compare the observed time T4 with two-worker ON mean 54.634394 s and
one-worker ON 100.814285 s: two-to-four speedup = 54.634394/T4;
one-to-four speedup = 100.814285/T4; worker-count efficiency =
100.814285/(4*T4). These are historical-reference, parallel-worker measures,
not true-serial or physical-core efficiency. Compare work and throughput as
well as time. If conditions changed or the result needs confirmation, propose
a matched two-worker control/repeat separately after review; do not silently
consume another solve. Default stays OFF and the audited ownership scope stays
unchanged.

True-serial overhead remains separate. THREADS=0 never activates snapshots,
but generated selectors and static-write guards still execute. Compare a
matched pre-implementation reference (candidate parent e89a246, to be verified
by source diff) with checkpoint 4421433 in separate checkouts and fresh user-run
Lisp images, isolated compiled-output caches and identical dependencies,
compiler/settings/problem/helper behavior. The older helper needs a neutral
reporting adapter because the snapshot setting does not exist there. Check
serial work equality and replay; repeat/order controls are needed for a small
overhead claim. The old 99-second result or current-source OFF/ON alone cannot
isolate implementation overhead. No reference checkout, adapter or solve has
been created/run for this proposal; preserve the current checkout and diagnostics.

## Four-worker snapshot-ON measurement approved

User approved ONE full solve: claustro-snapshot-on-4-01. Commands supplied for
fresh staging, WW-SET threads=4, then snapshots=T, and the explicitly loaded
baseline helper. Preconditions check all branches, no probe, inactive lifecycle
flags and the unchanged default task target controls. The helper checks the
remaining baseline search controls and replays the best path outside timing.
Await user results before any further run; no Lisp execution by Codex.

## Four-worker snapshot-ON result -- PASS

User report claustro-snapshot-on-4-01 completed on SBCL 2.6.8 X86-64:
:EXHAUSTED-WITH-SOLUTIONS / :COMPLETE, best depth 33, :REPLAY-VALID T.
All 33 actions executed successfully and the goal was satisfied. Effective
settings match the approved case: four workers, snapshots T, DFS graph/min-length,
cutoff 34, symmetry T, randomization NIL, debug 0, branch -1. Split/shard/donation
controls match prior cases. Target 256 again produced 343 tasks at depths 0-15.
No explicit restart/machine-change statement accompanied the report; affinity
and actual core placement remain unverified.

Baseline wall 33.62575 s; process CPU 111.03125 s; GC counter 3.28125 s;
allocated bytes 39,590,951,680; states 6,480,743; cycles 1,030,899;
duplicates 5,869,025; symmetry-pruned 472,657; throughput 192,731.55 states/s.
Nested TIME: 33.625 s wall, 3.304 s GC wall, 39,590,951,632 bytes consed.
Use the baseline envelope consistently (48 bytes above nested TIME).
Task generation 33 ms; worker phase 33.58 s; finalization rounded to 0 ms.
Closed entries 602,310 across 64 shards, size skew 1.04; this is not an
access-contention measurement. Printed 99.9% search efficiency is phase-time
fraction, not parallel efficiency; printed 0.0 branching factor remains unusable.

| Worker | States | Cycles | Duplicates | Donations |
| --- | ---: | ---: | ---: | ---: |
| 0 | 1599344 | 249900 | 1468253 | 0 |
| 1 | 1557728 | 246940 | 1397034 | 0 |
| 2 | 1791451 | 286839 | 1584999 | 0 |
| 3 | 1530148 | 247220 | 1417500 | 0 |

Worker state sum is 6,478,671 plus 2,072 coordinator states. Worker totals vary;
counts alone do not establish duration balance, terminal wait or core placement.

Against two-worker ON mean 54.634394 s, four workers are 1.62478x faster
(38.45% less elapsed). Against one-worker ON 100.814285 s, speedup is 2.99813x
and worker-count efficiency 74.95%. These are parallel-worker references,
not true-serial or physical-core efficiency. Relative to the mean two-worker
ON work, states increased 4.264% and cycles 4.191%; this is not fixed-work
scaling. Aggregate state throughput increased about 1.694x. CPU time rose
8.56% versus two-worker ON mean, with average process CPU/wall ratio 3.302.
That ratio does not identify how many physical cores were used. Extra search
work and remaining overhead may contribute; this run does not isolate causes.

All seven full solves passed depth-33 replay. This single four-worker result
supports further scaling in the audited configuration, with diminishing
worker-count efficiency; it does not establish repeatability or wider safety.
Default stays OFF. The one approved four-worker solve is complete; no remaining
solve authorization. Recommended next: separately approve ONE freshly staged
two-worker snapshot-ON control in the same session to check the historical
54-55 s reference under current conditions. Review it before considering a
four-worker repeat. True-serial overhead remains a separate unmatched question.
No Lisp or source changes by Codex; existing diagnostics/evidence preserved.

## Two-worker snapshot-ON control approved; awaiting result

User approved ONE freshly staged two-worker ON control in the same REPL session,
label claustro-snapshot-on-2-03, following four-worker ON 33.62575 s. Use the
existing baseline helper and unchanged search/split/shard/donation controls;
set threads via WW-SET after STAGE, then enable snapshots. Replay remains
outside timing. Purpose: check the historical two-worker 54-55 s reference
under current conditions. Review the returned report before proposing any
further solve; no four-worker repeat is authorized. No Lisp launched or source
changed by Codex; all existing evidence and diagnostics preserved.

## Two-worker snapshot-ON control -- PASS

claustro-snapshot-on-2-03 completed on SBCL 2.6.8 X86-64 with
:EXHAUSTED-WITH-SOLUTIONS / :COMPLETE, depth 33, :REPLAY-VALID T;
all 33 actions executed successfully and goal satisfied. Effective settings
match the approved two-worker ON control, including all branches and prior
split/shard/donation settings. Target 256 produced 343 tasks at depths 0-15.
No separate restart/machine-change statement was supplied with the report.

Wall 55.354248 s; process CPU 104.296875 s; GC counter 3.296875 s;
allocated bytes 37,998,483,680; states 6,228,618; cycles 991,269;
duplicates 5,643,938; symmetry-pruned 427,544; throughput 112,522.85 states/s.
Nested TIME wall 55.353 s, GC wall 3.304 s, consed 37,998,483,632 bytes;
baseline envelope again reports 48 additional bytes. Task generation 31 ms,
worker phase 55.31 s, finalization rounded to 0 ms. Closed entries 576,797,
64 shards, size skew 1.05. Existing printed efficiency/branching caveats apply.
Worker 0: states 3,197,345; cycles 514,938; duplicates 2,891,766.
Worker 1: states 3,029,201; cycles 476,331; duplicates 2,750,933.
No donations. Worker states sum to 6,226,546 plus 2,072 coordinator states.

The control is 1.318% slower than prior two-worker ON mean 54.634394 s,
and 0.431% slower than the latest previous ON run 55.116688 s. States rose
only 0.208% versus the prior two-run mean. It reproduces the approximately
54-55 s performance level, without evidence of a large timing shift. Three
ON times are 54.1521, 55.116688 and 55.354248 s (mean 54.874345 s).
Do not silently replace the two-run historical mean in earlier comparisons.

The preceding four-worker time 33.62575 s is 1.64619x faster than this control
(39.25% less elapsed), with 4.048% more states. This supports the additional
four-worker gain despite extra work. Four-worker repeatability is still
unmeasured; neither this control nor CPU utilization establishes core placement,
true-serial efficiency or a particular remaining bottleneck.

All eight full searches passed depth-33 replay. This approved control is
complete; no further solve authorized. Recommended next: separately approve
ONE fresh four-worker snapshot-ON repeat, claustro-snapshot-on-4-02, with the
same controls, to complete a four/two/four sequence. Review it before any
further measurement. Keep default OFF, ownership scope unchanged and true-serial
overhead separate. No Lisp launched or source changed by Codex; diagnostics
and previous uncommitted documentation preserved.

## Four-worker snapshot-ON repeat approved; awaiting result

User approved ONE freshly staged four-worker ON repeat in the same REPL session,
label claustro-snapshot-on-4-02. This completes the proposed four/two/four
sequence after 33.62575 s (four) and 55.354248 s (two). Use the existing
explicitly loaded baseline helper, unchanged controls and replay outside timing.
STAGE first, WW-SET threads=4, then snapshots=T. Review the returned result
before proposing further work. No other solve is authorized. No Lisp launched,
source changed or diagnostics removed by Codex.

## Four-worker ON repeat -- PASS; four/two/four sequence complete

claustro-snapshot-on-4-02 on SBCL 2.6.8 X86-64 completed with
:EXHAUSTED-WITH-SOLUTIONS / :COMPLETE, depth 33, :REPLAY-VALID T.
All 33 replay actions succeeded and goal satisfied. Effective controls match
four-worker ON first run, including split/shard/donation settings. Target 256
again produced 343 tasks at depths 0-15. No separate restart/machine-change
statement accompanied the report; core placement remains unverified.

Wall 36.41016 s; process CPU 120.828125 s; GC counter 3.625 s;
baseline allocation 39,420,815,568 bytes; states 6,453,081; cycles 1,026,785;
duplicates 5,848,619; symmetry-pruned 471,216; throughput 177,232.97 states/s.
Nested TIME wall 36.409 s; GC wall 3.646 s; consed 39,420,782,816 bytes.
Baseline allocation exceeds nested TIME by 32,752 bytes; retain both and use
the baseline envelope consistently, without attributing the difference to
snapshot copying. Task generation 30 ms; worker phase 36.37 s; finalization
rounded to 0 ms. Closed entries 595,750, 64 shards, size skew 1.04.

| Worker | States | Cycles | Duplicates | Donations |
| --- | ---: | ---: | ---: | ---: |
| 0 | 1657981 | 264529 | 1476033 | 0 |
| 1 | 1475384 | 234781 | 1360280 | 0 |
| 2 | 1668562 | 264857 | 1485222 | 0 |
| 3 | 1649082 | 262618 | 1525845 | 0 |

Worker states total 6,451,009 plus 2,072 coordinator states. The returned
valid solution differs at action 14: JAMMER2 is placed on GROUND at LOCATION6
instead of PLATE3; action 25 subsequently places it on PLATE3. Same valid
depth does not imply identical path or exploration order.

Repeat wall is 2.78441 s (+8.281%) above first four-worker time 33.62575 s,
while states decreased 0.427% and cycles decreased by 4,114. CPU increased
8.824%. GC wall increased 0.342 s, so measured GC wall alone cannot explain
the elapsed difference. Work counts do not capture state mix; scheduling,
core placement, clock changes and residual contention remain possibilities,
not established causes. No new instrumentation was run.

The four/two/four sequence is 33.62575 / 55.354248 / 36.41016 s, all passing.
Even the slower four-worker repeat takes 34.22% less time than the intervening
two-worker control. The additional gain repeats, but four-worker timing has
more observed variation than the three two-worker ON samples.

| Workers, snapshots ON | Samples | Mean wall s | Speedup vs one parallel worker | Worker-count efficiency |
| --- | ---: | ---: | ---: | ---: |
| 1 | 1 | 100.814285 | 1.000x | 100.0% |
| 2 | 3 | 54.874345 | 1.837x | 91.9% |
| 4 | 2 | 35.017955 | 2.879x | 72.0% |

Ratio of two/four means is 1.567x (36.19% less elapsed at four). Four-worker
mean states exceed three-run two-worker mean by 3.969%; this is not fixed-work
scaling. Means summarize these samples, not a statistical characterization;
old two-run reference comparisons remain dated evidence. Efficiency is relative
to one parallel worker, not true serial or dedicated physical cores. No
four-worker OFF measurement exists to isolate snapshot benefit at four workers.

All NINE full searches passed depth-33 replay. All authorized solves are now
complete. Default stays OFF and the ownership policy remains bounded to the
audited configuration. Preserve diagnostics and all evidence. Recommended next:
pause full solves and prepare/review the matched true-serial overhead comparison
(reference diff, isolated checkout/cache plan and neutral reporting helper),
with separate approval for its execution. Do not infer serial overhead from
these worker measurements or request more scaling solves automatically.
No Lisp launched or source changed by Codex.

## True-serial comparison prepared for review; no Lisp or solve

Source comparison identifies parent e89a246 as the matched pre-implementation
reference for 4421433; probs/tech and dependencies are unchanged. New
src/ww-serial-baseline.lisp is a neutral explicitly loaded helper usable in
both versions, with expected-root/version/zero-worker preflight and standard
solve-envelope timing plus replay. It does not load or solve automatically.
LF and static source checks only; user compilation/preflight remains pending.
See doc/parallel-serial-overhead-plan.md for isolated checkout/cache setup,
fresh-image A/B/B/A proposal, work-equality gates and limits. No reference
checkout or Lisp process created; no solves authorized. Next review is setup
and user-run no-solve preflight, before separate timing approval. Default off;
existing source implementation, diagnostics and evidence preserved.

## Isolated references created; A no-solve preflight ready

User approved setup. Detached worktrees are now artifacts/serial-01/a at
 e89a246b206c7208918004ad782821eaa0b961d4 and artifacts/serial-01/b at
44214337614130462360d82f0741d9a2fa77cdb1. Both were verified clean after creation;
main and its uncommitted documents/helper are preserved. Git required elevated
metadata access to register worktrees; no Lisp process was launched.
Checkout used core.autocrlf=false to preserve LF. Tracked-file SHA-256 manifests
are a-source-sha256.txt and b-source-sha256.txt inside that investigation directory.

Identical neutral helper copied to artifacts/serial-01/ww-serial-baseline.lisp:
SHA-256 EADD21ADA1BF8E87EDA595C884D1403AB18FC1A733B28B8F3417050312421A99.
Setup-manifest.json records setup/helper/source-manifest hashes. The user-run
start-a-preflight.ps1 checks A commit/cleanliness and helper hash, uses instance
serial-a-preflight and cache-a-preflight, launches SBCL with 4096 MiB, and restores
its PowerShell environment/location when SBCL exits. It passes no Lisp startup
commands and never launches a solve. PowerShell parser check passed; not launched.

User then LOADs preflight-a.lisp at the fresh REPL. It checks a fresh package
state, explicitly loads the A ASD, verifies source root, Quickloads, stages
Claustro, sets threads zero and loads the neutral helper. It verifies the serial
controls and snapshot absence, prints settings/environment/compiler policy,
checks the translated FASL lies in the private cache, and reports dependency
versions/roots before SERIAL A PREFLIGHT PASS -- NO SOLVE RUN.
Compiler policy reporting uses the documented SBCL function:
https://www.sbcl.org/manual/#Compiler-Policy
Preflight LF/delimiter checks passed, but runtime compilation/loading is still
unverified. Stop on the first failure and review it; do not run a solve.

Only A setup commands supplied this turn. Await its user report before B setup.
No full solve authorized. Measured cases will use fresh instances/caches, not
these preflight images. Preserve all reference/setup files until agreed closeout.

## A preflight source-selection failure and repair

User's A preflight stopped at the source-root assertion BEFORE Quickload,
staging or solve: ASDF returned the main checkout despite explicit LOAD-ASD.
Local ASDF source SEARCH-FOR-SYSTEM-DEFINITION searches definition functions
again, and Quicklisp installs a local-projects searcher. Explicit LOAD-ASD alone
therefore did not pin this reference. Keep the assertion; do not continue past it.

Updated artifacts/serial-01/preflight-a.lisp defines and prepends a named
SERIAL-REFERENCE-SYSTEM-SEARCH returning A's ASD only for system "wouldwork";
other names return NIL and retain normal dependency discovery. Clear the
registered Wouldwork system, load the intended ASD, then run the unchanged
root check before Quickload. Rule persists in this disposable image for staging
reloads. Apply the same exact-name pin with the corresponding path to B later.
No production/reference source change. Both worktrees remain tracked-clean.

Preserved previous setup as preflight-a.before-source-fix.lisp and
setup-manifest.before-source-fix.json; updated setup-manifest.json hashes.
Main now has untracked src/problem-serial-a-preflight.lisp, consistent with
ASD bootstrap during incorrect rediscovery. It is preserved, not deleted or
used for measurement. No full solve ran. Static delimiter/LF checks passed;
user execution is still required to validate the fix.

Next user step: exit this failed disposable SBCL image, rerun the same A
launcher and LOAD the corrected preflight file. No CONTINUE/ignore-error
restart and no solve. Review A's returned roots/settings/preflight before B.

## Reference A no-solve preflight PASS; reference B ready

User confirms SERIAL A PREFLIGHT PASS -- NO SOLVE RUN after the searcher fix.
Root is artifacts/serial-01/a/, SNAPSHOT-CODE NIL, THREADS=0, all expected
Claustro controls. SBCL 2.6.8, ASDF 3.3.1, X86-64; instance serial-a-preflight;
translated searcher FASL is inside cache-a-preflight/sbcl-2.6.8-win-x64/.
Printed global basic compiler qualities are all 1 (compilation-speed, debug,
safety, space, speed, inhibit-warnings). :POLICY NIL is the reporting function's
return value, not an absent policy; the preceding printed qualities are evidence.

Reported dependencies: Alexandria 1.0.1 (alexandria-20241012-git), Iterate 1.5.2
(iterate-release-b0f9a9c6-git), lparallel 2.8.4 (lparallel-20160825-git),
Bordeaux-Threads 0.9.4 (bordeaux-threads-v0.9.4), usocket 0.8.8 (usocket-0.8.8),
cl-json 0.6.0 (cl-json-20220707-git), flexi-streams 1.0.20
(flexi-streams-20241012-git). All under D:/quicklisp/dists/quicklisp/software/.
The report validates setup/helper loading and preflight only, not timing or replay.

Prepared start-b-preflight.ps1 and preflight-b.lisp in artifacts/serial-01/,
matching the now-passing A setup except B commit/path, instance/cache, expected
snapshot-code T and output label. Snapshot option remains NIL with zero workers.
Named ASDF searcher pins B during loading/restaging. Same neutral helper bytes;
setup-b-manifest.json records hashes. B worktree remains tracked-clean.
Launcher parsing and Lisp delimiter/LF checks passed; no Lisp launched by Codex.

Next user step: exit disposable A image, start fresh B via its launcher, LOAD
preflight-b.lisp and return compiler-policy/settings/root/cache/dependency output.
Expected SERIAL B PREFLIGHT PASS -- NO SOLVE RUN. Review B against A before any
timing approval. No solves authorized; source and diagnostics preserved.

## Reference B preflight PASS; matched setup gate complete

User confirms SERIAL B PREFLIGHT PASS -- NO SOLVE RUN, returning T.
Root artifacts/serial-01/b/, SNAPSHOT-CODE T, THREADS=0; the neutral preflight
also asserts snapshot enable/freeze/read views NIL. B's translated searcher
FASL is inside cache-b-preflight/sbcl-2.6.8-win-x64/ and its instance is
serial-b-preflight. Both source worktrees remain tracked-clean on inspection.

B matches A in all reported search controls, SBCL 2.6.8, ASDF 3.3.1, X86-64,
all printed basic/dependent compiler qualities, and all seven dependency
versions/source directories listed above. Intended differences are reference
source root/code presence and isolated instance/cache paths. :POLICY NIL is
again just the reporting function return. Setup checks do not measure overhead
or exercise the helper's full timing/replay path.

Both user-run loading/staging/preflight gates passed. Recommended next is
separate approval for four full true-serial solves, A1 / B1 / B2 / A2, fresh
user-started images/instances/caches and fresh staging for every case. Issue
one case at a time and review each complete outcome, work counts and depth-33
replay before continuing. Stop on any error/mismatch. No additional warmup
solves. Do not time the already-used preflight images. Runtime overhead remains
unmeasured; do not interpret parallel evidence as serial overhead.

No full solve is currently authorized and no solve command supplied. Existing
source, diagnostics, failed-bootstrap artifact and uncommitted evidence are
preserved. Default snapshots off; no ownership-policy change; no Lisp by Codex.

## Four true-serial solves approved; A1 supplied

User approved A1 / B1 / B2 / A2, with fresh image, instance, cache and staging
for each case; supply and review one run at a time. Both setup preflights passed.
First case is A1 at e89a246, label claustro-serial-before-a1, expected snapshot
code absent. New start-a1.ps1 launches the same SBCL/4096 MiB configuration with
instance serial-a1 and empty cache-a1; setup-a1.lisp uses the proven A preflight
with only instance/cache and completion-label substitutions. Setup performs no
solve. The separately supplied RUN-SERIAL-BASELINE invocation runs exactly one
full serial search and replays the best path outside timing. Enter package WW
at the interactive prompt after LOAD before calling the helper.

Launcher parsing, LF and setup-template comparison passed. Setup A1 manifest
retains script/helper hashes. No Lisp launched by Codex; setup/timing execution
awaits user report. Return setup diagnostics and ordinary TIME/summary plus one
BASELINE RESULT and replay validity, or stop on first error. Review A1 before
providing B1. Remaining approved cases after A1 are B1, B2, A2; no extra solves.
Existing source, worktrees, diagnostics and uncommitted evidence preserved.

## True-serial A1 -- completed and replay PASS; B1 supplied

User report claustro-serial-before-a1: SBCL 2.6.8 X86-64,
:EXHAUSTED-WITH-SOLUTIONS / :COMPLETE, depth 33, :REPLAY-VALID T.
All actions succeeded and goal satisfied. Baseline controls match true serial
Claustro DFS graph/min-length cutoff 34, symmetry T, randomization NIL, debug 0,
all branches, progress 1,000,000. Wall 103.828125 s; process CPU 103.5 s;
GC counter 5.09375 s; allocation 58,807,886,704 bytes; throughput 74,296.766.
States 7,714,094; cycles 1,877,372; duplicates 7,044,735; symmetry 623,614:
all exactly match the historical serial work reference. Nested TIME wall
103.827 s; GC wall 5.129 s; allocation agrees with baseline. The retained path
routes JAMMER2 through LOCATION4/GATE3 then PLATE2, unlike some parallel paths.

The paste begins at the final search summary; A1 setup environment/cache/policy
and source report were omitted. Helper run-time preflight precedes this result,
but do not claim the omitted A1 setup metadata was independently reviewed.
Request confirmation of A1 SETUP PASS and no setup errors with the next report;
retain B1 setup reports to verify matching fresh-image conditions. No rerun of
A1 is requested. Both reference worktrees still have no tracked changes.
No overhead inference is possible from A1 alone.

Next already-approved case B1: claustro-serial-after-b1, checkpoint 4421433,
zero workers, snapshot code present but disabled. Prepared start-b1.ps1 and
setup-b1.lisp from passing B preflight with only instance/cache/label changes;
new serial-b1 instance and empty cache-b1, same neutral helper and memory limit.
Launcher parse/LF checks passed; setup-b1-manifest.json retains hashes.
User starts a fresh image, loads setup, then invokes exactly one baseline/replay.
Review B1 before providing B2. Remaining approved after B1: B2, A2.
No Lisp launched or production source edited by Codex; all evidence preserved.

## True-serial B1 PASS; first pair shows candidate overhead, repeats pending

User supplied B1 setup and full result, claustro-serial-after-b1. Correct B root,
instance serial-b1, private cache-b1 FASL path, snapshot code T but option/views
NIL as asserted, zero workers and matching search controls. SBCL 2.6.8, ASDF
3.3.1, compiler qualities and all seven dependency versions/roots match the
preflights. SERIAL B1 SETUP PASS reported. A1 setup-output confirmation remains
omitted; retain this evidence gap without requesting an extra solve.

B1 :EXHAUSTED-WITH-SOLUTIONS / :COMPLETE, best depth 33, replay T; all actions
succeeded and goal satisfied. Wall 108.241875 s; CPU 107.84375 s; GC counter
5.15625 s; allocation 58,808,077,440 bytes; throughput 71,267.19 states/s.
States 7,714,094; cycles 1,877,372; duplicates 7,044,735; symmetry 623,614:
exactly equal to A1. Whitespace-normalized BEST-PATH text matches A1 exactly.
Nested TIME wall 108.241 s, GC wall 5.166 s, consed 58,808,044,688 bytes
(32,752 below baseline envelope; use baseline consistently).

B1 versus A1: +4.41375 s wall (+4.251%), +4.34375 s CPU (+4.197%),
+190,736 allocated bytes. GC wall differs by only +0.037 s. Equal work and
path make this a candidate implementation overhead signal, but one pair cannot
separate it from run-to-run variation/order effects. Do not report established
4.25% overhead before completing the approved B2/A2 reverse pair.

Next already-approved case B2, label claustro-serial-after-b2. Prepared new
start-b2.ps1/setup-b2.lisp from passing B1 setup with only case identifiers,
cache path and message substitutions. Fresh serial-b2 instance/cache-b2; same
reference and helper, source still tracked-clean. Launcher parse/LF checks
passed, setup-b2-manifest.json recorded. User starts fresh SBCL, loads setup,
then runs exactly one baseline/replay. Review B2 before issuing final A2.
No Lisp by Codex, no production changes, no diagnostics removed.

## True-serial B2 PASS; final approved A2 supplied

User supplied B2 setup and full report, claustro-serial-after-b2. Correct B
source, serial-b2 instance, cache-b2 FASL path, zero workers, matching controls,
SBCL 2.6.8/ASDF 3.3.1, compiler policy and seven dependency versions/roots.
SERIAL B2 SETUP PASS confirms the setup. Snapshot code is present but disabled.
A1's omitted setup-output confirmation remains an evidence gap.

B2 :EXHAUSTED-WITH-SOLUTIONS / :COMPLETE, depth 33, replay T. Wall 106.55689 s;
CPU 106.328125 s; GC counter 4.921875 s; allocated 58,808,032,064 bytes;
throughput 72,394.13 states/s. States 7,714,094; cycles 1,877,372; duplicates
7,044,735; symmetry 623,614 all exactly match A1/B1. Whitespace-normalized
BEST-PATH matches A1 (and therefore B1) exactly. Nested TIME wall 106.556 s,
GC wall 5.011 s; allocation agrees with baseline. All replay actions succeeded.

B2 is 1.684985 s faster than B1 (-1.557%) and 2.628% slower than A1.
This preserves a possible overhead signal but also demonstrates run variation.
Wait for A2 before computing final paired/mean comparisons or drawing a
conclusion. No additional solve is needed beyond the already-approved A2.

Prepared start-a2.ps1 and setup-a2.lisp by substituting case/instance/cache
identifiers in A1 setup. Fresh serial-a2 instance/cache-a2, same A commit and
neutral helper. Launcher parsing/LF checks passed; setup-a2-manifest.json
records hashes. A source remains tracked-clean. Next user run only:
claustro-serial-before-a2, expected snapshot code NIL, with replay outside timing.
After this result, all four serial solve approvals will be exhausted. No Lisp
launched or production source changed by Codex; evidence/diagnostics preserved.

## Final true-serial A2 PASS; A/B/B/A complete (2026-09-14)

A2 claustro-serial-before-a2: correct A root, serial-a2 instance/cache-a2,
SNAPSHOT-CODE NIL, zero workers; settings, compiler policy and dependency
versions/roots match prior preflights and B1/B2. SERIAL A2 SETUP PASS reported.
Outcome :EXHAUSTED-WITH-SOLUTIONS / :COMPLETE, depth 33, replay T.
Wall 102.472275 s; CPU 102.21875 s; GC counter 4.90625 s;
allocated 58,807,972,688 bytes; throughput 75,279.81 states/s.
Nested TIME wall 102.472 s, GC wall 4.987 s, allocation agrees with baseline.

All four cases have exactly 7,714,094 states, 1,877,372 cycles, 7,044,735
duplicates and 623,614 symmetry-pruned. All four whitespace-normalized best
paths match and all 33 replay actions succeed with goal satisfaction.

| Case | Version | Wall s | CPU s | GC counter s | Allocated bytes |
| --- | --- | ---: | ---: | ---: | ---: |
| A1 | Before e89a246 | 103.828125 | 103.5 | 5.09375 | 58807886704 |
| B1 | After 4421433 | 108.241875 | 107.84375 | 5.15625 | 58808077440 |
| B2 | After 4421433 | 106.55689 | 106.328125 | 4.921875 | 58808032064 |
| A2 | Before e89a246 | 102.472275 | 102.21875 | 4.90625 | 58807972688 |

Before mean 103.1502 s; after mean 107.3993825 s: +4.2491825 s (+4.1194%).
Mean CPU increase +4.1091%. Forward B1/A1 wall +4.2510%, reverse B2/A2
+3.9861%. A repeat improved 1.306%; B repeat improved 1.557%. GC wall pair
differences are only 0.037 and 0.024 s. Equal work/path, both paired orders
and matching CPU increase support approximately 4.1% serial implementation
overhead in this bounded configuration. This is two samples per version,
not a precise statistical estimate or proof of which selector/guard causes it.
A1 setup/environment output was omitted and never separately confirmed;
its run-time helper preflight passed, but retain the metadata evidence gap.

This measures the combined implementation with snapshots disabled at THREADS=0,
not snapshot copying or parallel-worker overhead. Do not silently substitute
this new serial reference into prior worker-count efficiency calculations:
serial and parallel explore different work. No source optimization is yet made.
Both reference worktrees remain tracked-clean. Raw serial reports are retained
under artifacts/serial-01/result-a1.txt, result-b1.txt, result-b2.txt, result-a2.txt.

All four serial solves and all earlier nine current-implementation parallel
comparison/scaling cases are complete and replay-valid. No further solves
remain authorized. Default remains off; preserve ownership/guards and diagnostics.
Next recommended source-only investigation: identify opportunities to avoid
repeated read-selector/static-guard overhead in ordinary serial execution while
retaining correct worker routing, freeze/write protection and restage semantics.
Compare generated-form specialization against per-read dynamic selection, but
present tradeoffs and a focused validation plan before implementation approval.
Keep residual four-worker scheduling/shared-access/GC/work-order questions
separate; propose measurements rather than presuming their cause or more cores.
Continuation prompt is doc/parallel-efficiency-continuation.md. No Lisp by Codex.

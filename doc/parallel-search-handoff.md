# Parallel graph-search diagnostics handoff

## Latest user result: all four full-search cases PASS

One worker OFF/ON: 100.391365 / 100.814285 seconds.
Two workers OFF/ON: 122.35287 / 54.1521 seconds.
All completed with successful depth-33 replay. Two-worker ON is 2.26x faster
than OFF; enabled one-to-two-worker scaling is 1.86x, with nearly identical
work. This establishes initial bounded full-search benefit, not repeatability
or general-domain ownership safety. Full details at the end of parallel-baseline.md.
All four approved solves are complete; no further solves authorized.
Keep default off and preserve diagnostics. Next recommendation: review and
consolidate for a clean checkpoint; agree cleanup scope before deleting files.

## Focused validation: second snapshot run PASS

Both focused runs passed: each has two three-seed/ten-successor read comparisons,
concurrent read/private-memo/hash checks and all 15 off/on lifecycle cases. The
second report also confirms NIL snapshot-enabled, frozen-phase and parallel-active
flags through successful ASSERTs (ASSERT's NIL return means success). It follows
the requested restage/repeat sequence; staging commands were not pasted.

The focused validation gate is satisfied. The focused tests ran no solve. Subsequent one-worker full solves passed
depth-33 replay off/on; the subsequent two-worker comparison also passed and showed benefit. Next proposal: four
freshly staged, uninstrumented cases (one worker off/on, two workers off/on),
reviewed individually, including setup/cleanup costs and replay outside timing.
The user approved this four-case comparison, one run at a time, with each
result posted and reviewed before the next. Codex must still never run Lisp. Detailed results and proposed controls are appended to
parallel-baseline.md. Preserve all investigation files.

## Current continuation status: approved implementation, focused validation passed

The opt-in worker read experiment is implemented, default off. All Lisp execution
remains with the user; no compilation/test/solve was run by Codex. See
[parallel-snapshot-audit.md](parallel-snapshot-audit.md) for the complete bounded
worker-path/ownership audit and the end of parallel-baseline.md for commands.
New production module: src/ww-worker-read-snapshots.lisp. New explicitly loaded
helper: src/ww-worker-read-snapshot-tests.lisp. Existing diagnostic files are
preserved; gate/receiver clone rewriting recognizes the new selectors.

The audit added private MOBILITY-ROUTE-KEYS and TRAVERSAL-CANONICAL-FAMILIES to
the proposed private vertical memos. Synchronization is unchanged. Guards cover
integer growth, static writes and supported lifecycle/installation entry points.
Publication follows root generation; cleanup covers preparation and partial
thread startup and joins all created workers before ending the freeze.

Next: review the completed four-case evidence and agree checkpoint/cleanup scope.
Do not run Lisp or launch additional solves. The earlier
entries below are chronological evidence, superseded by this current status.

## Workflow and scope

The user runs ALL Lisp commands at their own REPL and returns reports. Codex
edits and inspects source, supplies commands, and interprets results. Do not
launch Lisp or solves yourself. Both focused runs and clear-state assertions passed.
Keep action-sequence replay after solves: it is outside timing and all reported
33-step solutions have replayed successfully. Keep diagnostic helpers for this
ongoing investigation; remove temporary artifacts at feature closeout.

Repository: D:/quicklisp/local-projects/wouldwork. Follow AGENTS.md; ignore
CLAUDE.md. Preserve other edits and inspect live status before changes.

## Current files

- src/ww-parallel-baseline.lisp: explicitly loaded one-run helper; records
  settings, elapsed/CPU/allocation, counts, best path, and replay result.
- src/ww-parallel-phase-probe.lisp: temporary function wrappers restored on
  exit. Base phases EXPAND, successor processing, queue, worker duration.
  :detail t adds state construction, copying, followups, propagation loop/pass.
  :updates t adds five propagation updates. Use these modes separately.
- src/ww-table-read-probe.lisp: isolated fixed-work table lookup benchmark,
  private synchronized/unsynchronized STATIC-IDB copies, one/two workers,
  two rounds in reverse order. A missing parenthesis in thread creation was
  corrected before the successful user run. No planner tables modified.
- doc/parallel-baseline.md: chronological evidence and caveats.

Helpers are not ASDF components: reload explicitly after Lisp restart. User
confirmed a restart before claustro-detail-2-01. Use WW-SET for thread changes;
crossing zero rebuilds synchronization-sensitive state. Never merely bind the
thread variable around a solve. Staging can reset settings.

## Established baselines

Claustro-Topo: depth-first, graph, min-length, cutoff 34, symmetry on, random
off, debug 0, all initial branches. SBCL 2.6.8 x86-64. Thread count means
workers, with zero selecting serial. All runs completed at best depth 33 and
passed replay. These are user-reported observations, not agent-run tests.

| Run | Wall seconds | Generated states | CPU seconds |
| --- | ---: | ---: | ---: |
| serial | 99.05524 | 7714094 | 98.78125 |
| parallel 1 | 97.10532 | 6200489 | 96.78125 |
| parallel 1 repeat | 96.894394 | 6200489 | 96.828125 |
| parallel 2 | 115.920166 | 6219321 | 224.45313 |

Parallel 2 does essentially the same work as parallel 1 but takes about 19%
longer. About 194% CPU utilization, balanced worker totals (~49/51%), no
donations. Queue timing in later probes shows about three seconds of waiting
on one worker, including terminal wait. Initial split: 343 tasks through depth
15, about 28-29 ms. GC wall about 3-4 s. Shard size skew about 1.05. These do
not identify the primary slowdown; shard size balance is not access balance.

## Localization from sampled wall times

Sampling every 1024 calls; preserve raw counts and ticks in reports. Means are
approximate. Nested scopes overlap; do not add/subtract extrapolated totals
as exact exclusive accounting. Small calls approach timer resolution.

| Operation, mean microseconds | 1 worker | 2 workers |
| --- | ---: | ---: |
| EXPAND | about 80 | 207-221 |
| successor processing (includes closed table) | 15.66 | about 17 |
| state construction | 33.39 | 103-109 |
| followups | 8.75 | 26-28 |
| COPY-IDB | 0.325 | 0.330-0.348 |
| propagation pass | 2.23 | 6.36-6.67 |
| receiver update | 1.09 | 3.11-3.22 |
| gate update | 1.07 | 2.83-2.95 |
| plate update | 0.42 | 0.58-0.59 |
| blower update | 0.17 | about 0.17 |
| threat update | 0.19 | about 0.21 |

Propagation averages 1.039 passes/invocation for both worker counts. About
25.8 million invocations, 26.8 million passes, about 4.15 invocations per
generated child. This does not establish redundant work: inspect call sites
before proposing removal. Inflation is cost per pass rather than more passes.

Driver returned by user's REPL:
```
(LET ((*PROPAGATED-STATE-CHANGED* NIL))
  (UPDATE-PLATE-STATUS!)
  (UPDATE-RECEIVER-STATUS!)
  (UPDATE-GATE-STATUS!)
  (UPDATE-BLOWER-STATUS!)
  (ENFORCE-THREAT-SAFETY!)
  *PROPAGATED-STATE-CHANGED*)
```

Base probe overhead was small, but all-update wrappers raised one-worker time
to 115.12 s (~18 s extra across ~134 million intercepted calls). Two-worker
all-update run was 123.76 s. Do not benchmark production speedups with this
intrusive configuration. No statistical profiler run has been performed.
User has (profile), SBCL deterministic whole-package profiling; keep separate
from baseline and consider overhead before requesting it.

## Shared-table hypothesis and latest evidence

Live flags supplied by user:

| Table | Synchronized | Entries |
| --- | --- | ---: |
| STATIC-IDB | T | 1221 |
| STATIC-DB | NIL | 1201 |
| RELATIONS | T | 16 |
| STATIC-RELATIONS | T | 84 |
| CONSTANT-INTEGERS | NIL | 148 |
| INTEGER-CONSTANTS | T | 148 |
| PROP-KEY-CACHE | T | 0 |

Gate CONTROL-ON reads static controller wiring; receiver queries read static
apparatus/geometry. Generated queries and common metadata accesses need further
inspection. REGISTER-DYNAMIC-OBJECT in ww-converter.lisp can write STATIC-IDB;
do not simply disable synchronization as a general fix. No ownership/write
audit establishing safe read-only lifetime has been completed.

Latest isolated lookup test: 1221 keys, 2,500,608 successful lookups per case,
fixed total work, private table copies. Each cell lists two rounds, wall seconds:

| Synchronization | 1 worker | 2 workers |
| --- | --- | --- |
| on | 0.055519 / 0.054658 | 0.217485 / 0.215026 |
| off | 0.016565 / 0.016341 | 0.030044 / 0.029918 |

Same-worker synchronized cost is about 3.35x at one worker and 7.2x at two.
This supports synchronized-read contention as a plausible substantial contributor.
BUT two unsynchronized workers also slow down (~1.82x one worker). The test is
very short, includes startup/join, has no start barrier, scans identical key
order, and does not reproduce the search workload. It cannot establish an
end-to-end gain, exclusive cause, or safe production change. CPU clock granularity
is visible in these short runs. Function compilation policy and hardware/core
placement have not been controlled or recorded.

## Recommended next step

Pause full solves. Audit static-table lifetime and generated read paths, and
improve the isolated benchmark before altering production synchronization:
longer calibrated fixed-work cases, explicit benchmark compilation policy,
warmup, repeatable start coordination, and machine/core/affinity context.
Retain one/two-worker synchronized and unsynchronized controls. All Lisp runs
stay with the user. Then design a narrowly scoped reversible experiment if
the ownership audit supports it; validate goal/path correctness and compare
uninstrumented solves. Do not keep adding broad wrappers or increase workers
without resolving the observed negative scaling.

## Reporting issues already noticed

- Baseline symmetry counter initially used local pruning count (zero); fixed
  to use canonical counter when active. Serial correct count is 623614.
- Existing parallel progress double-counts solutions (shared list plus worker
  totals); final report has two, progress showed four. Not yet fixed.
- Existing final parallel average branching factor prints 0.0; do not use it.
- Printed search efficiency is phase-time fraction, not parallel efficiency.
- Serial/parallel cycle counter comparability remains unaudited.

No new session/task has been created. This document is the handoff, not a
claim that the bottleneck is fully diagnosed or repaired.

## Continuation prepared 2026-09-13 (awaiting user REPL results)

Read-path/lifetime source audit and V2 commands are appended to
parallel-baseline.md. Generated integer reads use STATIC-IDB and variable-code
reads in CONSTANT-INTEGERS. Runtime conversion/registration and reset paths mean
production immutability is still unproven. Production files remain unchanged.

ww-table-read-probe.lisp now has calibrated fixed work, per-worker warmup,
semaphore start coordination, explicit kernel policy, raw worker timings and
three orders of six cases, adding separate-table controls. Earlier descriptions
of two short rounds apply to V1 only. V2 has not been compiled/run by Codex;
all Lisp execution remains with the user. Next: run the appended REPL commands,
return generated forms and the 18 measured rows; no full solve yet. Hardware/
affinity remains unknown (CIM access denied). Keep the helpers for this ongoing
investigation. No production synchronization change is authorized by this audit.

## V2 results received 2026-09-13

Supersedes the pending benchmark status above. All 18 fixed-work cases completed;
see parallel-baseline.md for all timings. Median seconds: synchronized one/shared
 two/separate two = 6.802205 / 28.200723 / 3.411536; plain = 1.360141 /
1.909513 / 0.685358. Separate tables scale almost 2x in both modes; shared tables
slow down 4.15x synchronized and 1.40x plain. Start delays <=83 microseconds.
Table sharing is strongly implicated in this isolated workload, but the precise
SBCL mechanism and planner benefit remain unproven. Generated query forms were
not included. Recommended next: inspect SBCL lookup internals/disassembly and
obtain those forms, without a full solve or production synchronization change.

## Matching SBCL source inspected; next report prepared

Official sbcl-2.6.8 source confirms ordinary EQL getter stores the last successful
key index in HASH-TABLE-CACHE. Synchronized getters wrap lookup in a system lock.
A flat alternative exists, so the live selected implementation still needs
verification. Added INSPECT-STATIC-TABLE-READ-PATH in the existing helper: reports
live getter identities, private-copy cache transitions/disassembly, and generated
query forms. Load helper and call that function; no benchmark or solve. See
parallel-baseline.md for source URL, line references and exact REPL commands.
No production changes; cache-coherence contribution remains an inference.

## Installed lookup confirmation received

User report verifies ordinary EQL getter on the plain private copy, changing
cache indices on successful reads, and matching cache-slot machine-code store.
Synchronized private lookup changes the same slot and calls the recursive system
lock. Live constant map uses unsynchronized EQUAL; fluent-index table uses flat
EQ. See baseline for distinctions and limits. Report stops at GENERATED
CONTROL-ON with no form/error/prompt: obtain remaining output before claiming
generated-path inspection is complete. No more timing run or solve needed yet.

## Generated query continuation received

All five requested generated forms are now supplied. CONTROL-ON reads static
wiring; ENERGIZED reads static controller types and dynamic status, repeatedly
encoding objects through CONSTANT-INTEGERS. Direct receiver prefix reaches up
to three static and four constant-map reads before corridor/cut queries.
Literal gate/receiver loops do not read relation metadata for enumeration.
Both static facts and object-code mapping are now concrete hot-path candidates.
See baseline for conditional counts and proposed isolated update diagnostic;
no such new diagnostic has been implemented, and no solve requested.

## Gate-only factorial benchmark ready for user execution

New src/ww-gate-table-probe.lisp compiles private generated gate/control/energized
variants with explicit table arguments; retains synchronization and compares
one worker with four two-worker sharing combinations. Uses existing endpoints
and synthetic inverted OPEN inputs for correctness; timing is settled gate
updates with hash folding off. No solve, production replacement or table rebinding.
Existing table timer now accepts optional KERNEL; original lookup default remains.
See baseline for four REPL commands and limits. Static checks passed, Lisp not run.
Receiver extension deferred because height helpers have additional memoized
read/write caches and an indirect staged query call requiring further audit.
Next obtain 15 GATE RESULT rows or first error; do not launch a solve.

## Missing retained solution handled

First gate run stopped before compilation/timing because SOLUTION-PATHS was NIL.
Removed that unnecessary prerequisite: staged start state is sufficient, with
optional retained endpoint. Header labels START-ONLY versus START-AND-SOLUTION;
two versus four inputs including synthetic OPEN inversions. Abort debugger,
reload gate helper, compile kernel, rerun. No solve needed. Prior text requiring
a retained solution is superseded; correctness checks remain in place.

## Gate factorial completed successfully

All 15 user cases passed checks. Start-only settled workload: median seconds
one worker 1.106171; two sharing both 2.762102; private static only 1.260531;
private codes only 2.617643; both private 0.554015. Synchronization unchanged.
Both private recovers ~2x scaling; code-map isolation helps clearly once static
is private, while codes-only improvement is inconsistent. Compiler output was
32 optimization notes, not failures. No full-search speedup claim. Baseline
contains all timings/limits; recommended next is receiver cache/write audit and
isolated comparison, without further gate runs or a full solve yet.

## Receiver factorial prepared

New src/ww-receiver-table-probe.lisp discovers/clones the audited receiver query
closure, passing the two fact/code snapshots explicitly. Unknown/indirect helpers
stop setup. Vertical-type memo is prefilled and private in every case; BASE uses
FIXED-BASE, so the location-elevation memo is not admitted. Production memo write
lifetime remains unproven. Uses synthetic all-gates-open/closed start copies and
ACTIVE inversions; prints actual outcomes and checks originals versus clones.
Fifteen cases as for gates; no solution needed. See baseline for commands/limits.
Static checks passed, awaiting user compilation/run. Gate helper unchanged.

## Receiver factorial completed successfully

All 15 cases passed; seed outcomes NIL/T/NIL/T cover blocked and active receiver.
Median seconds: one 1.729518; two sharing both 4.838301; static private 1.818136;
codes private 5.106949; both private 0.869065. Both private recovers ~1.99x
scaling, as in gate tests. Vertical memo was private/prefilled throughout.
Unused clone-argument style warnings are harmless. Exact printed closure was
omitted from paste. See baseline for all results and limits. Next recommended:
ownership/publication and opt-in integration proposal, not another isolated
rerun or unapproved production change/full solve.

## Integration design ready for review

New doc/parallel-snapshot-proposal.md defines off-by-default Claustro read views,
post-root-split publication, frozen writes, preserved synchronization, private
memo ownership and cleanup spanning partial worker startup. Lists unresolved
payload/worker-path audit and focused validation before any separately approved
full solve. No implementation has been made; next decision is approval to
implement this bounded experiment. User still runs ALL Lisp commands.


## Checkpoint review complete

The tracked production diff and new snapshot module were reviewed against the
ownership/lifecycle audit. No blocking issue identified in this review. No
production or helper source was changed after the four measured runs.
Documentation status introductions now reflect the completed validation.

Checkpoint inventory (retain all):
- Production: the 13 tracked modified files plus
  src/ww-worker-read-snapshots.lisp. This includes the ASDF component,
  generated read routing, write guards, default-off setting and worker cleanup.
- Focused regression helper: src/ww-worker-read-snapshot-tests.lisp, explicit
  load only; it is not an ASDF component.
- Reproducible diagnostics: ww-parallel-baseline, ww-parallel-phase-probe,
  ww-table-read-probe, ww-gate-table-probe and ww-receiver-table-probe under src/.
  All remain explicitly loaded helpers, absent from normal ASDF loading.
- Evidence/design: parallel-baseline.md, parallel-search-handoff.md,
  parallel-snapshot-audit.md and parallel-snapshot-proposal.md under doc/.

No disposable task-created scratch file was identified for deletion; diagnostic
helpers are retained evidence, not leftover temporary files. No paths moved or
deleted, no source reformatted, no commit made. The untracked module/helpers/docs
must be included when saving a Git checkpoint; tracked diff statistics omit them.

Static validation: git diff --check passed; tracked modified Lisp/ASDF sources
and the new snapshot module/tests have LF line endings. The retained older
ww-table-read-probe.lisp has CRLF; it was not rewritten during this review. Runtime validation remains the user's two focused passes and four
completed solves. Normal-load diagnostics were checked through the ASDF component
list, not by launching Lisp. Default-off general modes have not received broad
regression testing; this checkpoint is the bounded experiment, not a production
rollout. Next recommended action: save this reviewed state in Git with the
retained helpers and documentation. No further solve is needed for that action.

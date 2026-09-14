# Current status: serial comparison complete, continuation ready (2026-09-14)

All approved solves complete. Serial before/after A/B/B/A passed identical work,
identical paths and depth-33 replay. Before mean 103.1502 s, after 107.3993825 s:
+4.119% wall, +4.109% CPU; paired wall differences +4.251% and +3.986%.
Evidence supports roughly 4.1% bounded serial overhead, not isolated causation.
Parallel ON means: one worker 100.814285 s, two 54.874345 s (3 samples), four
35.017955 s (2 samples); four-worker efficiency 72.0% vs one parallel worker.
Default off. No more solves or implementation changes authorized. Read
parallel-efficiency-continuation.md and the final baseline/serial-plan entries.
Preserve diagnostics and artifacts/serial-01 worktrees/caches/reports. Main source
is still checkpoint 4421433; later documents/helper are uncommitted. A1 setup
metadata omission remains recorded. All Lisp remains user-run.

The historical handoff follows; latest appended entries supersede old statuses.

# Parallel graph-search diagnostics handoff

## Latest user result: reverse-order repeat pair complete -- PASS

Checkpoint 4421433 on main preserves implementation and initial four cases.
Two-worker OFF: 122.35287 and 121.2312 s. ON: 54.1521 and 55.116688 s.
One-worker OFF/ON: 100.391365 / 100.814285 s. All six full solves completed
and replayed depth-33 solutions successfully; focused suite passed twice.
Reverse pair reproduced 2.20x benefit with only 0.169% state-count difference.
Enabled mean two-worker time 54.634394 s gives 1.845x scaling / 92.3%
efficiency relative to one parallel worker (not true serial).

All authorized solves are complete. User requested a continuation prompt for
next phase. Recommend inspecting CPU topology and proposing four-worker enabled
scaling next; true-serial overhead needs a separate matched design. Neither
measurement is authorized yet. Default stays off; no general-domain safety claim.
All Lisp remains user-run. Source unchanged since checkpoint; documentation
updates after checkpoint remain uncommitted. Preserve diagnostics and evidence.

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

## CPU topology inspected; four-worker solve approval pending

Windows reports i9-14900K, 24 enabled physical cores and 32 logical processors;
Intel specifies 8 P-cores plus 16 E-cores. REPL affinity/worker placement remains
unknown. Proposed next measurement is ONE freshly staged four-worker snapshot-ON
full solve using the existing baseline helper and unchanged search controls.
The default root-task target remains 256 at two/four workers. See the final
parallel-baseline.md section for controls, metrics and a separate matched
true-serial design. Obtain solve approval before providing execution commands;
review the result before proposing another run. No Lisp launched, no source
changes, no diagnostics removed. Existing uncommitted evidence is preserved.

## Four-worker snapshot-ON measurement approved

User approved ONE full solve: claustro-snapshot-on-4-01. Commands supplied for
fresh staging, WW-SET threads=4, then snapshots=T, and the explicitly loaded
baseline helper. Preconditions check all branches, no probe, inactive lifecycle
flags and the unchanged default task target controls. The helper checks the
remaining baseline search controls and replays the best path outside timing.
Await user results before any further run; no Lisp execution by Codex.

## Latest result: four-worker snapshot ON -- PASS

claustro-snapshot-on-4-01 completed and replayed depth 33 successfully.
Wall 33.62575 s; CPU 111.03125 s; states 6,480,743; cycles 1,030,899.
Same 343 root tasks and approved controls; no worker donations.
Versus two-worker ON mean: 1.625x speedup, 38.45% less elapsed, but 4.264%
more states. Versus one-worker ON: 2.998x speedup / 74.95% worker-count
efficiency, not true-serial or physical-core efficiency. All seven full runs
passed replay; four-worker repeatability remains unmeasured. Full evidence
is appended to parallel-baseline.md. No additional solve authorized.
Recommend approval for one fresh two-worker ON control in the same session,
then review before considering a four-worker repeat. Default off; true-serial
comparison separate. Source and diagnostics unchanged; no Lisp by Codex.

## Two-worker snapshot-ON control approved; awaiting result

User approved ONE freshly staged two-worker ON control in the same REPL session,
label claustro-snapshot-on-2-03, following four-worker ON 33.62575 s. Use the
existing baseline helper and unchanged search/split/shard/donation controls;
set threads via WW-SET after STAGE, then enable snapshots. Replay remains
outside timing. Purpose: check the historical two-worker 54-55 s reference
under current conditions. Review the returned report before proposing any
further solve; no four-worker repeat is authorized. No Lisp launched or source
changed by Codex; all existing evidence and diagnostics preserved.

## Latest result: two-worker ON control -- PASS

claustro-snapshot-on-2-03 completed/replayed depth 33 successfully, with matching
controls: wall 55.354248 s, CPU 104.296875 s, states 6,228,618, cycles 991,269.
Same 343 root tasks, no donations. Time is +1.318% versus previous two-run ON
mean and +0.431% versus latest prior ON; the 54-55 s reference is reproduced.
Preceding four-worker run is 1.646x faster (39.25% less time) than this control,
with 4.048% more states. All eight full searches passed replay.
Recommended next: approval for ONE fresh four-worker ON repeat, label
claustro-snapshot-on-4-02, completing four/two/four. No solve currently authorized;
provide execution only after approval and review each result. Four-worker
repeatability remains unmeasured. Default off; true-serial question separate.
Full evidence in parallel-baseline.md; no Lisp/source changes or deletions.

## Four-worker snapshot-ON repeat approved; awaiting result

User approved ONE freshly staged four-worker ON repeat in the same REPL session,
label claustro-snapshot-on-4-02. This completes the proposed four/two/four
sequence after 33.62575 s (four) and 55.354248 s (two). Use the existing
explicitly loaded baseline helper, unchanged controls and replay outside timing.
STAGE first, WW-SET threads=4, then snapshots=T. Review the returned result
before proposing further work. No other solve is authorized. No Lisp launched,
source changed or diagnostics removed by Codex.

## Latest result: four-worker repeat PASS; scaling sequence complete

claustro-snapshot-on-4-02: wall 36.41016 s, CPU 120.828125 s, states 6,453,081,
cycles 1,026,785; matching controls, 343 root tasks, no donations, depth-33
replay PASS. Four/two/four times: 33.62575 / 55.354248 / 36.41016 s.
Repeat is +8.281% wall with 0.427% fewer states; GC wall increase 0.342 s
does not explain the 2.78441 s difference alone. Cause remains unisolated.
The additional four-worker gain repeats with appreciable timing variation.

ON means: one worker 100.814285 s (n=1), two 54.874345 s (n=3), four
35.017955 s (n=2). Four-worker speedup 2.879x / efficiency 72.0% relative
to one parallel worker, not true serial or physical cores. Two/four mean
speedup 1.567x; four has about 3.969% more mean states. All nine full solves
passed replay. All solve approvals are exhausted; default stays off.

Next recommendation: pause solves and prepare/review a matched true-serial
overhead comparison, preserving this checkout and using isolated references
and compiled caches. Execution requires separate approval. No broad ownership
or default change. Full evidence in parallel-baseline.md. Source remains at
4421433; documentation is uncommitted; diagnostics preserved; no Lisp by Codex.

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

## Continuation focus clarified by user

User requests next session investigate repeated selectors and guards. Updated
parallel-efficiency-continuation.md to prioritize per-read/per-update cost,
inlining and safe mode-selection/specialization alternatives. Measured 4.1%
serial overhead is not the sequential fraction of parallel execution and must
not be inserted into Amdahl's law. It does not establish the primary cause of
diminishing returns. CPU clarification: 8 P-cores (two hardware threads each)
plus 16 E-cores (one each) = 24 physical cores / 32 logical processors; actual
worker placement remains unverified. No new production edit or solve approved.

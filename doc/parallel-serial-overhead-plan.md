# Matched true-serial overhead comparison

Status: preparation approved; source review and neutral helper prepared.
No Lisp, staging, compilation or solve has been run by Codex. No measurement
is authorized yet. Existing checkout and diagnostics are preserved.

## Question and matched references

Does the complete snapshot implementation add measurable end-to-end overhead
to Claustro-Topo true-serial search (THREADS=0), where no snapshot is created?

- A, before: e89a246b206c7208918004ad782821eaa0b961d4.
- B, after: 44214337614130462360d82f0741d9a2fa77cdb1.

A is B's parent. Git comparison confirms all probs/ and tech/ contents are
identical. Dependency declarations are unchanged. Changed production files
are the snapshot integration, guards, selectors and parallel lifecycle work;
added diagnostics are not ASDF-loaded. This compares the whole implementation,
not the isolated cost of any individual branch or guard.

Serial-reachable differences: generated static reads select the worker table
or canonical STATIC-IDB; variable object-code reads call the inline selector;
FOLD-STORE/FOLD-REMOVE execute the inline static-write guard; missing-code
allocation and initialization/staging/solve entry points have freeze checks.
WW-SOLVE and DFS validate snapshot mode even when disabled. Worker creation,
copying, cold private worker memos and joins do not run in true serial.
Staging/compilation costs are excluded from the timing envelope; initialization
inside WW-SOLVE is included. No production change is proposed to remove checks.

## Isolation and preparation gate

Use two separate detached reference checkouts under a NEW uniquely named
artifacts/serial-overhead directory within this repository. Do not switch,
reset, stash, clean or overwrite the current main checkout. Preparation so far
has not created those checkouts; they are the next setup step after review.
Record each full commit id, tracked cleanliness and source manifest before
running. Keep generated files and caches inside the investigation directory.

Each measured case uses a fresh user-started SBCL 2.6.8 image with 4096 MiB
(or the same actual limit as all compared cases, explicitly recorded), same
normal .sbclrc, Quicklisp installation, compiler policy and dependency versions.
Current .sbclrc loads Quicklisp and four libraries; it does not load Wouldwork.
Set a unique WOULDWORK_INSTANCE and isolated ASDF_OUTPUT_TRANSLATIONS cache
before launching each image, including dependencies loaded by .sbclrc. Do not
change machine-wide environment, power policy, affinity or the user's live REPL.
Keep only one measured process active at a time. Freeze dependencies for the
comparison; record versions/source roots and relevant compiler settings.

Explicitly load the intended checkout's wouldwork.asd before Quickloading
Wouldwork. Verify ASDF:SYSTEM-SOURCE-DIRECTORY and a representative translated
FASL output path; do not trust current directory or Quicklisp auto-discovery.
The neutral helper checks the expected source root again before every timing.
Fresh caches/images prevent cross-version compiled code and stale generated
query functions. Both checkouts start with fresh instance-specific settings,
not copied vals.lisp from the current parallel session.

User-run setup first, without a solve: load/stage each version, set THREADS=0
through WW-SET, explicitly load the identical helper, verify source/cache roots,
settings and no profiling, and run SERIAL-BASELINE-PREFLIGHT. Review compilation
warnings/errors and printed roots before requesting timing approval. These
setup images are disposable; use fresh images for the eventual matched cases.
Do not give an expensive solve as a workaround for a failed preflight.

## Neutral helper

src/ww-serial-baseline.lisp is explicitly loaded, outside normal ASDF loading.
Use the exact same file bytes in A and B; record its SHA-256 before the series.
It has independent SERIAL-BASELINE names and does not replace existing helpers.
It derives from the established baseline envelope, with snapshot settings and
worker statistics removed from common reporting. FIND-SYMBOL checks the new
symbols without interning them into the old image. Expected snapshot code is
absent for A, present but disabled/unbound views cleared for B.

SERIAL-BASELINE-PREFLIGHT accepts the expected absolute directory pathname
(with trailing slash) and expected snapshot-code boolean (NIL for A, T for B).
It checks the loaded root, zero workers, all branches, no probe or active
parallel group, and absent/disabled snapshot flags/views. It never calls solve.
RUN-SERIAL-BASELINE additionally accepts a case label and checks the common
Claustro search controls before timing exactly one WW-SOLVE. It captures wall,
process CPU, allocation, GC counter, work, best depth/path and then replays
outside timing. Search errors propagate; unsuccessful completion/depth/replay
prints a warning and must be treated as a failed case, never as timing evidence.

Source inspection and LF/static delimiter checks are complete. Runtime loading,
compilation and preflight are UNTESTED until the user supplies results. Root and
symbol checks supplement Git manifests; they do not prove the commit identity
of arbitrary manually edited or redefined code.

## Proposed measurement, not yet authorized

Four full solves, A1 / B1 / B2 / A2, each freshly started and freshly staged,
reviewed one at a time. This gives two matched orders (A then B; B then A),
but is only an initial overhead check, not a precise statistical estimate.
No uncounted warmup solves. Use identical setup/restaging procedure for each
case; initialization warms technology caches only as normal staging does.
No forced GC or instrumentation is added. Keep ordinary output destination
and progress interval identical; record unrelated workload or interruptions.

Controls: Claustro-Topo, depth-first graph/min-length, cutoff 34, symmetry T,
randomization NIL, debug 0, probe NIL, all branches, THREADS=0. Snapshot option
is absent in A and NIL in B; do not try setting a nonexistent option in A.
Retain ordinary TIME, one BASELINE RESULT, valid depth-33 replay and all settings.
The historical serial result (7,714,094 states; 1,877,372 cycles; 7,044,735
duplicates; 623,614 symmetry-pruned) is a sanity reference, not a hard assertion.
Check A/B counts and paths first. If work differs, investigate semantics/order
before attributing the time difference to implementation overhead.

Report each B/A paired wall and CPU ratio, all four raw times, and mean B/mean A
minus one. Retain allocation and GC separately. If the difference is comparable
to within-version variation, report it as unresolved; do not assert zero overhead
or statistical significance. No arbitrary acceptance threshold is imposed.
Do not use worker timings, old serial wall time alone or current OFF/ON runs
as a substitute for this matched comparison. Additional repetitions or focused
fixed-work tests require a separate proposal and solve approval where applicable.

## Review and cleanup

Next decision: approve isolated checkout/setup preparation and receive the first
user-run no-solve loading/preflight step. After both preparations pass, obtain
separate approval for the four full solves and issue them individually.
Default remains OFF and ownership scope unchanged regardless of serial timing.
Keep all current investigation files. At agreed closeout, remove only the newly
created reference worktrees/generated caches using verified exact paths, after
retaining reports/manifests; no cleanup is requested or performed now.

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

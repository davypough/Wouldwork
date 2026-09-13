# Proposal: opt-in worker read snapshots for Claustro-Topo

Status: implementation APPROVED, implemented and user-validated (2026-09-13).
Two focused passes and all four full-search cases passed; two-worker ON/OFF
elapsed times were 54.1521 / 122.35287 s, with successful depth-33 replay.
Default remains off. The original proposal below is retained as a design
record; outstanding audit/approval language is superseded by
[the implementation audit](parallel-snapshot-audit.md) and the final validation
update. No additional solve is authorized or requested.

## Purpose and bounded scope

Gate and receiver factorial tests recover approximately 2x scaling when each
worker owns both its static-fact table and its object-code map, retaining their
synchronization flags. The first integration should ask whether that benefit
survives in ordinary search. It must not presume a specific overall speedup.

Add an off-by-default experimental setting restricted to Claustro-Topo's current
parallel graph search with no happenings. Reject other problems/modes explicitly
when enabled. This restriction limits the experiment; a problem-name check is
not a substitute for the writer and value-ownership checks below.

Keep original DEFGLOBAL tables authoritative. Introduce separate worker-special
read-view variables, bound explicitly inside each worker. Generated integer
reads select the worker view when present and the authoritative table otherwise.
SUBST-INT-CODE supplies the static-table read expression; CONVERT-PROP-LIST
supplies the variable-object code-map read expression. Do not globally change
DEFGLOBAL declarations or replace SBCL getters. Preserve table tests, sizing and
synchronization flags in every copy.

This first experiment redirects generated action/query/update reads. General
runtime conversion and metadata tables remain canonical, with writes frozen as
specified below. Residual shared runtime reads may limit scaling. Do not silently
broaden to every engine lookup before this experiment is measured. New table
selection overhead must be included in the disabled-mode regression comparison.

## Ownership and publication

1. Normal staging and INIT finish on the coordinator. INIT performs initial
   conversion, initialization actions, then a second conversion, followed by
   consistency checks and other setup. Freezing at initial compilation is too early.
2. GENERATE-ROOT-TASKS also finishes before snapshots are taken. It expands states
   and can expose new codes/memos. If task generation completes the search, do not
   allocate snapshots or enter the frozen phase.
3. Enter an explicit frozen-read phase before creating any worker, coordinated
   with the integer-registration lock. Finish snapshot construction in that phase.
   Each worker gets distinct static-fact and code-map table objects; record the
   source table identities, object-index high-water mark and problem generation.
4. Workers bind their view variables around the entire worker loop, including
   donated tasks. Tasks carry state only, not another worker's table view. The
   coordinator and replay use canonical tables outside that binding.
5. Freeze ends only after every created worker has joined, including errors,
   interruption and partial thread-creation failures. Discard all read views at
   that boundary; never reuse them on the next solve or restage.

Current PROCESS-PARTITIONED-PARALLEL starts its unwind cleanup after creating
threads. The integration must place creation AND joining under one cleanup scope,
request shutdown, wake waiting workers and join all created workers before ending
the frozen phase. Restore parallel-active state on all exits as part of that same
scope. This is necessary lifecycle work, not a performance optimization.

## Mutation rules

During the frozen phase:

- Reject REGISTER-DYNAMIC-OBJECT and CONVERT-DATABASES-TO-INTEGERS before changes.
- In CONVERT-TO-INTEGER and CONVERT-FLUENTLESS-PROP-TO-INTEGER, existing-code reads
  remain valid. Reject a missing-code allocation under INTEGER-LOCK before changing
  LAST-OBJECT-INDEX or either constant map. Include both allocation sites.
- Reject supported staging/reset/reinitialization paths before clearing/replacing
  any table. Do not support concurrent user EVAL, manual table mutation, reload or
  another solve in the experimental lifetime.
- A generated lookup of a missing code must raise a descriptive experiment error,
  identifying the object. Do not allocate a private code, fall back to a newly
  written canonical entry, or continue with different worker mappings.

These rules make dynamic registration unsupported in this experiment. They do not
remove that capability from ordinary mode. A failure is evidence that the frozen
lifetime assumption was wrong; stop and report it rather than suppressing it.

Inventory found canonical writers in ww-converter.lisp (association, conversion,
registration), ww-support.lisp (fluentless fallback), ww-preliminaries.lisp (reset),
and installation/init population. The older #+IGNORE association implementation
is not an active writer. Static conversion is called by coordinate INIT actions,
including beam-direct. Claustro's worker action/query closure still needs a
specific write audit before implementation is considered ready for REPL testing.

## Values and auxiliary caches

Distinct hash tables alone do not establish value ownership. STATIC-IDB values
include lists such as controller clauses and geometry records. Code-map keys can
also include structured constants. Before publishing snapshots:

- Inspect the actual key/value types and reachable destructive uses in the staged
  problem. Define a copying policy for the observed data that preserves EQL/EQUAL
  key semantics and symbol identity. Recursively copy owned mutable fact payloads;
  do not use COPY-TREE as a blanket copier for vectors, strings or structures.
- Reject unsupported payload types in the experiment until their policy is
  explicit. Do not copy an identity-sensitive code-map key into a different object
  and thereby change lookup semantics. Structured code-map keys require a
  read-only-key contract as well as appropriate equality semantics.
- Compare canonical identities/content and code index before/after the run as
  validation evidence. Such end checks supplement prevention/audit; they cannot
  retroactively make a concurrent mutation safe.

VERTICAL-TYPE-CACHE and LOCATION-ELEVATION-CACHE are special DEFPARAMETER tables
with lazy writes and reset-on-technology-load lifetime. The receiver microbenchmark
made the vertical cache private; a full-search experiment must address both.
Proposed control: each worker binds fresh private memo tables with the existing
test/settings. Misses compute through that worker's generated read view. This
preserves lazy behavior and avoids needing a complete prewarm; include cold-cache
cost in full timing. Confirm all paths into these helpers respect the binding,
including the indirect LOCATION-LEVEL call. Their cached payloads must follow
the same ownership audit. Do not change any other memo cache without evidence.

## Expected implementation sites

| Area | Planned change |
| --- | --- |
| ww-settings.lisp and setting validation | Experimental flag and worker-special read views |
| ww-converter.lisp | Generated read routing; freeze checks on association/registration/conversion |
| ww-support.lisp | Missing-code freeze check in fluentless conversion |
| ww-preliminaries.lisp / staging entry | Reject reset while frozen; clear experiment state at normal reset |
| ww-parallel.lisp | Publish views after split; bind per worker; cleanup covering startup and join |
| Focused test helper and investigation docs | Correctness, lifecycle, disabled-mode and timing evidence |

Keep functions small. A dedicated snapshot context/lifecycle helper is preferable
to embedding this logic in the existing long coordinator. No production edits
have been made for this proposal.

## Validation sequence (user-run only)

First implement and inspect; then provide exact REPL commands for reviewable tests:

1. Focused preparation test: distinct table identity, equal contents, preserved
   flags, correct key semantics and private memo identity. Check copies do not
   mutate canonical payloads and reject unsupported types.
2. Read correctness: generated functions with/without worker views agree on both
   outputs and state effects, including change detection and incremental hash
   bookkeeping. The earlier diagnostics disabled hash folding; these tests must
   not omit production hash behavior. Check missing facts and missing object codes.
3. Lifecycle tests with small bounded tasks: normal finish, worker error, partial
   startup failure, cancellation, second run and restaging after cleanup. Verify
   all workers joined, views/freeze released, no canonical writes, and guard errors
   occur before changing object indices or table entries. Exercise each writer
   guard directly on disposable test data; no expensive problem is needed.
4. Recheck the ordinary setting-off path on focused tests. No new whole solve is
   needed to catch basic correctness errors. Report all results before requesting
   the performance trial.
5. Only with separate user authorization: one-worker and two-worker Claustro
   baselines with the option off/on, same rebuilt code and staged problem settings,
   uninstrumented, fixed search parameters. Include snapshot/setup and memo misses
   in total elapsed time; report worker phase separately. Replay each resulting
   best path outside timing, expect depth 33, and retain work/CPU/allocation totals.
   The unresolved question is end-to-end scaling, not another isolated lookup ratio.

Rollback is disable the flag, join any active workers, and rebuild/reload as
required by generated code changes. No saved snapshot survives. Keep existing
diagnostic evidence until closeout; remove temporary diagnostic artifacts only
when the investigation/feature is finished.

## Implementation resolution (2026-09-13)

The user approved implementation and focused helpers; no repeat approval is
needed. Implemented as *worker-read-snapshots*, default NIL and reset by STAGE.
Source-backed ownership and complete bounded worker-path audit are recorded in
[parallel-snapshot-audit.md](parallel-snapshot-audit.md), including the exact
remaining shared reads/caches and unsupported concurrent user operations.

Two additional unsynchronized lazy movement memos, MOBILITY-ROUTE-KEYS and
TRAVERSAL-CANONICAL-FAMILIES, require fresh private worker tables alongside the
two vertical memos. The synchronized traversal caches retain their existing
read-only payload contracts. Finite cons/string payloads are copied recursively;
structured keys require EQUAL and unsupported identity-sensitive data is rejected.

Publication follows root generation; integer allocation and supported lifecycle
writers are guarded. Static support writes are rejected while frozen. Startup,
all joins and freeze release share one cleanup scope, including partial startup,
worker/coordinator errors and nonlocal exits. Source identity/content, object
index, generation and view contents are checked at teardown.

The focused helper tests actual generated routing, copy/key ownership, guard
errors, state effects/hash folding, bounded sequential/concurrent expansions,
private memo behavior and lifecycle/setting-off cases. No Lisp has been run by
Codex. Commands are at the end of parallel-baseline.md; full-search timing and
production safety remain unestablished until user-returned validation. Full
solves still require separate authorization.


## Validation update: first user run passed

The user completed all focused checks with T: two three-seed/ten-successor
read comparisons, concurrent reads/private memos/hashes and all 15 off/on
lifecycle cases. See the final entry in parallel-baseline.md. Earlier pending
first-execution language is superseded. Fresh restaging and the repeated focused
run are still pending; no full solve or end-to-end speedup is established.


## Validation update: second user run passed

The repeat suite returned T with identical read and lifecycle results; the
snapshot-enabled, phase and parallel-active ASSERTs also passed. Focused
validation is complete. Full-search performance and depth-33 replay with this
integration remain untested. The proposed four-case end-to-end comparison and
its controls are at the end of parallel-baseline.md; separate solve approval
is still pending. No further implementation change was made for these results.


## Full-search validation update

All four user-run cases passed depth-33 replay. One worker OFF/ON took
100.391365 / 100.814285 s; two workers OFF/ON took 122.35287 / 54.1521 s.
Enabled scaling is 1.86x; two-worker ON versus OFF improves by 2.26x with
nearly identical work. This is initial bounded end-to-end evidence, one run
per case; default remains off and general safety is not established. See the
end of parallel-baseline.md for full counts and limitations. No more solves
authorized; preserve diagnostic files pending agreed cleanup scope.

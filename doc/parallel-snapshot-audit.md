# Claustro-Topo worker read experiment: implementation audit

2026-09-13. Implemented and validated by the user at the REPL: two focused
suite passes and four full-search cases, all depth-33 replays successful.
Two-worker snapshots ON took 54.1521 s versus 122.35287 s OFF. This audit
covers the bounded Claustro-Topo configuration, not arbitrary Lisp effects or
general-domain production safety. No Lisp or solve was run by Codex.
Later validation sections retain the chronological record; the final update
supersedes earlier pending language.

## Scope and read routing

`*worker-read-snapshots*` defaults to NIL, is accepted by WW-SET, is displayed
with parallel settings, and is deliberately absent from persisted settings.
STAGE resets it. Enabling requires Claustro-Topo, planning, parallel depth-first
graph/min-length, no happenings, randomization or debug/probe. Arbitrary bound,
heuristic, pruning, invariant and solution-validation callbacks are rejected.
The registered mobility provider, four traversal builders, two configuration
providers and traversal parameter list are checked against the audited set.
Rebuild and restage before testing; unsupported manual redefinition/reload or
another user evaluation/search during the frozen phase is outside the contract.

SUBST-INT-CODE redirects generated static reads through the worker-special
static view. CONVERT-PROP-LIST emits the inline WORKER-OBJECT-CODE selector for
variable objects. Literal codes remain compiled constants. Canonical DEFGLOBAL
tables remain authoritative. Without a binding, generated code reads them as
before. A missing worker code signals WORKER-READ-SNAPSHOT-ERROR and never
allocates or consults a newly populated canonical map. Missing facts remain
ordinary absent GETHASH results, including the presence value.

## Complete worker-path inventory for the bounded configuration

| Path | Reads, mutation and ownership |
| --- | --- |
| PARALLEL-WORKER / WORKER-LOCAL-DFS | Task queue, local DFS stack, worker stats, shared bounds and shutdown. The entire loop, including donated tasks, is inside one worker binding. Tasks carry states, never read views. |
| EXPAND / GENERATE-CHILDREN | All installed action preconditions/effects, dynamic instantiation and followups use the rebuilt generated functions. Action/argument/type metadata is read, not populated. INIT actions are a separate list; DO-INIT-ACTION-UPDATES now rejects the frozen phase. |
| MOVE | -mobility-action calls movement/configuration providers indirectly. The admitted registry resolves to -mobility, -configuration-transition and -traversal with walking, stairs, jump and ladder builders. Each indirect target's generated queries inherit the worker binding. No enumeration creates a planning object. |
| Mobility destructive operations | MOBILITY-SORT-BY-ROUTE-KEY changes a freshly collected outer list. Route extension copies segments; accumulated route results use fresh lists. Canonical family/clause sorting copies its input. Ladder sorts a newly collected ladder list; configuration sorting operates on freshly collected transitions. These do not destructively modify static controller/geometry lists or code-map keys. |
| Box/jammer pickup, putdown and jamming | Reachability/visibility read static families/geometry and dynamic location, support and gate facts. Placement/support-motion/settling build local lists; updates replace dynamic fluent values and maintain bijective indices. No static payload is destructively edited. Claustro has no tray/fan/recorder instances; their neutral substrate hooks remain, without registration or recorder lifecycle writes. |
| Propagation | Plate occupancy, receiver beam/height/corridor/occlusion, gate control, blower and threat updates. The latter two have empty apparatus populations here. Control DNF and geometry are iterated/read; BASE/TOP/FIXED-BASE read state/static facts and type defaults. Beam relay/cut and recording hooks are neutral for these includes. Updates target the supplied state IDB and dynamically bound change/hash accumulators. |
| General state updates | ADD/DEL-INT-PROP-KEY and general bijective/complement/symmetric updates reach FOLD-STORE/FOLD-REMOVE. These now reject canonical static DB/IDB and the current worker's static view before a table mutation. They still maintain the ordinary or canonical split hash components. |
| Runtime conversion | CONVERT-TO-INTEGER and CONVERT-FLUENTLESS-PROP-TO-INTEGER retain canonical existing-code reads. Both missing-code branches reject under INTEGER-LOCK before incrementing LAST-OBJECT-INDEX or writing either map. CONVERT-TO-INTEGER-MEMOIZED may populate the already synchronized PROP-KEY-CACHE; values are integers and keys are read-only proposition lists. |
| Successor processing | Consistency, goal, graph hashing, symmetry, closed-shard insertion/reopening, solutions and progress. State hashes/symmetry slices belong to states; symmetry scratch caches have local dynamic lifetime. Shared closed/bound/solution/statistic operations retain their existing synchronization. The rejected extension callbacks cannot introduce new snapshot writers. Goal recording copies paths; it does not allocate object codes. |

Source inventory followed the recursive includes of problem-claustro-topo.lisp.
Coordinate derivation in beam-direct, -beam-los-coordinates,
-walkability-coordinates and -reachability-coordinates calls integer conversion
only from initialization actions. Their local geometry-table construction is
not worker execution. REGISTER-DYNAMIC-OBJECT's technology caller belongs to
beam-crossing, which is not in this include closure. Both general conversion
allocation sites remain guarded rather than assuming they never miss.

## Payload/key policy and memo lifetime

Snapshots preserve hash test, size, rehash settings and synchronization flag.
Current authored/derived facts use symbol/number/character atoms and finite
cons trees: controller clauses, coordinates, barrier records and traversal
families. The copier owns every cons and string recursively and retains atom
identity. Other vectors, structures, tables and circular payloads are rejected.
This is an explicit admission policy, not COPY-TREE on arbitrary data.

Structured keys are admitted only with EQUAL, and only for the same finite
cons/string grammar. Ordinary vectors are not copied under EQUAL because their
identity participates in equality. Structured EQ/EQL keys are rejected. Thus a
copied object-code key still matches the original by the source table's test.
All consumers of the admitted structured constants use structural contents;
none depends on cons sharing or mutates the original key. Copying may separate
shared subtrees, which is intentional under that contract. The live tables are
checked at publication; an unanticipated payload stops preparation.

Four lazy **unsynchronized** technology memos receive fresh worker tables:

- VERTICAL-TYPE-CACHE: misses read TYPES and select a quoted type-default list.
  The list is read-only (height/axis/base selectors), so only the memo table needs
  private ownership. Its global source is not changed.
- LOCATION-ELEVATION-CACHE: misses call the installed LOCATION-LEVEL indirectly.
  Claustro's definition calls BASE then FIXED-BASE, whose reads use the worker
  view. Values are numeric; no state-dependent elevation is memoized here.
- MOBILITY-ROUTE-KEYS: lazy route-to-print-string writes are also unsynchronized.
  Private table and new strings per worker; routes are read-only keys.
- TRAVERSAL-CANONICAL-FAMILIES: lazy family normalization writes are
  unsynchronized. Private table; normalization copies before sorting.

The last two extend the proposal because the complete movement audit found
actual reachable writers. Both private tables start cold on every worker group,
as do the two originally proposed memos. Miss cost belongs in eventual timing.

TRAVERSAL-SEGMENT-CACHE and TRAVERSAL-DEPENDENCY-KEY-CACHE already synchronize
writes unconditionally. They remain canonical. Segment lists and the state
projection values used in keys are read-only; mobility copies segments before
route extension. A cached immutable segment can retain a fact list from a
finished worker; that is safe immutable data sharing, not reuse of its table or
memo binding. No saved worker read view is published through these caches.
Other relation/type/fluent-index/integer-to-object lookups remain shared reads.
These residual shared accesses may limit speedup.

## Publication, prevention and teardown

INIT's two conversions and GENERATE-ROOT-TASKS finish first. An early root-phase
completion never calls the worker group and allocates no view. The group enters
the freeze under INTEGER-LOCK, records source identities, content copies,
LAST-OBJECT-INDEX and GOAL-CHAIN-STAGE-GENERATION, then constructs every view
before creating a thread. Preparation failure releases the freeze with no
worker started. Reference copies include the reverse object-code map.

Association, conversion, registration, initialization, installation, supported
staging/reset entries, WW-SET and solve entries reject a frozen phase. Guards on
missing-code allocation sit inside the existing integer lock. Bulk initialization
and staging are coordinator-only operations; concurrent user EVAL, direct
SETF/CLRHASH, source reload and arbitrary redefinition are explicitly unsupported.
The guards are not a sandbox for arbitrary Lisp code. They prevent the audited
worker writers and accidental use of the supported public lifecycle entries.

RUN-PARALLEL-WORKER-GROUP owns publication, thread creation and joins in one
UNWIND-PROTECT. Interrupts are deferred across acquisition/recording of cleanup
handles and final teardown. Ordinary joining is interruptible. Workers catch
errors, record the condition, request shutdown and broadcast under the queue
mutex. The coordinator re-signals after joins. Abnormal coordinator exit also
requests shutdown, broadcasts and joins **every created thread**, including
abnormally terminated ones, before releasing the freeze. Interrupt scopes follow the
[SBCL asynchronous cleanup pattern](https://www.sbcl.org/manual/#Asynchronous-Operations)
using SB-SYS:WITHOUT-INTERRUPTS and WITH-LOCAL-INTERRUPTS. The outer coordinator
scope restores PARALLEL-SEARCH-ACTIVE even for a root-generation failure.

Teardown checks canonical table identities/content, index/generation and each
worker's static/code contents, then drops views and releases the phase even if
verification signals. These checks supplement the writer prevention and source
audit; they do not establish safety retroactively. No worker is allowed to keep
running once the freeze is released. Preparation, copying, verification, cold
memos and cleanup are included in the worker-phase/total timings.

## Focused validation, before any performance trial

Load src/ww-worker-read-snapshot-tests.lisp explicitly. The helper never calls
SOLVE, DFS or GENERATE-ROOT-TASKS. It tests recursive ownership/key rejection,
actual installed FIXED-BASE redirection using disposable incomplete views,
missing facts/codes, direct writer guards, changing/settled gate and receiver
updates and full propagation with standard **and** canonical hash accumulation.
Four synthetic gate/receiver seeds are correctness inputs, not claimed reachable
states. Normal successor comparisons use the start and at most six first-level
children, compare action/instantiation/state/hash outputs and independently
recompute hashes. Two actual workers repeat these comparisons concurrently;
canonical private-memo sources must remain unchanged.

Lifecycle cases exercise normal finish, worker error, partial thread creation,
coordinator error, nonlocal exit, cooperative cancellation, another normal group
and preparation failure. A no-worker coordinator harness also checks early
completion/error cleanup without claiming to execute root generation. Setting-off
groups exercise the same cleanup with no views. These are bounded thread harness tasks; they are not an end-to-end DFS
correctness or performance claim. The subsequent user STAGE and repeat test
check reset/reinitialization after all joins. Tests can warm synchronized caches
and increment diagnostic counters; stage before any later performance experiment.

Initial static checks cover delimiters, top-level list/binding structure, writer
placement and diff whitespace. Lisp compilation/runtime checks are pending.
Exact commands are at the end of parallel-baseline.md. Stop at the first error
and return it; do not run a solve to bypass a failed assertion.

After focused results pass, the separately authorized question will be whether
one/two-worker uninstrumented full search improves with this whole opt-in
ownership policy, including setup/cleanup, while preserving work accounting and
depth-33 replay. Both on/off cases must use rebuilt generated code so selector
and static-write-guard overhead are included in the disabled-mode comparison.


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

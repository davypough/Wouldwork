Investigate repeated read selectors and write guards in Wouldwork at D:\quicklisp\local-projects\wouldwork, with the aim of reducing measured ordinary-mode overhead while preserving the audited snapshot safety contract.

First read doc/parallel-search-handoff.md, doc/parallel-snapshot-audit.md,
doc/parallel-baseline.md (especially its final scaling and serial results),
doc/parallel-serial-overhead-plan.md, and doc/parallel-snapshot-proposal.md
for design history. Inspect git status and preserve all existing work.
Checkpoint 4421433 on main contains the tested snapshot implementation;
subsequent documentation and src/ww-serial-baseline.lisp are uncommitted.

Workflow: you inspect/edit source; I run ALL Lisp commands at my REPL and
return results. Do not launch Lisp yourself. Provide approved runs one at a
time and review each result before continuing. ALL prior solve authorizations
are exhausted. New full solves and production changes need separate approval.
Start with source-only analysis and a concrete proposal, not more solves.

The experiment remains default-off, limited to audited Claustro-Topo parallel
DFS graph/min-length. Worker-private static facts and object-code maps preserve
synchronization, plus four private cold technology memos. Publication follows
root-task generation; guards freeze supported writes; cleanup joins every
created worker, including partial-start failure, before releasing snapshots.
STAGE resets snapshots off; enable afterward through WW-SET. Two focused
correctness/lifecycle suite passes and nine full comparison/scaling searches
passed depth-33 replay. Do not broaden defaults, ownership or supported modes.

Parallel wall seconds:
- One worker OFF 100.391365; ON 100.814285.
- Two OFF 122.35287, 121.2312; ON 54.1521, 55.116688, 55.354248.
- Four ON 33.62575, 36.41016; intervening two-worker control 55.354248.
Two ON mean 54.874345; four ON mean 35.017955. Four has about 4% more work,
1.567x speedup over two, and 2.879x/72.0% efficiency versus one PARALLEL worker,
not true serial or physical-core efficiency. Four-repeat wall rose 8.28% with
0.43% fewer states; remaining causes are unisolated. No four-worker OFF test.
CPU: i9-14900K, 24 enabled physical cores / 32 logical, 8 P + 16 E by Intel
specification. Actual affinity/core placement is unverified; no linear scaling
assumption. Root target 256 produced 343 tasks at depth 15 for two/four workers.

True-serial comparison is now complete (THREADS=0, snapshots absent/disabled):
A=e89a246b206c7208918004ad782821eaa0b961d4, pre-implementation;
B=44214337614130462360d82f0741d9a2fa77cdb1, tested implementation.
A1 103.828125 s; B1 108.241875; B2 106.55689; A2 102.472275.
Means A=103.1502, B=107.3993825: +4.119% wall and +4.109% CPU.
Forward/reverse paired increases are 4.251% and 3.986%. All four had identical
7,714,094 states, 1,877,372 cycles, 7,044,735 duplicates, 623,614 symmetry prunes,
identical solution paths and successful depth-33 replay. GC wall differences
are tiny. Evidence supports roughly 4.1% serial overhead in this configuration,
not a precise statistical estimate or proof of the responsible checks.
A1 setup metadata was omitted/unconfirmed; its runtime preflight passed.
A/B preflights and B1/B2/A2 setup reports matched compiler policy/dependencies.

Preserve artifacts/serial-01/: detached A/B worktrees (tracked-clean), private
caches, instance-specific generated files, scripts, manifests and result-a1.txt,
result-b1.txt, result-b2.txt, result-a2.txt. Same neutral helper bytes used in
both versions. SBCL 2.6.8, ASDF 3.3.1, 4096 MiB launch setting. Do not reuse
these measurement images/caches for an allegedly fresh run. ASDF LOAD-ASD alone
selected main incorrectly; the validated setup prepends an exact-name Wouldwork
searcher before Quicklisp discovery and asserts the source/cache roots. Preserve
the accidental untracked src/problem-serial-a-preflight.lisp as diagnostic evidence.
Do not reset, clean, delete worktrees or remove investigation files.

Requested next phase (selectors and guards are the priority):
1. Inspect serial hot-path selectors and guards in generated static reads,
   WORKER-OBJECT-CODE and FOLD-STORE/FOLD-REMOVE, plus mode/staging lifecycle.
   Propose a small, clear way to reduce ordinary-mode overhead while preserving
   worker ownership, runtime setting semantics and every necessary write guard.
   Consider generated-code specialization versus repeated dynamic selection;
   do not assume either is safe without auditing rebuild/restage transitions.
2. Keep that serial-cost question separate from remaining four-worker limits:
   residual shared tables/caches, scheduling, GC and exploration order. Identify
   what evidence would discriminate causes before proposing instrumentation.
3. Present the source-backed proposal, risks and focused user-run validation
   plan for approval before implementation; full timing solves separately.

Keep functions small, avoid LABELS/FLET, preserve LF in Lisp (CRLF previously
broke FORMAT continuation strings), and update documentation as findings change.
Diagnostics stay explicitly loaded outside normal ASDF loading. Ignore CLAUDE.md.

Interpretation to preserve: the measured 4.1% overhead in a serial run is NOT
an estimate of the inherently sequential fraction of parallel search. The
selectors/guards can execute concurrently inside workers. Reducing their cost
may improve absolute runtime without fixing diminishing scaling. Do not use
4.1% as an Amdahl-law serial fraction or claim it explains the four-worker
shortfall. The CPU has 24 physical cores: 8 P-cores with two hardware threads
each, plus 16 E-cores with one each, yielding 32 logical processors. Wouldwork
worker count is a software setting; Windows placement on those unequal/shared
resources is not established by worker count or process CPU utilization.

Begin by mapping which checks execute per lookup/update versus once per solve,
which are inlined, and what state makes each necessary. Inspect current source
and the A/B diff; if generated forms or disassembly are needed, prepare bounded
user-run inspection commands only. Compare alternatives that move selection
outside repeated operations or specialize generated code, including the cost
and correctness of changing modes after STAGE/WW-SET. Keep canonical writes,
missing-code errors, publication order, cleanup joins and private memos intact.
Return findings and a concrete proposed patch/validation design for approval;
do not implement production changes or launch additional timing runs yet.

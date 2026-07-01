# Benchmarks

Lightweight, quickly repeatable performance checks for xplainfi, meant as a
developer sanity check for performance regressions during ongoing work.

This directory is **developer tooling, not part of the package**: it is
listed in `.Rbuildignore`, so it is excluded from the built package /
`R CMD check` and is never installed. For systematic, large-runtime
comparisons see the separate
[xplainfi-benchmark](https://github.com/jemus42/xplainfi-benchmark)
project; the scripts here are deliberately small "does this change move the
needle?" checks.

Run outputs (`results/*.csv`, plots) are **local, machine-specific
artifacts and are git-ignored** -- only the `results/` directory is kept
(via `.gitkeep`). The run log's provenance columns (host, R/pkg version,
git branch + sha) let you compare *your own* runs over time; they are not
meaningful across machines.

## Benchmark Scripts

There are two benchmark scripts:

1. **`bench-sage-seq.R`** - Sequential SAGE wall-time on the current code, with a local run log
2. **`bench-cross-branch.R`** - Compare a method's wall-time on HEAD vs a base branch (regression check)

### Prerequisites

```r
pak::pak("bench")                # both scripts
pak::pak("DavisVaughan/cross")   # bench-cross-branch.R only
```

### Sequential SAGE wall-time (`bench-sage-seq.R`)

**Purpose:** Quickly and repeatably measure sequential `MarginalSAGE` /
`ConditionalSAGE` wall time, and track how it changes across commits.

**Run:**
```bash
make install && Rscript bench/bench-sage-seq.R   # measure the installed package
Rscript bench/bench-sage-seq.R                    # measure the working tree (load_all)
```

**What it does:**
- Times the selected SAGE variants on synthetic regression tasks at
  selectable scales (`small`, `medium`, `heavy`, `wide`), with
  `early_stopping = FALSE` so the work per cell is fixed and timings are
  comparable run to run.
- Forces sequential execution (`xplain_opt(sequential = TRUE)`).
- **Appends** one row per cell to `bench/results/bench-sage-seq-results.csv`,
  tagged with run provenance: timestamp, hostname, OS, R version,
  package version, how the package was loaded (`load_all` vs
  `installed`), git branch + short SHA + dirty flag, learner,
  `check_interval`, and a result fingerprint for eyeballing correctness
  against a known-good run.

**Knobs (environment variables):**
- `BENCH_SCALES` - comma-separated subset of `small,medium,heavy,wide` (default `small,medium`)
- `BENCH_METHODS` - subset of `MarginalSAGE,ConditionalSAGE` (default both)
- `BENCH_LEARNER` - mlr3 regression learner id (default `regr.ranger`; e.g. `regr.lm` for cheap predictions)
- `BENCH_CHECK_INTERVAL` - integer, or `max` (= `n_perm`, one big checkpoint) (default `1`)
- `BENCH_REPS` - timing repetitions per cell; the minimum wall time is reported (default `1`)
- `BENCH_SEED` - RNG seed for task + permutation generation (default `20260630`)
- `BENCH_USE_INSTALLED=1` - force the installed package instead of the working tree

The `wide` scale plus `BENCH_LEARNER=regr.lm BENCH_CHECK_INTERVAL=max`
isolates coalition bookkeeping cost from prediction cost; the default
`regr.ranger` / `check_interval=1` is the realistic, prediction-dominated
regime.

The fingerprint should match bitwise across runs with the same seed on
the same code; a changed fingerprint flags a behavioural change, while
`wall_s` tracks the performance impact.

### Cross-branch regression check (`bench-cross-branch.R`)

**Purpose:** Catch a performance regression before merging by running the
same method benchmark on HEAD and a base branch (default `main`) via
`cross::bench_branches`, and reporting the speedup.

**Requirements:**
- Git tree **must be clean** -- `cross` checks out each branch and rebuilds.

**Run:**
```bash
Rscript bench/bench-cross-branch.R                 # PFI, HEAD vs main
BENCH_METHOD=MarginalSAGE Rscript bench/bench-cross-branch.R
```

**What it does:**
- Runs the chosen method (`task/learner/measure/resampling`, method-specific
  params at their defaults) on HEAD and the base branch and prints the
  `bench::mark` table plus a HEAD-vs-base speedup summary.
- Writes a scalar summary to `bench/results/bench-cross-branch-<timestamp>.csv`
  (local, git-ignored).

**Knobs (environment variables):**
- `BENCH_BASE` - base branch to compare HEAD against (default `main`)
- `BENCH_METHOD` - exported method class: `PFI`, `CFI`, `RFI`, `LOCO`, `LOCI`, `MarginalSAGE`, `ConditionalSAGE`, ... (default `PFI`)
- `BENCH_N` - task rows (default `1000`)
- `BENCH_ITERS` - `bench::mark` iterations per branch (default `5`)

## Files

- **`bench-sage-seq.R`**: Sequential SAGE wall-time on the current code, with a provenance-tagged local run log (`results/bench-sage-seq-results.csv`)
- **`bench-cross-branch.R`**: HEAD-vs-base regression check for any importance method, using `cross::bench_branches()`
- **`results/`**: local run logs (git-ignored except `.gitkeep`)
- **`README.md`**: This file
- **`.gitignore`**: Keeps run outputs local

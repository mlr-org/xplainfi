# Benchmarks

Runtime comparison of the `samples_per_row` / `n_synth` refactor against the
pre-refactor state. These are local dev-time perf checks, **not** unit tests.

## What it measures

CFI and `ConditionalSAGE` `compute()` time with `ConditionalARFSampler`,
before and after the refactor that made the samplers take `samples_per_row`
and pass it to `arf::forge()` as `n_synth` (instead of replicating evidence
rows and calling `arf::forge(n_synth = 1)` per row). The ARF model is fit
once per build and shared across timed iterations, so only the per-`compute`
sampling cost is timed.

## Running it (BIPS cluster)

```bash
sbatch bench/run.slurm
```

`run.slurm` is self-contained and works on network-isolated compute nodes:

1. `git worktree add` the pre-refactor commit (`aa88c624`) and current HEAD
2. `R CMD INSTALL` each into a throwaway library — dependencies resolve from
   the shared user library, so **no network is required** (this is why it
   does not use `cross`, whose `pak`-based install needs internet)
3. `bench/timing.R` times each method's `compute()` via `bench::mark` against
   each build
4. A summary table is printed and per-run results land in
   `bench/.results/*.rds` (gitignored)

Output: `bench/slurm-<jobid>.{out,err}` (gitignored).

## Running it interactively

On a networked machine (head node, laptop) `cross::bench_branches()` is the
nicer tool for ad-hoc exploration — it installs each branch via `pak` and
returns a tidy `bench` tibble. Example:

```r
cross::bench_branches(branches = c("aa88c624", "HEAD"), {
  library(xplainfi); library(mlr3); library(mlr3learners)
  set.seed(42L)
  task = sim_dgp_correlated(n = 1000L, r = 0.7)
  sampler = ConditionalARFSampler$new(task, verbose = FALSE)
  cfi = CFI$new(task, lrn("regr.ranger", num.trees = 100L),
                msr("regr.mse"), sampler = sampler, n_repeats = 30L)
  bench::mark(cfi$compute(), iterations = 3L, check = FALSE)
})
```

The committed automation deliberately does **not** depend on `cross` so it
can run unattended on the cluster.

## Reference commit

`aa88c624` — last commit before the `samples_per_row` plumbing began. Edit
`REF_PRE` in `run.slurm` to compare against a different baseline.

## Excluded from package build

The whole `bench/` directory is in `.Rbuildignore` so it never ships in the
package tarball.

#!/usr/bin/env Rscript
# bench/bench-cross-branch.R
#
# Cross-branch performance regression check: run the SAME importance-method
# benchmark on the current branch (HEAD) and a base branch (default `main`)
# via cross::bench_branches, and report the speedup. Use during development
# to catch a performance regression before merging.
#
# `cross` is a developer tool, not a package dependency:
#   pak::pak("DavisVaughan/cross")
# It checks out each branch and rebuilds the package, so the git working
# tree MUST be clean (commit or stash first).
#
# Usage from repo root:
#   Rscript bench/bench-cross-branch.R
#
# Knobs (env vars; read inside the benchmarked block so cross's per-branch
# subprocesses see them too):
#   BENCH_BASE    base branch to compare HEAD against        (default: main)
#   BENCH_METHOD  exported method class: PFI, CFI, RFI, LOCO,
#                 LOCI, MarginalSAGE, ConditionalSAGE, ...    (default: PFI)
#   BENCH_N       task rows                                   (default: 1000)
#   BENCH_ITERS   bench::mark iterations per branch           (default: 5)
#
# Output:
#   - stdout: full bench result + a HEAD-vs-base speedup summary
#   - bench/results/bench-cross-branch-<timestamp>.csv (local, git-ignored)

library(cross)

base_branch = Sys.getenv("BENCH_BASE", "main")
method_id = Sys.getenv("BENCH_METHOD", "PFI")

cli::cli_h1("Cross-branch benchmark: {method_id} on HEAD vs {base_branch}")
cli::cli_alert_warning("Git tree must be clean -- cross checks out branches and rebuilds.")

# The block runs in a fresh R session per branch (loading that branch's
# xplainfi), so it must be self-contained: no outer R variables, and knobs
# are read from the environment, which the subprocesses inherit.
results = cross::bench_branches(
  {
    library(xplainfi)
    library(mlr3)
    library(mlr3learners)
    library(bench)

    method_id = Sys.getenv("BENCH_METHOD", "PFI")
    n_obs = as.integer(Sys.getenv("BENCH_N", "1000"))
    iters = as.integer(Sys.getenv("BENCH_ITERS", "5"))

    # Generic construction: every FeatureImportanceMethod accepts
    # task/learner/measure/resampling; method-specific parameters take
    # their defaults, which is fine for a wall-time regression check.
    method_class = getExportedValue("xplainfi", method_id)

    set.seed(42)
    task = sim_dgp_independent(n = n_obs)
    learner = lrn("regr.ranger", num.trees = 100)
    measure = msr("regr.mse")
    resampling = rsmp("holdout")

    bench::mark(
      {
        imp = method_class$new(
          task = task,
          learner = learner,
          measure = measure,
          resampling = resampling
        )
        imp$compute()
      },
      check = FALSE,
      iterations = iters,
      memory = TRUE
    )
  },
  branches = base_branch
)

cli::cli_h2("Results")
print(results)

# --------------------------------------------------------------------
# HEAD-vs-base speedup. cross labels each row by its branch; the base row
# is `branch == base_branch`, HEAD is the other. Robust to whatever label
# cross gives the current branch (name or detached HEAD).
# --------------------------------------------------------------------
cli::cli_h2("Summary")
base_row = results[results$branch == base_branch, ]
head_row = results[results$branch != base_branch, ]

if (nrow(base_row) == 1L && nrow(head_row) == 1L) {
  base_median = as.numeric(base_row$median)
  head_median = as.numeric(head_row$median)
  speedup = base_median / head_median

  cli::cli_alert_info("{base_branch} median: {format(base_row$median)}")
  cli::cli_alert_info("HEAD ({head_row$branch}) median: {format(head_row$median)}")

  if (speedup > 1.05) {
    cli::cli_alert_success("HEAD is {round(speedup, 2)}x faster than {base_branch}")
  } else if (speedup < 0.95) {
    cli::cli_alert_danger("HEAD is {round(1 / speedup, 2)}x SLOWER than {base_branch}")
  } else {
    cli::cli_alert_success("Performance equivalent to {base_branch} (within 5%)")
  }
} else {
  cli::cli_alert_warning("Could not identify a unique HEAD/base row pair; see table above.")
}

# --------------------------------------------------------------------
# Local run log (git-ignored). Keep only scalar columns so fwrite is happy.
# --------------------------------------------------------------------
tryCatch(
  {
    results_dir = file.path("bench", "results")
    if (!dir.exists(results_dir)) {
      dir.create(results_dir, recursive = TRUE)
    }
    summ = data.table::data.table(
      datetime = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      method = method_id,
      base = base_branch,
      n = as.integer(Sys.getenv("BENCH_N", "1000")),
      branch = results$branch,
      median_s = as.numeric(results$median),
      mem_alloc_bytes = as.numeric(results$mem_alloc)
    )
    path = file.path(
      results_dir,
      sprintf(
        "bench-cross-branch-%s.csv",
        format(Sys.time(), "%Y%m%dT%H%M%S")
      )
    )
    data.table::fwrite(summ, path)
    cli::cli_alert_info("Saved {path}")
  },
  error = function(e) cli::cli_alert_warning("Could not write results CSV: {conditionMessage(e)}")
)

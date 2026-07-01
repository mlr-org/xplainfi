#!/usr/bin/env Rscript
# bench/bench-sage-seq.R
#
# Simple, quickly repeatable wall-time benchmark for SEQUENTIAL SAGE
# (MarginalSAGE + ConditionalSAGE). Built to measure the impact of
# code changes over time: every run appends rows to a tracked CSV log
# tagged with enough provenance (timestamp, host, R/pkg version, git
# branch + commit + dirty flag, learner, check_interval) to compare
# runs after the fact.
#
# Two complementary regimes, selected by the learner + check_interval:
#   * Realistic (default): learner = regr.ranger, check_interval = 1.
#     Prediction cost dominates, as in real use.
#   * Isolation: learner = regr.lm, check_interval = max. Cheap
#     predictions + one big checkpoint, so coalition bookkeeping (the
#     O(n^2) -> closed-form accumulation change) dominates the signal.
#
# Usage from repo root:
#   make install && Rscript bench/bench-sage-seq.R   # uses installed pkg
#   Rscript bench/bench-sage-seq.R                    # uses load_all() if pkgload present
#
# Knobs (env vars):
#   BENCH_SCALES         subset of {small,medium,heavy,wide}     (default: small,medium)
#   BENCH_METHODS        subset of {MarginalSAGE,ConditionalSAGE}(default: both)
#   BENCH_LEARNER        mlr3 regr learner id                    (default: regr.ranger)
#   BENCH_CHECK_INTERVAL integer, or "max" (= n_perm, one batch) (default: 1)
#   BENCH_REPS           timing reps per cell, min wall reported (default: 1)
#   BENCH_SEED           RNG seed for task + permutations        (default: 20260630)
#   BENCH_USE_INSTALLED  set to force installed pkg over load_all
#
# Output:
#   - stdout: per-cell timing + summary table
#   - bench/results/bench-sage-seq-results.csv : appended run log (one row per cell)

suppressPackageStartupMessages({
	library(mlr3)
	library(mlr3learners)
	library(data.table)
})

# --------------------------------------------------------------------
# Load xplainfi: working tree (dev) by default, installed on request.
# load_all() is the honest choice for measuring uncommitted changes;
# the git metadata below records exactly which tree was measured.
# --------------------------------------------------------------------
use_installed = nzchar(Sys.getenv("BENCH_USE_INSTALLED"))
if (!use_installed && requireNamespace("pkgload", quietly = TRUE)) {
	pkgload::load_all(".", quiet = TRUE)
	pkg_load_mode = "load_all"
} else {
	library(xplainfi)
	pkg_load_mode = "installed"
}

# --------------------------------------------------------------------
# Configuration
# --------------------------------------------------------------------
SEED = as.integer(Sys.getenv("BENCH_SEED", "20260630"))
REPS = as.integer(Sys.getenv("BENCH_REPS", "1"))
LEARNER_ID = Sys.getenv("BENCH_LEARNER", "regr.ranger")
CHECK_INTERVAL_RAW = Sys.getenv("BENCH_CHECK_INTERVAL", "1")

ALL_SCALES = list(
	small = list(n = 300L, p = 6L, n_perm = 10L, n_samples = 30L),
	medium = list(n = 500L, p = 8L, n_perm = 20L, n_samples = 50L),
	heavy = list(n = 2000L, p = 20L, n_perm = 50L, n_samples = 100L),
	# `wide` is tuned to surface the accumulation cost: coalition
	# bookkeeping scales with (coalitions)^2 while data expansion +
	# prediction scale with coalitions * n_test * n_samples. Many
	# features x permutations but FEW test rows and samples makes the
	# coalition count dominate, so the O(n^2) -> closed-form change is
	# visible end-to-end (pair it with BENCH_LEARNER=regr.lm and
	# BENCH_CHECK_INTERVAL=max).
	wide = list(n = 150L, p = 25L, n_perm = 100L, n_samples = 5L)
)
scale_sel = trimws(strsplit(Sys.getenv("BENCH_SCALES", "small,medium"), ",")[[1]])
SCALES = ALL_SCALES[scale_sel]
if (anyNA(names(SCALES)) || length(SCALES) == 0) {
	stop("BENCH_SCALES must be a subset of: ", paste(names(ALL_SCALES), collapse = ", "))
}

method_sel = trimws(strsplit(Sys.getenv("BENCH_METHODS", "MarginalSAGE,ConditionalSAGE"), ",")[[1]])
if (!all(method_sel %in% c("MarginalSAGE", "ConditionalSAGE"))) {
	stop("BENCH_METHODS must be a subset of: MarginalSAGE, ConditionalSAGE")
}

# Resolve check_interval per scale: "max" means one big checkpoint
# (all n_perm permutations evaluated together).
resolve_check_interval = function(n_perm) {
	if (identical(CHECK_INTERVAL_RAW, "max")) n_perm else as.integer(CHECK_INTERVAL_RAW)
}

# --------------------------------------------------------------------
# Run provenance: captured once, attached to every result row so the
# CSV log is self-describing and runs stay comparable.
# --------------------------------------------------------------------
git = function(args) {
	out = tryCatch(
		suppressWarnings(system2("git", args, stdout = TRUE, stderr = FALSE)),
		error = function(e) NA_character_
	)
	if (length(out) == 0) NA_character_ else paste(out, collapse = " ")
}
git_dirty = {
	status = git(c("status", "--porcelain", "--untracked-files=no"))
	!is.na(status) && nzchar(status)
}

run_meta = list(
	run_id = format(Sys.time(), "%Y%m%dT%H%M%S"),
	datetime = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
	hostname = Sys.info()[["nodename"]],
	os = paste(Sys.info()[["sysname"]], Sys.info()[["release"]]),
	r_version = paste(R.version$major, R.version$minor, sep = "."),
	pkg_version = as.character(utils::packageVersion("xplainfi")),
	pkg_load = pkg_load_mode,
	git_branch = git(c("rev-parse", "--abbrev-ref", "HEAD")),
	git_sha = git(c("rev-parse", "--short", "HEAD")),
	git_dirty = git_dirty,
	benchmark = "sage-seq",
	backend = "sequential",
	learner = LEARNER_ID
)

# --------------------------------------------------------------------
# Synthetic task: Gaussian features, sparse-ish linear signal, noise.
# Sin-spaced coefficients so feature signal is non-uniform. A plain
# regr.lm is the (near-)correct model here, hence a fair "good enough
# but cheap" learner for the isolation regime.
# --------------------------------------------------------------------
make_task = function(n, p, seed) {
	set.seed(seed)
	X = matrix(rnorm(n * p), nrow = n, ncol = p)
	colnames(X) = paste0("x", seq_len(p))
	beta = sin(seq_len(p))
	y = as.vector(X %*% beta) + rnorm(n)
	dt = as.data.table(X)
	dt$y = y
	as_task_regr(dt, target = "y", id = sprintf("bench_n%d_p%d", n, p))
}

# ranger gets a modest tree count; other learners (regr.lm, ...) take
# their defaults.
mk_learner = function() {
	if (grepl("ranger", LEARNER_ID, fixed = TRUE)) {
		lrn(LEARNER_ID, num.trees = 50L)
	} else {
		lrn(LEARNER_ID)
	}
}

# Stable per-result fingerprint for eyeballing correctness against a
# known-good run (same seed + sequential => should match bitwise).
fingerprint = function(imp_dt) {
	v = imp_dt[order(feature), importance]
	paste(format(round(v, 4), nsmall = 4), collapse = ",")
}

# --------------------------------------------------------------------
# One benchmark cell. early_stopping = FALSE so the work is fixed
# (n_perm permutations) and timings are directly comparable run to run.
# --------------------------------------------------------------------
bench_one = function(method, scale_name) {
	cfg = SCALES[[scale_name]]
	ci = resolve_check_interval(cfg$n_perm)
	task = make_task(cfg$n, cfg$p, SEED)
	learner = mk_learner()

	sage = switch(
		method,
		MarginalSAGE = MarginalSAGE$new(
			task = task,
			learner = learner,
			n_permutations = cfg$n_perm,
			n_samples = cfg$n_samples,
			early_stopping = FALSE
		),
		ConditionalSAGE = ConditionalSAGE$new(
			task = task,
			learner = learner,
			n_permutations = cfg$n_perm,
			n_samples = cfg$n_samples,
			early_stopping = FALSE
		)
	)

	walls = numeric(REPS)
	fp = NA_character_
	for (r in seq_len(REPS)) {
		set.seed(SEED) # seed the caller-side permutation generator each rep
		walls[r] = system.time(sage$compute(check_interval = ci))[["elapsed"]]
		if (r == 1L) fp = fingerprint(sage$importance())
	}

	data.table(
		method = method,
		scale = scale_name,
		n = cfg$n,
		p = cfg$p,
		n_perm = cfg$n_perm,
		n_samples = cfg$n_samples,
		check_interval = ci,
		reps = REPS,
		wall_s = min(walls),
		fingerprint = fp
	)
}

# --------------------------------------------------------------------
# Run grid (force sequential regardless of any ambient backend).
# --------------------------------------------------------------------
xplain_opt(sequential = TRUE)

cat(sprintf(
	"Host: %s | R %s | xplainfi %s (%s) | git %s@%s%s\n",
	run_meta$hostname,
	run_meta$r_version,
	run_meta$pkg_version,
	run_meta$pkg_load,
	run_meta$git_branch,
	run_meta$git_sha,
	if (run_meta$git_dirty) " +dirty" else ""
))
cat(sprintf(
	"Learner: %s | check_interval: %s | scales: %s | reps: %d | seed: %d\n\n",
	LEARNER_ID,
	CHECK_INTERVAL_RAW,
	paste(names(SCALES), collapse = ", "),
	REPS,
	SEED
))

grid = expand.grid(
	method = method_sel,
	scale = names(SCALES),
	stringsAsFactors = FALSE
)

rows = vector("list", nrow(grid))
for (i in seq_len(nrow(grid))) {
	g = grid[i, ]
	cat(sprintf("[%d/%d] %-16s | %-6s ... ", i, nrow(grid), g$method, g$scale))
	flush.console()
	rows[[i]] = bench_one(g$method, g$scale)
	cat(sprintf("%8.2fs\n", rows[[i]]$wall_s))
}

res = rbindlist(rows)
# Prepend run metadata columns to every result row.
res = cbind(as.data.table(run_meta), res)

cat("\n=== Results ===\n")
print(res[, .(
	method,
	scale,
	check_interval,
	wall_s = round(wall_s, 2),
	fingerprint = substr(fingerprint, 1, 24)
)])

# --------------------------------------------------------------------
# Append to the tracked run log (create with header if absent).
# --------------------------------------------------------------------
results_dir = file.path("bench", "results")
if (!dir.exists(results_dir)) {
	dir.create(results_dir, recursive = TRUE)
}
log_path = file.path(results_dir, "bench-sage-seq-results.csv")
fwrite(res, log_path, append = file.exists(log_path))
cat("\nAppended", nrow(res), "rows to", log_path, "\n")

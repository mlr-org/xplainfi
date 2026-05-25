#!/usr/bin/env Rscript
# scripts/bench-sage-parallel.R
#
# Test-drive xplainfi SAGE parallelization across task sizes and
# parallel backends. Compares wall time of sequential vs mirai-medium
# vs mirai-heavy for MarginalSAGE and ConditionalSAGE (ARF sampler) on
# medium and heavy synthetic tasks.
#
# Usage from repo root:
#   module load R
#   make install
#   Rscript scripts/bench-sage-parallel.R
#
# Output: results table to stdout plus
#   scripts/bench-sage-parallel-<timestamp>.csv
#
# Runtime: 12 cells (2 methods x 2 scales x 3 backends). Heavy +
# ConditionalSAGE+ARF dominates; total wall time typically 15-40 min
# on a workstation. Edit SCALES / BACKENDS below to taste.

suppressPackageStartupMessages({
	library(xplainfi)
	library(mlr3)
	library(mlr3learners)
	library(mirai)
	library(data.table)
})

# --------------------------------------------------------------------
# Configuration
# --------------------------------------------------------------------

# Task scales: medium and heavy. n = obs, p = features, n_perm =
# SAGE permutations, n_samples = marginalization sample size.
SCALES = list(
	medium = list(n = 500L,  p = 8L,  n_perm = 20L, n_samples = 50L),
	heavy  = list(n = 2000L, p = 20L, n_perm = 50L, n_samples = 100L)
)

# Mirai daemon counts. Heavy auto-sized to a safe local cap.
N_MIRAI_MEDIUM = 4L
N_MIRAI_HEAVY  = min(parallel::detectCores() - 2L, 12L)

# Backends: each entry has setup() returning prev state and
# teardown(prev) restoring it. Sequential is always the reference.
BACKENDS = list(
	sequential = list(
		setup = function() {
			prev = xplain_opt(sequential = TRUE)
			list(sequential = prev)
		},
		teardown = function(prev) {
			xplain_opt(sequential = prev$sequential)
		}
	),
	mirai_medium = list(
		setup = function() {
			prev = xplain_opt(sequential = FALSE)
			mirai::daemons(N_MIRAI_MEDIUM)
			list(sequential = prev)
		},
		teardown = function(prev) {
			mirai::daemons(0L)
			xplain_opt(sequential = prev$sequential)
		}
	),
	mirai_heavy = list(
		setup = function() {
			prev = xplain_opt(sequential = FALSE)
			mirai::daemons(N_MIRAI_HEAVY)
			list(sequential = prev)
		},
		teardown = function(prev) {
			mirai::daemons(0L)
			xplain_opt(sequential = prev$sequential)
		}
	)
)

# --------------------------------------------------------------------
# Synthetic task (Gaussian features, sparse linear y, noise)
# --------------------------------------------------------------------

make_task = function(n, p, seed = 20260525L) {
	set.seed(seed)
	X = matrix(rnorm(n * p), nrow = n, ncol = p)
	colnames(X) = paste0("x", seq_len(p))
	# Sin-spaced coefficients so signal is non-uniform across features.
	beta = sin(seq_len(p))
	y = as.vector(X %*% beta) + rnorm(n)
	dt = as.data.table(X)
	dt$y = y
	mlr3::as_task_regr(dt, target = "y", id = sprintf("bench_n%d_p%d", n, p))
}

mk_learner = function() lrn("regr.ranger", num.trees = 50L)

# Stable per-result fingerprint so a parallel cell can be eyeballed
# against the sequential one. MarginalSAGE under fixed seed should
# match bitwise across backends; ConditionalSAGE matches only
# statistically (RNG stream layout differs across workers).
fingerprint = function(imp_dt) {
	v = imp_dt[order(feature), importance]
	paste(format(round(v, 4), nsmall = 4), collapse = ",")
}

# --------------------------------------------------------------------
# Single benchmark cell
# --------------------------------------------------------------------

bench_one = function(method, scale_name, backend_name) {
	cfg = SCALES[[scale_name]]
	task = make_task(cfg$n, cfg$p)
	learner = mk_learner()

	sage = switch(method,
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

	bk = BACKENDS[[backend_name]]
	prev = bk$setup()
	on.exit(bk$teardown(prev), add = TRUE)

	# Seed the caller-side permutation generator so MarginalSAGE
	# reproduces bit-identically across backends within a cell.
	set.seed(20260525L)
	wall = system.time(sage$compute())[["elapsed"]]

	list(
		method = method,
		scale = scale_name,
		backend = backend_name,
		wall_s = wall,
		fingerprint = fingerprint(sage$importance())
	)
}

# --------------------------------------------------------------------
# Run grid
# --------------------------------------------------------------------

grid = expand.grid(
	method = c("MarginalSAGE", "ConditionalSAGE"),
	scale = names(SCALES),
	backend = names(BACKENDS),
	stringsAsFactors = FALSE
)
# Run sequential cells first per (method, scale) so speedup ratio
# computation has the reference available, and so early failures in
# parallel cells do not block the baseline numbers.
grid = grid[order(grid$method, grid$scale, grid$backend != "sequential"), ]

cat(sprintf("Host: %s | cores detected: %d\n",
	Sys.info()[["nodename"]], parallel::detectCores()))
cat(sprintf("Mirai medium = %d daemons | mirai heavy = %d daemons\n",
	N_MIRAI_MEDIUM, N_MIRAI_HEAVY))
cat(sprintf("Cells: %d (2 methods x 2 scales x 3 backends)\n\n", nrow(grid)))

results = vector("list", nrow(grid))
for (i in seq_len(nrow(grid))) {
	g = grid[i, ]
	cat(sprintf("[%2d/%2d] %-16s | %-6s | %-12s ... ",
		i, nrow(grid), g$method, g$scale, g$backend))
	flush.console()
	results[[i]] = tryCatch(
		bench_one(g$method, g$scale, g$backend),
		error = function(e) list(
			method = g$method, scale = g$scale, backend = g$backend,
			wall_s = NA_real_, fingerprint = NA_character_, err = conditionMessage(e)
		)
	)
	wall = results[[i]]$wall_s
	if (is.na(wall)) {
		cat("ERROR:", results[[i]]$err, "\n")
	} else {
		cat(sprintf("%7.1fs\n", wall))
	}
}

res_dt = rbindlist(lapply(results, function(r) {
	data.table(
		method = r$method,
		scale = r$scale,
		backend = r$backend,
		wall_s = r$wall_s,
		fingerprint = if (is.null(r$fingerprint)) NA_character_ else r$fingerprint
	)
}))

# Speedup vs sequential within each (method, scale).
res_dt[, speedup := wall_s[backend == "sequential"] / wall_s, by = .(method, scale)]

cat("\n=== Results ===\n")
print(res_dt[, .(method, scale, backend, wall_s = round(wall_s, 1),
	speedup = round(speedup, 2), fingerprint = substr(fingerprint, 1, 32))])

ts = format(Sys.time(), "%Y%m%dT%H%M%S")
csv_path = file.path("scripts", sprintf("bench-sage-parallel-%s.csv", ts))
fwrite(res_dt, csv_path)
cat("\nSaved:", csv_path, "\n")

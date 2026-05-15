# Time one importance method's $compute() for a single installed xplainfi build.
#
# Usage:
#   Rscript bench/timing.R <libpath> <method> <label>
#
#   libpath : library directory containing the xplainfi build to benchmark
#             (prepended to .libPaths() so `library(xplainfi)` loads it;
#             dependencies fall back to the shared user library)
#   method  : "cfi" or "sage"
#   label   : free-text tag for the output row, e.g. "pre" / "post"
#
# Writes a one-row RDS to bench/.results/<label>-<method>.rds and prints it.
# Self-contained: no network, no `cross`. The driver (bench/run.slurm) builds
# the two library directories with `R CMD INSTALL`.

args = commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L)
libpath = args[[1L]]
method = match.arg(args[[2L]], c("cfi", "sage"))
label = args[[3L]]

.libPaths(c(libpath, .libPaths()))

suppressPackageStartupMessages({
	library(xplainfi)
	library(mlr3)
	library(mlr3learners)
	library(bench)
})
lgr::get_logger("mlr3")$set_threshold("warn")

# Task / sampler / learner are constructed once and shared across the timed
# iterations, so the ARF *fit* is not re-timed — only the per-compute
# *sampling* cost, which is exactly what the samples_per_row / n_synth change
# affects.
set.seed(42L)
task = sim_dgp_correlated(n = 1000L, r = 0.7)
sampler = ConditionalARFSampler$new(task, verbose = FALSE)
learner = lrn("regr.ranger", num.trees = 100L)

make_obj = function() {
	if (method == "cfi") {
		CFI$new(
			task = task,
			learner = learner,
			measure = msr("regr.mse"),
			sampler = sampler,
			n_repeats = 30L
		)
	} else {
		ConditionalSAGE$new(
			task = task,
			learner = learner,
			measure = msr("regr.mse"),
			sampler = sampler,
			n_permutations = 5L,
			n_samples = 30L
		)
	}
}

bm = bench::mark(make_obj()$compute(), iterations = 3L, check = FALSE)

res = data.frame(
	label = label,
	method = method,
	xplainfi = as.character(utils::packageVersion("xplainfi")),
	min_s = as.numeric(bm$min),
	median_s = as.numeric(bm$median),
	mem_mb = as.numeric(bm$mem_alloc) / 1024^2,
	n_itr = bm$n_itr
)

dir.create("bench/.results", showWarnings = FALSE, recursive = TRUE)
saveRDS(res, sprintf("bench/.results/%s-%s.rds", label, method))
print(res, row.names = FALSE)

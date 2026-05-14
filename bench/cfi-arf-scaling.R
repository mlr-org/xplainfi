# Benchmark CFI + ConditionalARFSampler before/after the samples_per_row refactor.
#
# Compares the current branch (where PerturbationImportance passes
# `samples_per_row = n_repeats` to the sampler on unique test rows) against
# the pre-refactor commit (where evidence rows were replicated `n_repeats`
# times and the sampler called `arf::forge(n_synth = 1)` per row).
#
# Run from an R session at the package root:
#   source("bench/cfi-arf-scaling.R")

suppressPackageStartupMessages({
	library(cross)
	library(usethis)
	library(bench)
})
proj_set(".", force = TRUE)

# Commit before the samples_per_row plumbing started.
REF_PRE = "aa88c624"

result = cross::bench_branches(
	branches = c(REF_PRE, "HEAD"),
	{
		suppressPackageStartupMessages({
			library(xplainfi)
			library(mlr3)
			library(mlr3learners)
		})
		lgr::get_logger("mlr3")$set_threshold("warn")
		set.seed(42L)
		task = sim_dgp_correlated(n = 1000L, r = 0.7)
		sampler = ConditionalARFSampler$new(task, verbose = FALSE)
		learner = lrn("regr.ranger", num.trees = 100L)
		cfi = CFI$new(
			task = task,
			learner = learner,
			measure = msr("regr.mse"),
			sampler = sampler,
			n_repeats = 30L
		)
		bench::mark(cfi$compute(), iterations = 3L, check = FALSE)
	}
)

print(result, n = Inf)

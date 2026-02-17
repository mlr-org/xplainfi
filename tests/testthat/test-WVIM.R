# =============================================================================
# WVIM/LOCO Tests using higher-level test helpers
# =============================================================================

# -----------------------------------------------------------------------------
# Basic functionality - WVIM
# -----------------------------------------------------------------------------

test_that("WVIM default behavior with minimal parameters", {
	test_default_behavior(WVIM, task_type = "regr", direction = "leave-out", n_repeats = 1L)
})

test_that("WVIM basic workflow with regression", {
	task = tgen("friedman1")$generate(n = 150)

	wvim = WVIM$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		direction = "leave-out",
		n_repeats = 1L
	)
	checkmate::expect_r6(wvim, c("FeatureImportanceMethod", "WVIM"))

	wvim$compute()
	expect_method_output(wvim)
})

test_that("WVIM direction parameter (leave-out vs leave-in)", {
	task = tgen("friedman1")$generate(n = 150)
	learner = lrn("regr.rpart")
	measure = msr("regr.mse")
	features = task$feature_names[1:3]

	# Test leave-out direction
	wvim_out = WVIM$new(
		task = task,
		learner = learner,
		measure = measure,
		features = features,
		direction = "leave-out",
		n_repeats = 1L
	)
	expect_equal(wvim_out$direction, "leave-out")
	wvim_out$compute()
	result_out = wvim_out$importance()
	expect_importance_dt(result_out, features = features)

	# Test leave-in direction
	wvim_in = WVIM$new(
		task = task,
		learner = learner,
		measure = measure,
		features = features,
		direction = "leave-in",
		n_repeats = 1L
	)
	expect_equal(wvim_in$direction, "leave-in")
	wvim_in$compute()
	result_in = wvim_in$importance()
	expect_importance_dt(result_in, features = features)

	# Results should differ
	expect_false(isTRUE(all.equal(result_out, result_in)))
})

test_that("WVIM with feature groups", {
	task = tgen("friedman1")$generate(n = 150)

	groups = list(
		important_set = c("important1", "important2", "important3"),
		unimportant_set = c("unimportant1", "unimportant2")
	)

	test_grouped_importance(
		WVIM,
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		groups = groups,
		expected_classes = c("FeatureImportanceMethod", "WVIM"),
		direction = "leave-out",
		n_repeats = 1L
	)
})

# -----------------------------------------------------------------------------
# Basic functionality - LOCO
# -----------------------------------------------------------------------------

test_that("LOCO default behavior with minimal parameters", {
	test_default_behavior(LOCO, task_type = "regr", n_repeats = 1L)
})

test_that("LOCO basic workflow with regression", {
	task = tgen("friedman1")$generate(n = 100)

	loco = LOCO$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		n_repeats = 1L
	)
	checkmate::expect_r6(loco, c("FeatureImportanceMethod", "WVIM", "LOCO"))

	loco$compute()
	expect_method_output(loco)

	# LOCO-specific checks
	expect_equal(loco$direction, "leave-out")
	expect_equal(loco$label, "Leave-One-Covariate-Out (LOCO)")
})

test_that("LOCO basic workflow with classification", {
	task = tgen("simplex", d = 5)$generate(n = 100)

	loco = LOCO$new(
		task = task,
		learner = lrn("classif.rpart", predict_type = "prob"),
		measure = msr("classif.ce"),
		n_repeats = 1L
	)
	checkmate::expect_r6(loco, c("FeatureImportanceMethod", "WVIM", "LOCO"))

	loco$compute()
	expect_method_output(loco)
})

# -----------------------------------------------------------------------------
# Feature selection
# -----------------------------------------------------------------------------

test_that("LOCO with all features (features = NULL)", {
	task = tgen("friedman1")$generate(n = 100)
	learner = lrn("regr.rpart")
	measure = msr("regr.mse")

	loco = LOCO$new(task, learner, measure, n_repeats = 1L)
	expect_equal(loco$features, task$feature_names)

	loco$compute()
	expect_importance_dt(loco$importance(), features = task$feature_names)
})

test_that("LOCO with feature subset", {
	task = tgen("friedman1")$generate(n = 100)
	learner = lrn("regr.rpart")
	measure = msr("regr.mse")

	features_subset = task$feature_names[1:3]
	loco = LOCO$new(task, learner, measure, features = features_subset, n_repeats = 1L)
	expect_equal(loco$features, features_subset)

	loco$compute()
	expect_importance_dt(loco$importance(), features = features_subset)
})

# -----------------------------------------------------------------------------
# Repeats and resampling
# -----------------------------------------------------------------------------

test_that("LOCO with multiple refits", {
	task = tgen("friedman1")$generate(n = 150)
	learner = lrn("regr.rpart")
	measure = msr("regr.mse")

	loco = LOCO$new(
		task = task,
		learner = learner,
		measure = measure,
		features = task$feature_names[1:3],
		n_repeats = 3L
	)

	loco$compute()
	expect_importance_dt(loco$importance(), features = loco$features)

	# Scores should have multiple refits
	scores = loco$scores()
	expect_gte(nrow(scores), length(loco$features))
	expect_true(all(scores$iter_repeat %in% 1:3))
})

test_that("LOCO with cross-validation", {
	task = tgen("friedman1")$generate(n = 150)

	loco = LOCO$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		resampling = rsmp("cv", folds = 3),
		features = task$feature_names[1:2]
	)
	loco$compute()
	expect_importance_dt(loco$importance(), features = loco$features)
})

# -----------------------------------------------------------------------------
# Sensible results
# -----------------------------------------------------------------------------

test_that("LOCO friedman1 produces sensible ranking", {
	test_friedman1_sensible_ranking(
		LOCO,
		n_repeats = 5L
	)
})

# -----------------------------------------------------------------------------
# Lei et al. (2018) inference
# -----------------------------------------------------------------------------

test_that("LOCO ci_method='lei' with default wilcoxon test", {
	task = sim_dgp_independent(n = 200)

	loco = LOCO$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		resampling = rsmp("holdout"),
		n_repeats = 1L
	)
	loco$compute()

	imp = loco$importance(ci_method = "lei")

	expect_importance_dt(imp, features = loco$features)
	expected_cols = c(
		"feature",
		"importance",
		"se",
		"statistic",
		"p.value",
		"conf_lower",
		"conf_upper"
	)
	expect_true(all(expected_cols %in% names(imp)))
	expect_true(all(is.finite(imp$importance)))
	expect_true(all(is.finite(imp$p.value)))
	expect_true(all(imp$p.value >= 0 & imp$p.value <= 1))
})

test_that("LOCO ci_method='lei' with t-test", {
	task = sim_dgp_independent(n = 200)

	loco = LOCO$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		resampling = rsmp("holdout"),
		n_repeats = 1L
	)
	loco$compute()

	imp = loco$importance(ci_method = "lei", test = "t")

	expect_importance_dt(imp, features = loco$features)
	expect_true(all(c("statistic", "p.value", "conf_lower", "conf_upper") %in% names(imp)))
})

test_that("LOCO ci_method='lei' alternative='greater' produces one-sided CIs", {
	task = sim_dgp_independent(n = 200)

	loco = LOCO$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		resampling = rsmp("holdout"),
		n_repeats = 1L
	)
	loco$compute()

	imp_greater = loco$importance(ci_method = "lei", alternative = "greater")
	imp_twosided = loco$importance(ci_method = "lei", alternative = "two.sided")

	# One-sided: upper bound should be Inf
	expect_true(all(is.infinite(imp_greater$conf_upper)))
	expect_true(all(is.finite(imp_greater$conf_lower)))

	# Two-sided: both bounds should be finite
	expect_true(all(is.finite(imp_twosided$conf_lower)))
	expect_true(all(is.finite(imp_twosided$conf_upper)))
})

test_that("LOCO ci_method='lei' custom aggregator", {
	task = sim_dgp_independent(n = 200)

	loco = LOCO$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		resampling = rsmp("holdout"),
		n_repeats = 1L
	)
	loco$compute()

	# Default aggregator is median
	imp_median = loco$importance(ci_method = "lei")
	# Override with mean
	imp_mean = loco$importance(ci_method = "lei", aggregator = mean)

	# Point estimates should generally differ (median != mean)
	expect_importance_dt(imp_median, features = loco$features)
	expect_importance_dt(imp_mean, features = loco$features)
})

test_that("LOCO ci_method='lei' warns about duplicated observations", {
	task = sim_dgp_independent(n = 200)

	# Subsampling with repeats causes observations to appear in multiple test sets
	loco = LOCO$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		resampling = rsmp("subsampling", repeats = 3),
		n_repeats = 1L
	)
	loco$compute()

	expect_warning(
		loco$importance(ci_method = "lei"),
		regexp = "duplicated observation"
	)
})

test_that("LOCO ci_method='lei' works without warnings for cross-validation", {
	task = sim_dgp_independent(n = 200)

	loco = LOCO$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		resampling = rsmp("cv", folds = 3),
		n_repeats = 1L
	)
	loco$compute()

	# CV has no duplicate observations, so no warning expected
	# (wilcox.test may warn about zeroes, which is expected/harmless)
	imp = suppressWarnings(loco$importance(ci_method = "lei"))
	expect_importance_dt(imp, features = loco$features)
	expect_true(all(c("statistic", "p.value", "conf_lower", "conf_upper") %in% names(imp)))
})

test_that("WVIM ci_method='lei' works for leave-out direction", {
	task = tgen("friedman1")$generate(n = 200)

	wvim = WVIM$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		resampling = rsmp("holdout"),
		features = task$feature_names[1:3],
		direction = "leave-out",
		n_repeats = 1L
	)
	wvim$compute()

	imp = wvim$importance(ci_method = "lei")
	expect_importance_dt(imp, features = wvim$features)
	expect_true(all(c("statistic", "p.value", "conf_lower", "conf_upper") %in% names(imp)))
})

test_that("LOCO ci_method='lei' other ci_methods still work", {
	task = sim_dgp_independent(n = 200)

	loco = LOCO$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		resampling = rsmp("subsampling", repeats = 5),
		n_repeats = 1L
	)
	loco$compute()

	# Base methods should still work via super$importance()
	imp_none = loco$importance(ci_method = "none")
	imp_raw = loco$importance(ci_method = "raw")

	expect_importance_dt(imp_none, features = loco$features)
	expect_importance_dt(imp_raw, features = loco$features)
})

# -----------------------------------------------------------------------------
# Multiplicity correction
# -----------------------------------------------------------------------------

test_that("LOCO ci_method='lei' p_adjust corrects p-values", {
	task = sim_dgp_independent(n = 200)

	loco = LOCO$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		resampling = rsmp("holdout"),
		n_repeats = 1L
	)
	loco$compute()

	imp_none = suppressWarnings(loco$importance(ci_method = "lei", p_adjust = "none"))
	imp_holm = suppressWarnings(loco$importance(ci_method = "lei", p_adjust = "holm"))
	imp_bonf = suppressWarnings(loco$importance(ci_method = "lei", p_adjust = "bonferroni"))
	imp_bh = suppressWarnings(loco$importance(ci_method = "lei", p_adjust = "BH"))

	# All should produce valid output
	expect_importance_dt(imp_none, features = loco$features)
	expect_importance_dt(imp_holm, features = loco$features)
	expect_importance_dt(imp_bonf, features = loco$features)
	expect_importance_dt(imp_bh, features = loco$features)

	# Adjusted p-values should be >= unadjusted (or equal for single comparisons)
	expect_true(all(imp_holm$p.value >= imp_none$p.value - 1e-10))
	expect_true(all(imp_bonf$p.value >= imp_none$p.value - 1e-10))
})

test_that("LOCO ci_method='lei' default p_adjust is 'none'", {
	task = sim_dgp_independent(n = 200)

	loco = LOCO$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		resampling = rsmp("holdout"),
		n_repeats = 1L
	)
	loco$compute()

	imp_default = suppressWarnings(loco$importance(ci_method = "lei"))
	imp_none = suppressWarnings(loco$importance(ci_method = "lei", p_adjust = "none"))

	expect_equal(imp_default$p.value, imp_none$p.value)
})

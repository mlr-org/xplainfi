# =============================================================================
# CFI Tests using higher-level test helpers
# =============================================================================

# -----------------------------------------------------------------------------
# Basic functionality
# -----------------------------------------------------------------------------

test_that("CFI default behavior with minimal parameters", {
	skip_if_not_installed("arf")

	test_default_behavior(CFI, task_type = "regr", n_repeats = 1L)
})

test_that("CFI basic workflow with classification", {
	task = tgen("2dnormals")$generate(n = 100)

	cfi = CFI$new(
		task = task,
		learner = lrn("classif.rpart", predict_type = "prob"),
		measure = msr("classif.ce"),
		sampler = ConditionalGaussianSampler$new(task),
		n_repeats = 1L
	)
	checkmate::expect_r6(cfi, c("FeatureImportanceMethod", "PerturbationImportance", "CFI"))

	cfi$compute()
	expect_method_output(cfi)
})

test_that("CFI uses ConditionalARFSampler by default", {
	skip_if_not_installed("arf")

	task = tgen("xor")$generate(n = 100)

	cfi = CFI$new(
		task = task,
		learner = lrn("classif.rpart", predict_type = "prob"),
		measure = msr("classif.ce")
	)

	checkmate::expect_r6(cfi$sampler, "ConditionalARFSampler")
	expect_equal(cfi$label, "Conditional Feature Importance")
})

test_that("CFI featureless learner produces zero importance", {
	skip_if_not_installed("arf")

	test_featureless_zero_importance(CFI, task_type = "classif", n_repeats = 1L)
})

# -----------------------------------------------------------------------------
# Repeats and scores
# -----------------------------------------------------------------------------

test_that("CFI multiple repeats and scores structure", {
	task = tgen("friedman1")$generate(n = 100)

	test_n_repeats_and_scores(
		CFI,
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		n_repeats = 2L,
		sampler = ConditionalGaussianSampler$new(task)
	)
})

test_that("CFI single feature", {
	task = tgen("friedman1")$generate(n = 100)

	test_single_feature(
		CFI,
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		feature = "important4",
		n_repeats = 2L,
		sampler = ConditionalGaussianSampler$new(task)
	)
})

# -----------------------------------------------------------------------------
# Sensible results
# -----------------------------------------------------------------------------

test_that("CFI friedman1 produces sensible ranking", {
	skip_if_not_installed("arf")

	test_friedman1_sensible_ranking(
		CFI,
		n_repeats = 5L
	)
})

# -----------------------------------------------------------------------------
# Resampling
# -----------------------------------------------------------------------------

test_that("CFI with resampling", {
	task = tgen("2dnormals")$generate(n = 100)

	cfi = CFI$new(
		task = task,
		learner = lrn("classif.rpart", predict_type = "prob"),
		measure = msr("classif.ce"),
		resampling = rsmp("cv", folds = 3),
		n_repeats = 2L,
		sampler = ConditionalGaussianSampler$new(task)
	)
	cfi$compute()
	expect_method_output(cfi)
})

# -----------------------------------------------------------------------------
# Parameter validation
# -----------------------------------------------------------------------------

test_that("CFI parameter validation", {
	task = tgen("2dnormals")$generate(n = 50)

	test_parameter_validation(
		CFI,
		task = task,
		learner = lrn("classif.rpart", predict_type = "prob"),
		measure = msr("classif.ce"),
		sampler = ConditionalGaussianSampler$new(task)
	)
})

# -----------------------------------------------------------------------------
# Grouped importance
# -----------------------------------------------------------------------------

test_that("CFI with feature groups", {
	task = tgen("friedman1")$generate(n = 100)

	groups = list(
		important_features = c("important1", "important2", "important3"),
		unimportant_features = c("unimportant1", "unimportant2", "unimportant3")
	)

	test_grouped_importance(
		CFI,
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		groups = groups,
		expected_classes = c("FeatureImportanceMethod", "PerturbationImportance", "CFI"),
		sampler = ConditionalGaussianSampler$new(task),
		n_repeats = 1L
	)
})

# -----------------------------------------------------------------------------
# Custom samplers
# -----------------------------------------------------------------------------

test_that("CFI with custom ARF sampler", {
	skip_if_not_installed("arf")

	task = tgen("spirals")$generate(n = 100)

	cfi = CFI$new(
		task = task,
		learner = lrn("classif.rpart", predict_type = "prob"),
		measure = msr("classif.ce"),
		sampler = ConditionalARFSampler$new(task),
		n_repeats = 1L
	)
	checkmate::expect_r6(cfi$sampler, "ConditionalARFSampler")
	cfi$compute()
	expect_importance_dt(cfi$importance(), features = cfi$features)
})

test_that("CFI with KnockoffSampler and KnockoffGaussianSampler", {
	skip_if_not_installed("knockoff")

	task = tgen("friedman1")$generate(n = 150)
	learner = lrn("regr.rpart")
	measure = msr("regr.mse")

	# Test with KnockoffSampler
	cfi_ko = CFI$new(
		task = task,
		learner = learner,
		measure = measure,
		sampler = KnockoffSampler$new(task),
		n_repeats = 1L
	)
	checkmate::expect_r6(cfi_ko$sampler, "KnockoffSampler")
	cfi_ko$compute()
	expect_importance_dt(cfi_ko$importance(), features = cfi_ko$features)

	# Test with KnockoffGaussianSampler
	cfi_gko = CFI$new(
		task = task,
		learner = learner,
		measure = measure,
		sampler = KnockoffGaussianSampler$new(task),
		n_repeats = 1L
	)
	checkmate::expect_r6(cfi_gko$sampler, "KnockoffGaussianSampler")
	cfi_gko$compute()
	expect_importance_dt(cfi_gko$importance(), features = cfi_gko$features)
})

# -----------------------------------------------------------------------------
# CFI-specific: CPI variance method
# -----------------------------------------------------------------------------

test_that("CFI with CPI variance method using KnockoffGaussianSampler", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("knockoff")

	# Use n=500 for stable p-value comparisons
	task = sim_dgp_correlated(n = 500, r = 0.7)
	learner = lrn("regr.ranger", num.trees = 100)
	measure = msr("regr.mse")
	resampling = rsmp("holdout")

	gaussian_sampler = KnockoffGaussianSampler$new(task)
	cfi = CFI$new(
		task = task,
		learner = learner,
		measure = measure,
		resampling = resampling,
		sampler = gaussian_sampler,
		n_repeats = 1L
	)

	# Check that CPI is in the variance methods registry
	expect_contains(cfi$.__enclos_env__$private$.ci_methods, "cpi")

	cfi$compute()

	# Test CPI variance method
	cpi_result = cfi$importance(ci_method = "cpi")

	# Check structure
	expect_importance_dt(cpi_result, features = cfi$features)
	expected_cols = c(
		"feature",
		"importance",
		"se",
		"statistic",
		"p.value",
		"conf_lower",
		"conf_upper"
	)
	expect_contains(names(cpi_result), expected_cols)

	# Check that all values are finite (except conf_upper which is Inf for one-sided)
	checkmate::expect_numeric(cpi_result$importance, finite = TRUE)
	checkmate::expect_numeric(cpi_result$se, finite = TRUE, lower = 0)
	checkmate::expect_numeric(cpi_result$statistic, finite = TRUE)
	checkmate::expect_numeric(cpi_result$p.value, finite = TRUE, lower = 0, upper = 1)
	checkmate::expect_numeric(cpi_result$conf_lower, finite = TRUE)
	checkmate::expect_numeric(cpi_result$conf_upper, finite = TRUE)

	# For correlated DGP, important features should have smaller p-values
	important_pvals = cpi_result[feature %in% c("x1", "x3")]$p.value
	unimportant_pvals = cpi_result[feature %in% c("x2", "x4")]$p.value
	expect_lt(mean(important_pvals), mean(unimportant_pvals))
})

test_that("CFI with CPI and p_adjust = 'BH' adjusts p-values", {
	skip_if_not_installed("knockoff")

	task = sim_dgp_correlated(n = 200, r = 0.7)
	learner = lrn("regr.rpart")
	measure = msr("regr.mse")
	resampling = rsmp("holdout")

	gaussian_sampler = KnockoffGaussianSampler$new(task)
	cfi = CFI$new(
		task = task,
		learner = learner,
		measure = measure,
		resampling = resampling,
		sampler = gaussian_sampler,
		n_repeats = 1L
	)
	cfi$compute()

	cpi_none = cfi$importance(ci_method = "cpi")
	cpi_bh = cfi$importance(ci_method = "cpi", p_adjust = "BH")

	# Point estimates should be identical
	expect_equal(cpi_none$importance, cpi_bh$importance)

	# BH-adjusted p-values should be >= unadjusted
	expect_true(all(cpi_bh$p.value >= cpi_none$p.value - 1e-10))

	# CIs should be unchanged (BH only adjusts p-values, not CIs)
	expect_equal(cpi_none$conf_lower, cpi_bh$conf_lower)
	expect_equal(cpi_none$conf_upper, cpi_bh$conf_upper)
})

test_that("CPI with non-t test reports se = NA", {
	skip_if_not_installed("knockoff")

	task = sim_dgp_correlated(n = 200, r = 0.7)
	learner = lrn("regr.rpart")
	measure = msr("regr.mse")
	resampling = rsmp("holdout")

	gaussian_sampler = KnockoffGaussianSampler$new(task)
	cfi = CFI$new(
		task = task,
		learner = learner,
		measure = measure,
		resampling = resampling,
		sampler = gaussian_sampler,
		n_repeats = 1L
	)
	cfi$compute()

	# Default test is t-test, se should be finite
	cpi_t = cfi$importance(ci_method = "cpi", test = "t")
	checkmate::expect_numeric(cpi_t$se, finite = TRUE, lower = 0)

	# Wilcoxon test: se should be NA
	cpi_wilcox = suppressWarnings(cfi$importance(ci_method = "cpi", test = "wilcoxon"))
	expect_true(all(is.na(cpi_wilcox$se)))
})

test_that("CFI with CPI warning on problematic resampling", {
	skip_if_not_installed("knockoff")

	task = sim_dgp_correlated(n = 50)
	learner = lrn("regr.rpart")
	measure = msr("regr.mse")
	resampling = rsmp("subsampling", repeats = 5)

	gaussian_sampler = KnockoffGaussianSampler$new(task)
	cfi = CFI$new(
		task = task,
		learner = learner,
		measure = measure,
		resampling = resampling,
		sampler = gaussian_sampler,
		n_repeats = 1L
	)
	cfi$compute()

	# Should warn about non-holdout resampling
	expect_warning(
		cpi_result <- cfi$importance(ci_method = "cpi"),
		regexp = "single test set"
	)

	expect_importance_dt(cpi_result, features = cfi$features)

	# CV also warns (overlapping training data)
	cfi_cv = CFI$new(
		task = task,
		learner = learner,
		measure = measure,
		resampling = rsmp("cv", folds = 5),
		sampler = gaussian_sampler,
		n_repeats = 1L
	)
	cfi_cv$compute()
	expect_warning(
		cfi_cv$importance(ci_method = "cpi"),
		regexp = "single test set"
	)

	# With holdout (single split), should be silent
	cfi_holdout = CFI$new(
		task = task,
		learner = learner,
		measure = measure,
		resampling = rsmp("holdout"),
		sampler = gaussian_sampler,
		n_repeats = 1L
	)
	cfi_holdout$compute()
	expect_silent(cfi_holdout$importance(ci_method = "cpi"))
})

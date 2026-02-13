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
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	task = tgen("2dnormals")$generate(n = 100)

	test_basic_workflow(
		CFI,
		task = task,
		learner = lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
		measure = msr("classif.ce"),
		expected_classes = c("FeatureImportanceMethod", "PerturbationImportance", "CFI"),
		sampler = ConditionalGaussianSampler$new(task),
		n_repeats = 1L
	)
})

test_that("CFI uses ConditionalARFSampler by default", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	task = tgen("xor")$generate(n = 100)

	cfi = CFI$new(
		task = task,
		learner = lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
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
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	task = tgen("friedman1")$generate(n = 100)

	test_n_repeats_and_scores(
		CFI,
		task = task,
		learner = lrn("regr.ranger", num.trees = 50),
		measure = msr("regr.mse"),
		n_repeats = 2L,
		sampler = ConditionalGaussianSampler$new(task)
	)
})

test_that("CFI single feature", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	task = tgen("friedman1")$generate(n = 100)

	test_single_feature(
		CFI,
		task = task,
		learner = lrn("regr.ranger", num.trees = 50),
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
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	test_friedman1_sensible_ranking(
		CFI,
		learner = lrn("regr.ranger", num.trees = 50),
		measure = msr("regr.mse"),
		n_repeats = 5L
	)
})

# -----------------------------------------------------------------------------
# Resampling
# -----------------------------------------------------------------------------

test_that("CFI with resampling", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	task = tgen("xor", d = 5)$generate(n = 100)

	test_with_resampling(
		CFI,
		task = task,
		learner = lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
		measure = msr("classif.ce"),
		resampling = rsmp("cv", folds = 3),
		n_repeats = 2L,
		sampler = ConditionalGaussianSampler$new(task)
	)
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
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	task = tgen("spirals")$generate(n = 100)

	test_custom_sampler(
		CFI,
		task = task,
		learner = lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
		measure = msr("classif.ce"),
		sampler = ConditionalARFSampler$new(task),
		expected_sampler_class = "ConditionalARFSampler",
		n_repeats = 1L
	)
})

test_that("CFI with KnockoffSampler and KnockoffGaussianSampler", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("knockoff")

	task = tgen("friedman1")$generate(n = 150)
	learner = lrn("regr.ranger", num.trees = 50)
	measure = msr("regr.mse")

	# Test with KnockoffSampler
	test_custom_sampler(
		CFI,
		task = task,
		learner = learner,
		measure = measure,
		sampler = KnockoffSampler$new(task),
		expected_sampler_class = "KnockoffSampler",
		n_repeats = 1L
	)

	# Test with KnockoffGaussianSampler
	test_custom_sampler(
		CFI,
		task = task,
		learner = learner,
		measure = measure,
		sampler = KnockoffGaussianSampler$new(task),
		expected_sampler_class = "KnockoffGaussianSampler",
		n_repeats = 1L
	)
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
	resampling = rsmp("cv", folds = 5)

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

	# conf_upper should be Inf for one-sided test
	expect_true(all(is.infinite(cpi_result$conf_upper)))

	# For correlated DGP, important features should have smaller p-values
	important_pvals = cpi_result[feature %in% c("x1", "x3")]$p.value
	unimportant_pvals = cpi_result[feature %in% c("x2", "x4")]$p.value
	expect_lt(mean(important_pvals), mean(unimportant_pvals))
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

	# Should warn about duplicated observations
	expect_warning(
		cpi_result <- cfi$importance(ci_method = "cpi"),
		regexp = "duplicated observation"
	)

	expect_importance_dt(cpi_result, features = cfi$features)

	# With proper CV, should be silent
	cfi_cv = CFI$new(
		task = task,
		learner = learner,
		measure = measure,
		resampling = rsmp("cv", folds = 5),
		sampler = gaussian_sampler,
		n_repeats = 1L
	)
	cfi_cv$compute()
	expect_silent(cfi_cv$importance(ci_method = "cpi"))
})

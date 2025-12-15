# =============================================================================
# CFI Tests using higher-level test helpers
# =============================================================================

# -----------------------------------------------------------------------------
# Basic functionality
# -----------------------------------------------------------------------------

test_that("CFI constructor validation", {
	test_constructor_validation(CFI)
})

test_that("CFI default behavior with minimal parameters", {
	skip_if_not_installed("arf")

	set.seed(123)
	test_default_behavior(CFI, task_type = "regr")
})

test_that("CFI basic workflow with classification", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("2dnormals")$generate(n = 100)

	test_basic_workflow(
		CFI,
		task = task,
		learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
		measure = mlr3::msr("classif.ce"),
		expected_classes = c("FeatureImportanceMethod", "PerturbationImportance", "CFI")
	)
})

test_that("CFI uses ConditionalARFSampler by default", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("xor")$generate(n = 100)

	cfi = CFI$new(
		task = task,
		learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
		measure = mlr3::msr("classif.ce")
	)

	checkmate::expect_r6(cfi$sampler, "ConditionalARFSampler")
	expect_equal(cfi$label, "Conditional Feature Importance")
})

test_that("CFI featureless learner produces zero importance", {
	skip_if_not_installed("arf")

	set.seed(123)
	test_featureless_zero_importance(CFI, task_type = "classif")
})

# -----------------------------------------------------------------------------
# Repeats and scores
# -----------------------------------------------------------------------------

test_that("CFI multiple repeats and scores structure", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 200)

	test_n_repeats_and_scores(
		CFI,
		task = task,
		learner = mlr3::lrn("regr.ranger", num.trees = 50),
		measure = mlr3::msr("regr.mse"),
		n_repeats = 2L
	)
})

test_that("CFI single feature", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 200)

	test_single_feature(
		CFI,
		task = task,
		learner = mlr3::lrn("regr.ranger", num.trees = 50),
		measure = mlr3::msr("regr.mse"),
		feature = "important4",
		n_repeats = 2L
	)
})

# -----------------------------------------------------------------------------
# Sensible results
# -----------------------------------------------------------------------------

test_that("CFI friedman1 produces sensible ranking", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	test_friedman1_sensible_ranking(
		CFI,
		learner = mlr3::lrn("regr.ranger", num.trees = 50),
		measure = mlr3::msr("regr.mse")
	)
})

# -----------------------------------------------------------------------------
# Resampling
# -----------------------------------------------------------------------------

test_that("CFI with resampling", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("xor", d = 5)$generate(n = 200)

	test_with_resampling(
		CFI,
		task = task,
		learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
		measure = mlr3::msr("classif.ce"),
		resampling = mlr3::rsmp("cv", folds = 3),
		n_repeats = 2L
	)
})

# -----------------------------------------------------------------------------
# Parameter validation
# -----------------------------------------------------------------------------

test_that("CFI parameter validation", {
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("2dnormals")$generate(n = 50)

	test_parameter_validation(
		CFI,
		task = task,
		learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
		measure = mlr3::msr("classif.ce")
	)
})

# -----------------------------------------------------------------------------
# Grouped importance
# -----------------------------------------------------------------------------

test_that("CFI with feature groups", {
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 200)

	groups = list(
		important_features = c("important1", "important2", "important3"),
		unimportant_features = c("unimportant1", "unimportant2", "unimportant3")
	)

	test_grouped_importance(
		CFI,
		task = task,
		learner = mlr3::lrn("regr.rpart"),
		measure = mlr3::msr("regr.mse"),
		groups = groups,
		expected_classes = c("FeatureImportanceMethod", "PerturbationImportance", "CFI")
	)
})

# -----------------------------------------------------------------------------
# Custom samplers
# -----------------------------------------------------------------------------

test_that("CFI with custom ARF sampler", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("spirals")$generate(n = 100)

	test_custom_sampler(
		CFI,
		task = task,
		learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
		measure = mlr3::msr("classif.ce"),
		sampler = ConditionalARFSampler$new(task),
		expected_sampler_class = "ConditionalARFSampler"
	)
})

test_that("CFI with KnockoffSampler and KnockoffGaussianSampler", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("knockoff")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 150)
	learner = mlr3::lrn("regr.ranger", num.trees = 50)
	measure = mlr3::msr("regr.mse")

	# Test with KnockoffSampler
	test_custom_sampler(
		CFI,
		task = task,
		learner = learner,
		measure = measure,
		sampler = KnockoffSampler$new(task),
		expected_sampler_class = "KnockoffSampler"
	)

	# Test with KnockoffGaussianSampler
	test_custom_sampler(
		CFI,
		task = task,
		learner = learner,
		measure = measure,
		sampler = KnockoffGaussianSampler$new(task),
		expected_sampler_class = "KnockoffGaussianSampler"
	)
})

test_that("CFI with KnockoffSequentialSampler", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("seqknockoff")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 150)

	test_custom_sampler(
		CFI,
		task = task,
		learner = mlr3::lrn("regr.ranger", num.trees = 50),
		measure = mlr3::msr("regr.mse"),
		sampler = KnockoffSequentialSampler$new(task),
		expected_sampler_class = "KnockoffSequentialSampler"
	)
})

# -----------------------------------------------------------------------------
# CFI-specific: CPI variance method
# -----------------------------------------------------------------------------

test_that("CFI with CPI variance method using KnockoffGaussianSampler", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("knockoff")

	set.seed(123)
	task = sim_dgp_correlated(n = 300, r = 0.7)
	learner = mlr3::lrn("regr.ranger", num.trees = 100)
	measure = mlr3::msr("regr.mse")
	resampling = mlr3::rsmp("cv", folds = 5)

	gaussian_sampler = KnockoffGaussianSampler$new(task)
	cfi = CFI$new(
		task = task,
		learner = learner,
		measure = measure,
		resampling = resampling,
		sampler = gaussian_sampler
	)

	# Check that CPI is in the variance methods registry
	expect_true("cpi" %in% cfi$.__enclos_env__$private$.ci_methods)

	cfi$compute()

	# Test CPI variance method
	cpi_result = cfi$importance(ci_method = "cpi")

	# Check structure
	expect_importance_dt(cpi_result, features = cfi$features)
	expected_cols = c("feature", "importance", "se", "statistic", "p.value", "conf_lower", "conf_upper")
	expect_true(all(expected_cols %in% names(cpi_result)))

	# Check that all values are finite (except conf_upper which is Inf for one-sided)
	expect_true(all(is.finite(cpi_result$importance)))
	expect_true(all(is.finite(cpi_result$se)))
	expect_true(all(is.finite(cpi_result$statistic)))
	expect_true(all(is.finite(cpi_result$p.value)))
	expect_true(all(is.finite(cpi_result$conf_lower)))
	expect_true(all(is.infinite(cpi_result$conf_upper)))

	# P-values should be in [0, 1]
	expect_true(all(cpi_result$p.value >= 0 & cpi_result$p.value <= 1))

	# SE should be positive
	expect_true(all(cpi_result$se > 0))

	# For correlated DGP, important features should have smaller p-values
	important_pvals = cpi_result[feature %in% c("x1", "x3")]$p.value
	unimportant_pvals = cpi_result[feature %in% c("x2", "x4")]$p.value
	expect_lt(mean(important_pvals), mean(unimportant_pvals))
})

test_that("CFI with CPI warning on problematic resampling", {
	skip_if_not_installed("knockoff")

	set.seed(123)
	task = sim_dgp_correlated(n = 50)
	learner = mlr3::lrn("regr.rpart")
	measure = mlr3::msr("regr.mse")
	resampling = mlr3::rsmp("subsampling", repeats = 5)

	gaussian_sampler = KnockoffGaussianSampler$new(task)
	cfi = CFI$new(
		task = task,
		learner = learner,
		measure = measure,
		resampling = resampling,
		sampler = gaussian_sampler
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
		resampling = mlr3::rsmp("cv", folds = 5),
		sampler = gaussian_sampler
	)
	cfi_cv$compute()
	expect_silent(cfi_cv$importance(ci_method = "cpi"))
})

# =============================================================================
# ConditionalSAGE Tests
# =============================================================================

# -----------------------------------------------------------------------------
# Basic functionality
# -----------------------------------------------------------------------------

test_that("ConditionalSAGE default behavior with minimal parameters", {
	skip_if_not_installed("arf")

	set.seed(123)
	test_default_behavior(ConditionalSAGE, task_type = "regr")
})

test_that("ConditionalSAGE works with classification tasks", {
	skip_if_not_installed("arf")

	# Binary classification
	set.seed(123)
	task_binary = tgen("2dnormals")$generate(n = 50)
	sage_binary = ConditionalSAGE$new(
		task = task_binary,
		learner = lrn("classif.rpart", predict_type = "prob"),
		n_permutations = 2L,
		n_samples = 20L
	)
	checkmate::expect_r6(sage_binary, c("FeatureImportanceMethod", "SAGE", "ConditionalSAGE"))
	sage_binary$compute()
	expect_importance_dt(sage_binary$importance(), features = sage_binary$features)

	# Multiclass classification
	set.seed(123)
	task_multi = tgen("cassini")$generate(n = 50)
	sage_multi = ConditionalSAGE$new(
		task = task_multi,
		learner = lrn("classif.rpart", predict_type = "prob"),
		n_permutations = 2L,
		n_samples = 20L
	)
	sage_multi$compute()
	expect_importance_dt(sage_multi$importance(), features = sage_multi$features)
	expect_length(task_multi$class_names, 3L)
})

test_that("ConditionalSAGE featureless learner produces zero importance", {
	skip_if_not_installed("arf")

	set.seed(123)
	test_featureless_zero_importance(ConditionalSAGE, task_type = "regr")
})

# -----------------------------------------------------------------------------
# Sensible results
# -----------------------------------------------------------------------------

test_that("ConditionalSAGE friedman1 produces sensible ranking", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	test_friedman1_sensible_ranking(
		ConditionalSAGE,
		learner = lrn("regr.ranger", num.trees = 50),
		n = 200L
	)
})

# -----------------------------------------------------------------------------
# Sampler behavior
# -----------------------------------------------------------------------------

test_that("ConditionalSAGE uses ConditionalARFSampler by default", {
	skip_if_not_installed("arf")

	set.seed(123)
	task = tgen("xor")$generate(n = 50)

	sage = ConditionalSAGE$new(
		task = task,
		learner = lrn("classif.rpart", predict_type = "prob"),
		n_permutations = 2L,
		n_samples = 20L
	)

	checkmate::expect_r6(sage$sampler, "ConditionalARFSampler")
	expect_equal(sage$label, "Conditional SAGE")
})

test_that("ConditionalSAGE with custom sampler", {
	skip_if_not_installed("arf")

	set.seed(123)
	task = tgen("spirals")$generate(n = 50)
	custom_sampler = ConditionalARFSampler$new(task, finite_bounds = "local")

	test_custom_sampler(
		ConditionalSAGE,
		task = task,
		learner = lrn("classif.rpart", predict_type = "prob"),
		measure = msr("classif.ce"),
		sampler = custom_sampler,
		expected_sampler_class = "ConditionalSampler",
		n_permutations = 2L,
		n_samples = 20L
	)
})

# -----------------------------------------------------------------------------
# Parameter validation
# -----------------------------------------------------------------------------

test_that("ConditionalSAGE requires predict_type='prob' for classification", {
	set.seed(123)
	task = tgen("2dnormals")$generate(n = 50)

	expect_error(
		ConditionalSAGE$new(
			task = task,
			learner = lrn("classif.rpart", predict_type = "response")
		),
		"Classification learners require probability predictions for SAGE."
	)
})

# -----------------------------------------------------------------------------
# Batching
# -----------------------------------------------------------------------------

test_that("ConditionalSAGE batching produces consistent results", {
	skip_if_not_installed("arf")
	skip_if_not_installed("withr")

	set.seed(123)
	task = tgen("friedman1")$generate(n = 20)

	# Results should be identical with or without batching
	result_batch = withr::with_seed(42, {
		sage = ConditionalSAGE$new(task = task, learner = lrn("regr.rpart"), n_permutations = 2L)
		sage$compute(batch_size = 1)
		sage$importance()
	})

	result_normal = withr::with_seed(42, {
		sage = ConditionalSAGE$new(task = task, learner = lrn("regr.rpart"), n_permutations = 2L)
		sage$compute()
		sage$importance()
	})

	expect_equal(result_batch$importance, result_normal$importance, tolerance = 1e-10)
})

# -----------------------------------------------------------------------------
# n_samples parameter
# -----------------------------------------------------------------------------

test_that("ConditionalSAGE with custom n_samples", {
	skip_if_not_installed("arf")

	set.seed(123)
	task = tgen("friedman1")$generate(n = 50)

	sage = ConditionalSAGE$new(
		task = task,
		learner = lrn("regr.rpart"),
		n_permutations = 2L,
		n_samples = 20L
	)
	expect_equal(sage$param_set$values$n_samples, 20L)

	sage$compute()
	expect_importance_dt(sage$importance(), features = sage$features)
})

# -----------------------------------------------------------------------------
# Convergence tracking
# -----------------------------------------------------------------------------

test_that("ConditionalSAGE SE tracking in convergence_history", {
	skip_if_not_installed("arf")

	set.seed(123)
	task = tgen("friedman1")$generate(n = 30)
	learner = lrn("regr.rpart")
	measure = msr("regr.mse")

	sage = ConditionalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		n_permutations = 6L,
		n_samples = 20L
	)

	# Compute with early stopping to get convergence history
	sage$compute(early_stopping = TRUE, se_threshold = 0.05, check_interval = 2L)

	# Check that convergence_history exists and has SE column
	expect_false(is.null(sage$convergence_history))
	expect_contains(colnames(sage$convergence_history), "se")

	# Check structure of convergence_history
	expected_cols = c("n_permutations", "feature", "importance", "se")
	expect_setequal(colnames(sage$convergence_history), expected_cols)

	# SE values should be non-negative and finite
	se_values = sage$convergence_history$se
	checkmate::expect_numeric(se_values, lower = 0, finite = TRUE)

	# For each feature, SE should be in a reasonable range for conditional sampling
	for (feat in unique(sage$convergence_history$feature)) {
		feat_data = sage$convergence_history[feature == feat]
		feat_data = feat_data[order(n_permutations)]

		if (nrow(feat_data) > 1) {
			# More generous upper bound for conditional sampling
			expect_lt(max(feat_data$se), 20)
			checkmate::expect_numeric(feat_data$se, finite = TRUE)
		}
	}

	# All features should be represented in convergence history
	expect_setequal(
		unique(sage$convergence_history$feature),
		sage$features
	)
})

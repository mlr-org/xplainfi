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

test_that("ConditionalSAGE can be constructed with simple objects", {
	skip_if_not_installed("arf")

	# Test with binary classification
	set.seed(123)
	task_binary = tgen("2dnormals")$generate(n = 50)
	sage_binary = ConditionalSAGE$new(
		task = task_binary,
		learner = lrn("classif.rpart", predict_type = "prob"),
		measure = msr("classif.ce"),
		n_permutations = 2L,
		n_samples = 20L
	)
	checkmate::expect_r6(sage_binary, c("FeatureImportanceMethod", "SAGE", "ConditionalSAGE"))
	sage_binary$compute()
	expect_importance_dt(sage_binary$importance(), features = sage_binary$features)

	# Test with multiclass classification
	set.seed(123)
	task_multi = tgen("cassini")$generate(n = 50)
	sage_multi = ConditionalSAGE$new(
		task = task_multi,
		learner = lrn("classif.rpart", predict_type = "prob"),
		measure = msr("classif.ce"),
		n_permutations = 2L,
		n_samples = 20L
	)
	checkmate::expect_r6(sage_multi, c("FeatureImportanceMethod", "SAGE", "ConditionalSAGE"))
	sage_multi$compute()
	expect_importance_dt(sage_multi$importance(), features = sage_multi$features)

	# Test with regression
	set.seed(123)
	task_regr = tgen("friedman1")$generate(n = 50)
	sage_regr = ConditionalSAGE$new(
		task = task_regr,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		n_permutations = 2L,
		n_samples = 20L
	)
	checkmate::expect_r6(sage_regr, c("FeatureImportanceMethod", "SAGE", "ConditionalSAGE"))
	sage_regr$compute()
	expect_importance_dt(sage_regr$importance(), features = sage_regr$features)
})

test_that("ConditionalSAGE null result for featureless learner", {
	skip_if_not_installed("arf")

	set.seed(123)

	# Test with binary classification
	task_binary = tgen("xor")$generate(n = 50)
	sage_binary = ConditionalSAGE$new(
		task = task_binary,
		learner = lrn("classif.featureless", predict_type = "prob"),
		measure = msr("classif.ce"),
		n_permutations = 2L,
		n_samples = 20L
	)
	sage_binary$compute()
	expected_binary = data.table::data.table(
		feature = sage_binary$features,
		importance = 0,
		key = "feature"
	)
	expect_identical(sage_binary$importance(), expected_binary)

	# Test with multiclass classification
	task_multi = tgen("cassini")$generate(n = 50)
	sage_multi = ConditionalSAGE$new(
		task = task_multi,
		learner = lrn("classif.featureless", predict_type = "prob"),
		measure = msr("classif.ce"),
		n_permutations = 2L,
		n_samples = 20L
	)
	sage_multi$compute()
	expected_multi = data.table::data.table(
		feature = sage_multi$features,
		importance = 0,
		key = "feature"
	)
	expect_identical(sage_multi$importance(), expected_multi)

	# Test with regression
	task_regr = tgen("friedman1")$generate(n = 50)
	sage_regr = ConditionalSAGE$new(
		task = task_regr,
		learner = lrn("regr.featureless"),
		measure = msr("regr.mse"),
		n_permutations = 2L,
		n_samples = 20L
	)
	sage_regr$compute()
	expected_regr = data.table::data.table(
		feature = sage_regr$features,
		importance = 0,
		key = "feature"
	)
	expect_equal(sage_regr$importance(), expected_regr)
})

# -----------------------------------------------------------------------------
# Sensible results
# -----------------------------------------------------------------------------

test_that("ConditionalSAGE with friedman1 produces sensible results", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = tgen("friedman1")$generate(n = 200)
	learner = lrn("regr.ranger", num.trees = 50)
	measure = msr("regr.mse")

	sage = ConditionalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		n_permutations = 3L
	)

	sage$compute()
	result = sage$importance()
	expect_importance_dt(result, features = sage$features)

	# Check that important features (important1-5) generally have higher scores
	# than unimportant features (unimportant1-5)
	important_features = grep("^important", result$feature, value = TRUE)
	unimportant_features = grep("^unimportant", result$feature, value = TRUE)

	important_scores = result[feature %in% important_features]$importance
	unimportant_scores = result[feature %in% unimportant_features]$importance

	# On average, important features should have higher SAGE values
	expect_gt(mean(important_scores), mean(unimportant_scores))

	# Check that scores are finite and not all zero
	checkmate::expect_numeric(result$importance, finite = TRUE)
	expect_gt(max(abs(result$importance)), 0)
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
		measure = msr("classif.ce"),
		n_permutations = 2L,
		n_samples = 20L
	)

	# Should have ConditionalARFSampler
	checkmate::expect_r6(sage$sampler, "ConditionalARFSampler")
	expect_equal(sage$label, "Conditional SAGE")
})

test_that("ConditionalSAGE with custom sampler", {
	skip_if_not_installed("arf")

	set.seed(123)
	task = tgen("spirals")$generate(n = 50)
	custom_sampler = ConditionalARFSampler$new(task, finite_bounds = "local")

	sage = ConditionalSAGE$new(
		task = task,
		learner = lrn("classif.rpart", predict_type = "prob"),
		measure = msr("classif.ce"),
		sampler = custom_sampler,
		n_permutations = 2L,
		n_samples = 20L
	)

	# Should use the custom sampler
	checkmate::expect_r6(sage$sampler, "ConditionalSampler")
	sage$compute()
	expect_importance_dt(sage$importance(), features = sage$features)
})

# -----------------------------------------------------------------------------
# Parameter validation
# -----------------------------------------------------------------------------

test_that("ConditionalSAGE requires predict_type='prob' for classification", {
	set.seed(123)
	task = tgen("2dnormals")$generate(n = 50)
	learner = lrn("classif.rpart") # Default is response
	measure = msr("classif.ce")

	# Should error for ConditionalSAGE
	expect_error(
		ConditionalSAGE$new(
			task = task,
			learner = learner,
			measure = measure
		),
		"Classification learners require probability predictions for SAGE."
	)
})

# -----------------------------------------------------------------------------
# Multiclass classification
# -----------------------------------------------------------------------------

test_that("ConditionalSAGE works with multiclass classification", {
	skip_if_not_installed("arf")

	set.seed(123)
	task = tgen("simplex")$generate(n = 150)
	learner = lrn("classif.rpart", predict_type = "prob")
	measure = msr("classif.ce")

	sage = ConditionalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		n_permutations = 2L
	)

	sage$compute()
	result = sage$importance()
	expect_importance_dt(result, features = sage$features)

	# Check that scores are finite and not all zero
	checkmate::expect_numeric(result$importance, finite = TRUE)
	expect_gt(max(abs(result$importance)), 0)

	# Verify task has 4 classes
	expect_length(task$class_names, 4L)
})

# -----------------------------------------------------------------------------
# Batching
# -----------------------------------------------------------------------------

test_that("ConditionalSAGE batching handles edge cases", {
	skip_if_not_installed("arf")
	skip_if_not_installed("withr")

	set.seed(123)
	task = tgen("friedman1")$generate(n = 20)
	learner = lrn("regr.rpart")
	measure = msr("regr.mse")

	# Test with batch_size = 1
	result_batch_1 = withr::with_seed(42, {
		sage = ConditionalSAGE$new(
			task = task,
			learner = learner,
			measure = measure,
			n_permutations = 2L
		)
		sage$compute(batch_size = 1)
		sage$importance()
	})

	# Compare with normal result
	result_normal = withr::with_seed(42, {
		sage = ConditionalSAGE$new(
			task = task,
			learner = learner,
			measure = measure,
			n_permutations = 2L
		)
		sage$compute()
		sage$importance()
	})

	expect_equal(
		result_batch_1$importance,
		result_normal$importance,
		tolerance = 1e-10
	)
})

test_that("ConditionalSAGE batching with custom sampler", {
	skip_if_not_installed("arf")
	skip_if_not_installed("withr")

	set.seed(123)
	task = tgen("friedman1")$generate(n = 25)
	learner = lrn("regr.rpart")
	measure = msr("regr.mse")

	# Create custom ARF sampler
	custom_sampler = ConditionalARFSampler$new(task, verbose = FALSE)

	# Test with custom sampler - no batching
	result_no_batch = withr::with_seed(42, {
		sage = ConditionalSAGE$new(
			task = task,
			learner = learner,
			measure = measure,
			sampler = custom_sampler,
			n_permutations = 2L
		)
		sage$compute()
		sage$importance()
	})

	# Test with custom sampler - with batching
	result_batch = withr::with_seed(42, {
		sage = ConditionalSAGE$new(
			task = task,
			learner = learner,
			measure = measure,
			sampler = custom_sampler,
			n_permutations = 2L
		)
		sage$compute(batch_size = 30)
		sage$importance()
	})

	expect_equal(
		result_no_batch$importance,
		result_batch$importance,
		tolerance = 1e-10
	)
})

# -----------------------------------------------------------------------------
# n_samples parameter
# -----------------------------------------------------------------------------

test_that("ConditionalSAGE with n_samples parameter", {
	skip_if_not_installed("arf")

	set.seed(123)
	task = tgen("friedman1")$generate(n = 50)
	learner = lrn("regr.rpart")
	measure = msr("regr.mse")

	# Test with default n_samples (100L)
	sage_default = ConditionalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		n_permutations = 2L
	)
	expect_equal(sage_default$param_set$values$n_samples, 100L)
	sage_default$compute()
	result_default = sage_default$importance()
	expect_importance_dt(result_default, features = sage_default$features)

	# Test with custom n_samples = 10L
	sage_10 = ConditionalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		n_permutations = 2L,
		n_samples = 10L
	)
	expect_equal(sage_10$param_set$values$n_samples, 10L)
	sage_10$compute()
	result_10 = sage_10$importance()
	expect_importance_dt(result_10, features = sage_10$features)

	# Test with custom n_samples = 50L
	sage_50 = ConditionalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		n_permutations = 2L,
		n_samples = 50L
	)
	expect_equal(sage_50$param_set$values$n_samples, 50L)
	sage_50$compute()
	result_50 = sage_50$importance()
	expect_importance_dt(result_50, features = sage_50$features)

	# All results should have finite values
	checkmate::expect_numeric(result_default$importance, finite = TRUE)
	checkmate::expect_numeric(result_10$importance, finite = TRUE)
	checkmate::expect_numeric(result_50$importance, finite = TRUE)

	# Test with multiclass classification and custom n_samples
	task_multi = tgen("cassini")$generate(n = 50)
	sage_multi = ConditionalSAGE$new(
		task = task_multi,
		learner = lrn("classif.rpart", predict_type = "prob"),
		measure = msr("classif.ce"),
		n_permutations = 2L,
		n_samples = 20L
	)
	expect_equal(sage_multi$param_set$values$n_samples, 20L)
	sage_multi$compute()
	result_multi = sage_multi$importance()
	expect_importance_dt(result_multi, features = sage_multi$features)
	checkmate::expect_numeric(result_multi$importance, finite = TRUE)
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

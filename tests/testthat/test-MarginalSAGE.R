# =============================================================================
# MarginalSAGE Tests
# =============================================================================

# -----------------------------------------------------------------------------
# Basic functionality
# -----------------------------------------------------------------------------

test_that("MarginalSAGE default behavior with minimal parameters", {
	set.seed(123)
	test_default_behavior(MarginalSAGE, task_type = "regr")
})

test_that("MarginalSAGE can be constructed with simple objects", {
	# Test with binary classification
	set.seed(123)
	task_binary = tgen("2dnormals")$generate(n = 100)
	sage_binary = MarginalSAGE$new(
		task = task_binary,
		learner = lrn("classif.rpart", predict_type = "prob"),
		measure = msr("classif.ce"),
		n_permutations = 2L
	)
	checkmate::expect_r6(sage_binary, c("FeatureImportanceMethod", "SAGE", "MarginalSAGE"))
	sage_binary$compute()
	expect_importance_dt(sage_binary$importance(), features = sage_binary$features)

	# Test with multiclass classification
	set.seed(123)
	task_multi = tgen("cassini")$generate(n = 100)
	sage_multi = MarginalSAGE$new(
		task = task_multi,
		learner = lrn("classif.rpart", predict_type = "prob"),
		measure = msr("classif.ce"),
		n_permutations = 2L
	)
	checkmate::expect_r6(sage_multi, c("FeatureImportanceMethod", "SAGE", "MarginalSAGE"))
	sage_multi$compute()
	expect_importance_dt(sage_multi$importance(), features = sage_multi$features)

	# Test with regression
	set.seed(123)
	task_regr = tgen("friedman1")$generate(n = 100)
	sage_regr = MarginalSAGE$new(
		task = task_regr,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		n_permutations = 2L
	)
	checkmate::expect_r6(sage_regr, c("FeatureImportanceMethod", "SAGE", "MarginalSAGE"))
	sage_regr$compute()
	expect_importance_dt(sage_regr$importance(), features = sage_regr$features)
})

test_that("MarginalSAGE null result for featureless learner", {
	set.seed(123)

	# Test with binary classification
	task_binary = tgen("xor")$generate(n = 200)
	sage_binary = MarginalSAGE$new(
		task = task_binary,
		learner = lrn("classif.featureless", predict_type = "prob"),
		measure = msr("classif.ce"),
		n_permutations = 2L
	)
	sage_binary$compute()
	expected_binary = data.table::data.table(
		feature = sage_binary$features,
		importance = 0,
		key = "feature"
	)
	expect_identical(sage_binary$importance(), expected_binary)

	# Test with multiclass classification
	task_multi = tgen("cassini")$generate(n = 200)
	sage_multi = MarginalSAGE$new(
		task = task_multi,
		learner = lrn("classif.featureless", predict_type = "prob"),
		measure = msr("classif.ce"),
		n_permutations = 2L
	)
	sage_multi$compute()
	expected_multi = data.table::data.table(
		feature = sage_multi$features,
		importance = 0,
		key = "feature"
	)
	expect_identical(sage_multi$importance(), expected_multi)

	# Test with regression
	task_regr = tgen("friedman1")$generate(n = 200)
	sage_regr = MarginalSAGE$new(
		task = task_regr,
		learner = lrn("regr.featureless"),
		measure = msr("regr.mse"),
		n_permutations = 2L
	)
	sage_regr$compute()
	expected_regr = data.table::data.table(
		feature = sage_regr$features,
		importance = 0,
		key = "feature"
	)
	expect_identical(sage_regr$importance(), expected_regr)
})

# -----------------------------------------------------------------------------
# Sensible results
# -----------------------------------------------------------------------------

test_that("MarginalSAGE with friedman1 produces sensible results", {
	set.seed(123)
	task = tgen("friedman1")$generate(n = 200)
	learner = lrn("regr.rpart")
	measure = msr("regr.mse")

	sage = MarginalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		n_permutations = 3L,
		n_samples = 50L
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
# Resampling
# -----------------------------------------------------------------------------

test_that("MarginalSAGE with multiple resampling iterations", {
	set.seed(123)

	# Test with binary classification
	task_binary = tgen("xor")$generate(n = 200)
	sage_binary = MarginalSAGE$new(
		task = task_binary,
		learner = lrn("classif.rpart", predict_type = "prob"),
		measure = msr("classif.ce"),
		resampling = rsmp("cv", folds = 3),
		n_permutations = 2L
	)
	sage_binary$compute()
	expect_importance_dt(sage_binary$importance(), features = sage_binary$features)
	checkmate::expect_data_table(
		sage_binary$scores(),
		types = c("integer", "character", "numeric"),
		nrows = sage_binary$resampling$iters * length(sage_binary$features),
		ncols = 3,
		any.missing = FALSE,
		min.cols = 3
	)

	# Test with regression
	task_regr = tgen("friedman1")$generate(n = 200)
	sage_regr = MarginalSAGE$new(
		task = task_regr,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		resampling = rsmp("cv", folds = 3),
		n_permutations = 2L
	)
	sage_regr$compute()
	expect_importance_dt(sage_regr$importance(), features = sage_regr$features)
	checkmate::expect_data_table(
		sage_regr$scores(),
		types = c("integer", "character", "numeric"),
		nrows = sage_regr$resampling$iters * length(sage_regr$features),
		ncols = 3,
		any.missing = FALSE,
		min.cols = 3
	)
})

# -----------------------------------------------------------------------------
# Single feature
# -----------------------------------------------------------------------------

test_that("MarginalSAGE only one feature", {
	set.seed(123)
	task = tgen("friedman1")$generate(n = 100)

	sage = MarginalSAGE$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		features = "important4",
		n_permutations = 2L
	)

	sage$compute()
	expect_importance_dt(sage$importance(), features = "important4")

	# Should only have one feature
	expect_equal(nrow(sage$importance()), 1L)
	expect_equal(sage$importance()$feature, "important4")
})

# -----------------------------------------------------------------------------
# n_samples parameter
# -----------------------------------------------------------------------------

test_that("MarginalSAGE with n_samples parameter", {
	set.seed(123)

	# Test with binary classification
	task_binary = tgen("2dnormals")$generate(n = 200)
	sage_binary = MarginalSAGE$new(
		task = task_binary,
		learner = lrn("classif.rpart", predict_type = "prob"),
		measure = msr("classif.ce"),
		n_samples = 30L,
		n_permutations = 2L
	)
	sage_binary$compute()
	expect_importance_dt(sage_binary$importance(), features = sage_binary$features)

	# Test with regression
	task_regr = tgen("friedman1")$generate(n = 200)
	sage_regr = MarginalSAGE$new(
		task = task_regr,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		n_samples = 30L,
		n_permutations = 2L
	)
	sage_regr$compute()
	expect_importance_dt(sage_regr$importance(), features = sage_regr$features)

	# Test with multiclass classification
	task_multi = tgen("cassini")$generate(n = 200)
	sage_multi = MarginalSAGE$new(
		task = task_multi,
		learner = lrn("classif.rpart", predict_type = "prob"),
		measure = msr("classif.ce"),
		n_samples = 30L,
		n_permutations = 2L
	)
	sage_multi$compute()
	expect_importance_dt(sage_multi$importance(), features = sage_multi$features)
})

# -----------------------------------------------------------------------------
# Reproducibility
# -----------------------------------------------------------------------------

test_that("MarginalSAGE reproducibility with same seed", {
	set.seed(123)
	task = tgen("2dnormals")$generate(n = 100)
	learner = lrn("classif.rpart", predict_type = "prob")
	measure = msr("classif.ce")

	set.seed(42)
	sage1 = MarginalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		n_permutations = 3L
	)
	sage1$compute()
	result1 = sage1$importance()

	set.seed(42)
	sage2 = MarginalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		n_permutations = 3L
	)
	sage2$compute()
	result2 = sage2$importance()

	# Results should be identical with same seed
	expect_equal(result1$importance, result2$importance, tolerance = 1e-10)
})

# -----------------------------------------------------------------------------
# Parameter validation
# -----------------------------------------------------------------------------

test_that("MarginalSAGE parameter validation", {
	set.seed(123)
	task = tgen("2dnormals")$generate(n = 50)
	learner = lrn("classif.rpart", predict_type = "prob")
	measure = msr("classif.ce")

	# n_permutations must be positive integer
	expect_error(MarginalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		n_permutations = 0L
	))

	expect_error(MarginalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		n_permutations = -1L
	))
})

test_that("MarginalSAGE requires predict_type='prob' for classification", {
	set.seed(123)
	task = tgen("2dnormals")$generate(n = 50)
	learner = lrn("classif.rpart", predict_type = "response")
	measure = msr("classif.ce")

	# Should error for classification without predict_type = "prob"
	expect_error(
		MarginalSAGE$new(
			task = task,
			learner = learner,
			measure = measure
		),
		"Classification learners require probability predictions for SAGE."
	)

	# Should work fine for regression
	task_regr = tgen("friedman1")$generate(n = 50)
	learner_regr = lrn("regr.rpart")

	expect_no_error(
		MarginalSAGE$new(
			task = task_regr,
			learner = learner_regr,
			resampling = rsmp("holdout"),
			measure = msr("regr.mse")
		)
	)
})

# -----------------------------------------------------------------------------
# Multiclass classification
# -----------------------------------------------------------------------------

test_that("MarginalSAGE works with multiclass classification", {
	set.seed(123)
	task = tgen("cassini")$generate(n = 50)
	learner = lrn("classif.rpart", predict_type = "prob")
	measure = msr("classif.ce")

	sage = MarginalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		n_permutations = 2L,
		n_samples = 30L
	)

	sage$compute()
	result = sage$importance()
	expect_importance_dt(result, features = sage$features)

	# Check that scores are finite and not all zero
	checkmate::expect_numeric(result$importance, finite = TRUE)
	expect_gt(max(abs(result$importance)), 0)

	# Verify task has 3 classes
	expect_length(task$class_names, 3L)
})

# -----------------------------------------------------------------------------
# Convergence tracking
# -----------------------------------------------------------------------------

test_that("MarginalSAGE SE tracking in convergence_history", {
	set.seed(123)
	task = tgen("friedman1")$generate(n = 30)
	learner = lrn("regr.rpart")
	measure = msr("regr.mse")

	sage = MarginalSAGE$new(
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

	# For each feature, SE should be in a reasonable range
	for (feat in unique(sage$convergence_history$feature)) {
		feat_data = sage$convergence_history[feature == feat]
		feat_data = feat_data[order(n_permutations)]

		if (nrow(feat_data) > 1) {
			# Just check that SE values are in a reasonable range and not exploding
			expect_lt(max(feat_data$se), 10)
			expect_lt(max(abs(diff(feat_data$se))), 5)
		}
	}

	# All features should be represented in convergence history
	expect_setequal(
		unique(sage$convergence_history$feature),
		sage$features
	)
})

test_that("MarginalSAGE SE-based convergence detection", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	set.seed(123)
	task = tgen("friedman1")$generate(n = 100)
	learner = lrn("regr.ranger", num.trees = 50)
	measure = msr("regr.mse")

	sage = MarginalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		n_permutations = 10L,
		n_samples = 20L
	)

	# Test with very loose SE threshold (should trigger convergence easily)
	sage$compute(
		early_stopping = TRUE,
		se_threshold = 100.0,
		min_permutations = 5L,
		check_interval = 1L
	)

	# Should converge early because SE will be well below 100.0
	expect_true(sage$converged)
	expect_lte(sage$n_permutations_used, 10L)

	# Reset for next test
	sage$reset()

	# Test with very strict SE threshold (should not converge)
	sage$compute(
		early_stopping = TRUE,
		se_threshold = 0.001,
		min_permutations = 5L,
		check_interval = 1L
	)

	# With very strict SE threshold, should not converge early
	expect_false(sage$converged)

	# Test with moderate SE threshold
	sage$reset()

	sage$compute(
		early_stopping = TRUE,
		se_threshold = 0.1,
		min_permutations = 5L,
		check_interval = 1L
	)

	# Should have convergence history with SE tracking regardless of convergence
	expect_false(is.null(sage$convergence_history))
	expect_contains(colnames(sage$convergence_history), "se")
})

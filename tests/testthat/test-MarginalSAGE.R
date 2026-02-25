# =============================================================================
# MarginalSAGE Tests
# =============================================================================

# -----------------------------------------------------------------------------
# Basic functionality
# -----------------------------------------------------------------------------

test_that("MarginalSAGE default behavior with minimal parameters", {
	# Use small params for test speed
	test_default_behavior(MarginalSAGE, task_type = "regr", n_permutations = 2L, n_samples = 20L)
})

test_that("MarginalSAGE works with classification tasks", {
	# Binary classification
	task_binary = tgen("2dnormals")$generate(n = 100)
	sage_binary = MarginalSAGE$new(
		task = task_binary,
		learner = lrn("classif.rpart", predict_type = "prob"),
		n_permutations = 2L
	)
	checkmate::expect_r6(sage_binary, c("FeatureImportanceMethod", "SAGE", "MarginalSAGE"))
	sage_binary$compute()
	expect_importance_dt(sage_binary$importance(), features = sage_binary$features)

	# Multiclass classification
	task_multi = tgen("cassini")$generate(n = 100)
	sage_multi = MarginalSAGE$new(
		task = task_multi,
		learner = lrn("classif.rpart", predict_type = "prob"),
		n_permutations = 2L
	)
	sage_multi$compute()
	expect_importance_dt(sage_multi$importance(), features = sage_multi$features)
	expect_length(task_multi$class_names, 3L)
})

test_that("MarginalSAGE featureless learner produces zero importance", {
	# Use small params for test speed
	test_featureless_zero_importance(
		MarginalSAGE,
		task_type = "regr",
		n_permutations = 2L,
		n_samples = 20L
	)
})

# -----------------------------------------------------------------------------
# Sensible results
# -----------------------------------------------------------------------------

test_that("MarginalSAGE friedman1 produces sensible ranking", {
	# Use small params for test speed
	test_friedman1_sensible_ranking(MarginalSAGE, n = 200L, n_permutations = 2L, n_samples = 20L)
})

# -----------------------------------------------------------------------------
# Resampling
# -----------------------------------------------------------------------------

test_that("MarginalSAGE with cross-validation resampling", {
	task = tgen("friedman1")$generate(n = 200)

	sage = MarginalSAGE$new(
		task = task,
		learner = lrn("regr.rpart"),
		resampling = rsmp("cv", folds = 3),
		n_permutations = 2L
	)
	sage$compute()

	expect_importance_dt(sage$importance(), features = sage$features)
	checkmate::expect_data_table(
		sage$scores(),
		types = c("integer", "character", "numeric"),
		nrows = sage$resampling$iters * length(sage$features),
		ncols = 3,
		any.missing = FALSE
	)
})

# -----------------------------------------------------------------------------
# Single feature
# -----------------------------------------------------------------------------

test_that("MarginalSAGE with single feature", {
	task = tgen("friedman1")$generate(n = 100)

	sage = MarginalSAGE$new(
		task = task,
		learner = lrn("regr.rpart"),
		features = "important4",
		n_permutations = 2L
	)
	sage$compute()

	expect_importance_dt(sage$importance(), features = "important4")
	expect_equal(nrow(sage$importance()), 1L)
})

# -----------------------------------------------------------------------------
# n_samples parameter
# -----------------------------------------------------------------------------

test_that("MarginalSAGE with custom n_samples", {
	task = tgen("friedman1")$generate(n = 200)

	sage = MarginalSAGE$new(
		task = task,
		learner = lrn("regr.rpart"),
		n_samples = 30L,
		n_permutations = 2L
	)
	sage$compute()

	expect_importance_dt(sage$importance(), features = sage$features)
})

# -----------------------------------------------------------------------------
# Reproducibility
# -----------------------------------------------------------------------------

test_that("MarginalSAGE reproducibility with same seed", {
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
	task = tgen("friedman1")$generate(n = 50)
	learner = lrn("regr.rpart")

	# n_permutations must be positive integer
	expect_error(MarginalSAGE$new(task = task, learner = learner, n_permutations = 0L))
	expect_error(MarginalSAGE$new(task = task, learner = learner, n_permutations = -1L))
})

test_that("MarginalSAGE requires predict_type='prob' for classification", {
	task = tgen("2dnormals")$generate(n = 50)

	# Should error for classification without predict_type = "prob"
	expect_error(
		MarginalSAGE$new(
			task = task,
			learner = lrn("classif.rpart", predict_type = "response")
		),
		"Classification learners require probability predictions for SAGE."
	)
})

# -----------------------------------------------------------------------------
# Convergence tracking
# -----------------------------------------------------------------------------

test_that("MarginalSAGE SE tracking in convergence_history", {
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
	skip_on_cran() # ~1s - tests early stopping feature, not core SAGE

	task = tgen("friedman1")$generate(n = 100)
	learner = lrn("regr.rpart")
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

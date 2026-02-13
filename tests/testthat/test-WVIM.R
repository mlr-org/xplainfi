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
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	task = tgen("friedman1")$generate(n = 150)
	learner = lrn("regr.ranger", num.trees = 20)
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
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	task = tgen("friedman1")$generate(n = 100)

	loco = LOCO$new(
		task = task,
		learner = lrn("regr.ranger", num.trees = 50),
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
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	task = tgen("simplex", d = 5)$generate(n = 100)

	loco = LOCO$new(
		task = task,
		learner = lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
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
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	task = tgen("friedman1")$generate(n = 100)
	learner = lrn("regr.ranger", num.trees = 20)
	measure = msr("regr.mse")

	loco = LOCO$new(task, learner, measure, n_repeats = 1L)
	expect_equal(loco$features, task$feature_names)

	loco$compute()
	expect_importance_dt(loco$importance(), features = task$feature_names)
})

test_that("LOCO with feature subset", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	task = tgen("friedman1")$generate(n = 100)
	learner = lrn("regr.ranger", num.trees = 20)
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
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	task = tgen("friedman1")$generate(n = 150)
	learner = lrn("regr.ranger", num.trees = 20)
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
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	task = tgen("friedman1")$generate(n = 150)

	loco = LOCO$new(
		task = task,
		learner = lrn("regr.ranger", num.trees = 20),
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
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	test_friedman1_sensible_ranking(
		LOCO,
		learner = lrn("regr.ranger", num.trees = 50),
		measure = msr("regr.mse"),
		n_repeats = 5L
	)
})

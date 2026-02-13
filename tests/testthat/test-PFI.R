# =============================================================================
# PFI Tests using higher-level test helpers
# =============================================================================

# -----------------------------------------------------------------------------
# Basic functionality
# -----------------------------------------------------------------------------

test_that("PFI default behavior with minimal parameters", {
	test_default_behavior(PFI, task_type = "regr", n_repeats = 1L)
})

test_that("PFI basic workflow with classification", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	task = tgen("2dnormals")$generate(n = 100)

	test_basic_workflow(
		PFI,
		task = task,
		learner = lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
		measure = msr("classif.ce"),
		expected_classes = c("FeatureImportanceMethod", "PFI"),
		n_repeats = 1L
	)
})

test_that("PFI featureless learner produces zero importance", {
	test_featureless_zero_importance(PFI, task_type = "classif", n_repeats = 1L)
})

# -----------------------------------------------------------------------------
# Repeats and scores
# -----------------------------------------------------------------------------

test_that("PFI multiple repeats and scores structure", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	task = tgen("friedman1")$generate(n = 200)

	test_n_repeats_and_scores(
		PFI,
		task = task,
		learner = lrn("regr.ranger", num.trees = 50),
		measure = msr("regr.mse"),
		n_repeats = 2L
	)
})

test_that("PFI single feature", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	task = tgen("friedman1")$generate(n = 200)

	test_single_feature(
		PFI,
		task = task,
		learner = lrn("regr.ranger", num.trees = 50),
		measure = msr("regr.mse"),
		feature = "important4",
		n_repeats = 2L
	)
})

# -----------------------------------------------------------------------------
# Relation parameter
# -----------------------------------------------------------------------------

test_that("PFI difference vs ratio relations", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	task = tgen("2dnormals")$generate(n = 100)

	test_relation_parameter(
		PFI,
		task = task,
		learner = lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
		measure = msr("classif.ce"),
		n_repeats = 1L
	)
})

# -----------------------------------------------------------------------------
# Sensible results
# -----------------------------------------------------------------------------

test_that("PFI friedman1 produces sensible ranking", {
	test_friedman1_sensible_ranking(PFI, n_repeats = 5L)
})

# -----------------------------------------------------------------------------
# Grouped importance
# -----------------------------------------------------------------------------

test_that("PFI with feature groups", {
	task = tgen("friedman1")$generate(n = 200)

	groups = list(
		important_group = c("important1", "important2", "important3"),
		unimportant_group = c("unimportant1", "unimportant2")
	)

	test_grouped_importance(
		PFI,
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		groups = groups,
		expected_classes = c("FeatureImportanceMethod", "PFI"),
		n_repeats = 1L
	)
})

# -----------------------------------------------------------------------------
# PFI-specific tests (not covered by generic helpers)
# -----------------------------------------------------------------------------

test_that("PFI scores and obs_losses agree", {
	task = tgen("friedman1")$generate(n = 200)

	pfi = PFI$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		resampling = rsmp("cv", folds = 3),
		n_repeats = 2
	)

	pfi$compute()

	importance_agg = pfi$importance()
	importance_scores = pfi$scores()[, .(iter_rsmp, iter_repeat, feature, importance)][
		order(iter_rsmp, iter_repeat, feature)
	]
	importance_obs_loss = pfi$obs_loss()

	expect_equal(
		importance_agg,
		importance_scores[, list(importance = mean(importance)), by = "feature"],
		ignore_attr = TRUE
	)

	# Aggregate squared errors to get mse per iteration
	obs_agg = importance_obs_loss[,
		list(importance = mean(obs_importance)),
		by = c("iter_rsmp", "iter_repeat", "feature")
	][order(iter_rsmp, iter_repeat, feature)]

	expect_equal(importance_scores, obs_agg, tolerance = sqrt(.Machine$double.eps))
})

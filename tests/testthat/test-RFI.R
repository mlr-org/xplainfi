# =============================================================================
# RFI Tests using higher-level test helpers
# =============================================================================

# -----------------------------------------------------------------------------
# Basic functionality
# -----------------------------------------------------------------------------

test_that("RFI constructor validation", {
	test_constructor_validation(RFI)
})

test_that("RFI default behavior with minimal parameters", {
	skip_if_not_installed("arf")

	set.seed(123)
	# RFI warns when conditioning_set is not specified (defaults to empty)
	expect_warning(
		test_default_behavior(RFI, task_type = "regr")
	)
})

test_that("RFI basic workflow with classification", {
	skip_if_not_installed("arf")

	set.seed(123)
	task = tgen("2dnormals")$generate(n = 100)

	test_basic_workflow(
		RFI,
		task = task,
		learner = lrn("classif.rpart", predict_type = "prob"),
		measure = msr("classif.ce"),
		expected_classes = c("FeatureImportanceMethod", "PerturbationImportance", "RFI"),
		conditioning_set = "x2"
	)
})

test_that("RFI uses ConditionalARFSampler by default", {
	skip_if_not_installed("arf")

	set.seed(123)
	task = tgen("xor")$generate(n = 100)

	rfi = RFI$new(
		task = task,
		learner = lrn("classif.rpart", predict_type = "prob"),
		measure = msr("classif.ce"),
		conditioning_set = "x2"
	)

	checkmate::expect_r6(rfi$sampler, "ConditionalARFSampler")
	expect_equal(rfi$label, "Relative Feature Importance")
})

test_that("RFI featureless learner produces zero importance", {
	skip_if_not_installed("arf")

	set.seed(123)
	test_featureless_zero_importance(RFI, task_type = "classif", conditioning_set = "x1")
})

# -----------------------------------------------------------------------------
# Repeats and scores
# -----------------------------------------------------------------------------

test_that("RFI multiple repeats and scores structure", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = tgen("friedman1")$generate(n = 200)

	test_n_repeats_and_scores(
		RFI,
		task = task,
		learner = lrn("regr.ranger", num.trees = 50),
		measure = msr("regr.mse"),
		n_repeats = 2L,
		conditioning_set = "important1"
	)
})

test_that("RFI single feature", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = tgen("friedman1")$generate(n = 200)

	test_single_feature(
		RFI,
		task = task,
		learner = lrn("regr.ranger", num.trees = 50),
		measure = msr("regr.mse"),
		feature = "important4",
		n_repeats = 2L,
		conditioning_set = c("important1", "important2")
	)
})

# -----------------------------------------------------------------------------
# Relation parameter
# -----------------------------------------------------------------------------

test_that("RFI difference vs ratio relations", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = tgen("2dnormals")$generate(n = 100)

	test_relation_parameter(
		RFI,
		task = task,
		learner = lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
		measure = msr("classif.ce"),
		conditioning_set = character(0)
	)
})

# -----------------------------------------------------------------------------
# Sensible results
# -----------------------------------------------------------------------------

test_that("RFI friedman1 produces sensible ranking", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	test_friedman1_sensible_ranking(
		RFI,
		learner = lrn("regr.ranger", num.trees = 50),
		measure = msr("regr.mse"),
		conditioning_set = "important1"
	)
})

# -----------------------------------------------------------------------------
# Parameter validation
# -----------------------------------------------------------------------------

test_that("RFI parameter validation", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = tgen("2dnormals")$generate(n = 50)

	test_parameter_validation(
		RFI,
		task = task,
		learner = lrn("classif.ranger", num.trees = 10, predict_type = "prob"),
		measure = msr("classif.ce"),
		conditioning_set = "x1"
	)
})

test_that("RFI rejects invalid conditioning_set", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = tgen("2dnormals")$generate(n = 50)

	expect_error(RFI$new(
		task = task,
		learner = lrn("classif.ranger", num.trees = 10, predict_type = "prob"),
		measure = msr("classif.ce"),
		conditioning_set = c("nonexistent_feature")
	))
})

# -----------------------------------------------------------------------------
# Grouped importance
# -----------------------------------------------------------------------------

test_that("RFI with feature groups", {
	skip_if_not_installed("arf")

	set.seed(123)
	task = tgen("friedman1")$generate(n = 200)

	groups = list(
		early_important = c("important1", "important2"),
		late_important = c("important3", "important4"),
		noise = c("unimportant1", "unimportant2")
	)

	test_grouped_importance(
		RFI,
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		groups = groups,
		expected_classes = c("FeatureImportanceMethod", "PerturbationImportance", "RFI"),
		conditioning_set = "important5"
	)
})

# -----------------------------------------------------------------------------
# Custom samplers
# -----------------------------------------------------------------------------

test_that("RFI with custom ARF sampler", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = tgen("spirals")$generate(n = 100)

	test_custom_sampler(
		RFI,
		task = task,
		learner = lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
		measure = msr("classif.ce"),
		sampler = ConditionalARFSampler$new(task),
		expected_sampler_class = "ConditionalARFSampler",
		conditioning_set = "x1"
	)
})

# -----------------------------------------------------------------------------
# RFI-specific: conditioning_set behavior
# -----------------------------------------------------------------------------

test_that("RFI with custom conditioning set", {
	skip_if_not_installed("arf")

	set.seed(123)
	task = tgen("friedman1")$generate(n = 200)
	conditioning_set = c("important1", "important2")

	rfi = RFI$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		conditioning_set = conditioning_set
	)

	expect_identical(rfi$param_set$values$conditioning_set, conditioning_set)

	rfi$compute()
	expect_importance_dt(rfi$importance(), features = rfi$features)
})

test_that("RFI with empty conditioning set (equivalent to PFI)", {
	skip_if_not_installed("arf")

	set.seed(123)
	task = tgen("friedman1")$generate(n = 200)

	# RFI with empty conditioning set warns
	expect_warning({
		rfi = RFI$new(
			task = task,
			learner = lrn("regr.rpart"),
			measure = msr("regr.mse")
		)
	})

	expect_equal(length(rfi$param_set$values$conditioning_set), 0)

	rfi$compute()
	expect_importance_dt(rfi$importance(), features = rfi$features)
})

test_that("RFI with single conditioning feature", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = tgen("2dnormals")$generate(n = 100)

	rfi = RFI$new(
		task = task,
		learner = lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
		measure = msr("classif.ce"),
		conditioning_set = "x1"
	)

	expect_equal(length(rfi$param_set$values$conditioning_set), 1)
	expect_equal(rfi$param_set$values$conditioning_set, "x1")

	rfi$compute()
	expect_importance_dt(rfi$importance(), features = rfi$features)
})

test_that("RFI different conditioning sets produce different results", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = tgen("friedman1")$generate(n = 200)
	learner = lrn("regr.ranger", num.trees = 50)
	measure = msr("regr.mse")

	# RFI with empty conditioning set
	rfi_empty = RFI$new(
		task = task,
		learner = learner,
		measure = measure,
		conditioning_set = character(0),
		n_repeats = 2
	)

	# RFI with one conditioning feature
	rfi_one = RFI$new(
		task = task,
		learner = learner,
		measure = measure,
		conditioning_set = "important1",
		n_repeats = 2
	)

	# RFI with multiple conditioning features
	rfi_multi = RFI$new(
		task = task,
		learner = learner,
		measure = measure,
		conditioning_set = c("important1", "important2"),
		n_repeats = 2
	)

	rfi_empty$compute()
	rfi_one$compute()
	rfi_multi$compute()

	result_empty = rfi_empty$importance()
	result_one = rfi_one$importance()
	result_multi = rfi_multi$importance()

	# All should be valid importance tables
	expect_importance_dt(result_empty, features = rfi_empty$features)
	expect_importance_dt(result_one, features = rfi_one$features)
	expect_importance_dt(result_multi, features = rfi_multi$features)

	# Results should generally be different
	expect_false(all(abs(result_empty$importance - result_one$importance) < 1e-10))
	expect_false(all(abs(result_one$importance - result_multi$importance) < 1e-10))
})

# Tests for ConditionalARFSampler

test_that("ConditionalARFSampler initialization works", {
	skip_if_not_installed("arf")
	library(mlr3)

	task = tgen("circle", d = 5)$generate(n = 100)
	sampler = ConditionalARFSampler$new(task)

	expect_s3_class(sampler, "ConditionalARFSampler")
	expect_s3_class(sampler, "ConditionalSampler")
	expect_equal(sampler$label, "Adversarial Random Forest sampler")
	expect_s3_class(sampler$param_set, "ParamSet")

	# Check expected parameters
	expected_params = c("conditioning_set", "finite_bounds", "round", "stepsize", "verbose", "parallel")
	expect_true(all(expected_params %in% sampler$param_set$ids()))

	# Check that ARF model was fitted
	expect_s3_class(sampler$arf_model, "ranger")
	expect_type(sampler$psi, "list")
})

test_that("ConditionalARFSampler sampling works", {
	skip_if_not_installed("arf")
	library(mlr3)

	task = tgen("circle", d = 5)$generate(n = 100)
	sampler = ConditionalARFSampler$new(task)

	# Marginal sampling
	expect_marginal_sampling(sampler, feature = "x1", row_ids = 1:20)

	# Conditional sampling
	expect_conditional_sampling(
		sampler,
		feature = "x1",
		conditioning_set = c("x2", "x3"),
		row_ids = 1:20
	)

	# Multiple features
	expect_conditional_sampling(
		sampler,
		feature = c("x1", "x2"),
		conditioning_set = "x3",
		row_ids = 1:20
	)
})

test_that("ConditionalARFSampler handles empty conditioning set", {
	skip_if_not_installed("arf")
	library(mlr3)

	task = tgen("circle", d = 5)$generate(n = 100)
	sampler = ConditionalARFSampler$new(task, conditioning_set = character(0))
	data = task$data()

	sampled = sampler$sample("x1")

	expect_sampler_output_structure(sampled, task, nrows = 100)

	# Sampled feature should differ from original
	expect_sampled_features_changed(sampled, data, "x1")

	# Non-sampled features should remain unchanged
	expect_non_sampled_unchanged(sampled, data, c("x2", "x3", "x4", "x5"))

	# Verify empty conditioning is stored
	expect_equal(sampler$param_set$values$conditioning_set, character(0))
})

test_that("ConditionalARFSampler works with different task types", {
	skip_if_not_installed("arf")
	library(mlr3)

	# Regression task
	task_regr = tgen("circle", d = 4)$generate(n = 100)
	sampler_regr = ConditionalARFSampler$new(task_regr)
	expect_conditional_sampling(sampler_regr, "x1", "x2", row_ids = 1:20)

	# Binary classification task
	task_classif = tsk("sonar")
	sampler_classif = ConditionalARFSampler$new(task_classif)
	expect_conditional_sampling(sampler_classif, "V1", "V2", row_ids = 1:20)

	# Multiclass classification task
	task_multi = tsk("iris")
	sampler_multi = ConditionalARFSampler$new(task_multi)
	expect_conditional_sampling(sampler_multi, "Sepal.Length", "Sepal.Width", row_ids = 1:20)
})

test_that("ConditionalARFSampler param_set structure", {
	skip_if_not_installed("arf")
	library(mlr3)

	task = tgen("circle", d = 5)$generate(n = 50)
	sampler = ConditionalARFSampler$new(task, finite_bounds = "no")

	# Check that removed/hardcoded parameters are not exposed
	hardcoded = c("evidence_row_mode", "sample_NAs", "nomatch", "n_synth")
	expect_false(any(hardcoded %in% sampler$param_set$ids()))

	# Check parameter types and defaults
	expect_equal(sampler$param_set$params[id == "finite_bounds"]$cls, "ParamFct")
	expect_equal(sampler$param_set$params[id == "finite_bounds"]$default[[1]], "no")
	expect_equal(sampler$param_set$params[id == "round"]$default[[1]], TRUE)
	expect_equal(sampler$param_set$params[id == "stepsize"]$default[[1]], 0)
	expect_equal(sampler$param_set$params[id == "verbose"]$default[[1]], FALSE)
	expect_equal(sampler$param_set$params[id == "parallel"]$default[[1]], FALSE)
})

test_that("ConditionalARFSampler parameter priority and storage", {
	skip_if_not_installed("arf")
	library(mlr3)

	task = tgen("circle", d = 5)$generate(n = 50)
	sampler = ConditionalARFSampler$new(
		task,
		conditioning_set = "x2",
		verbose = FALSE,
		parallel = FALSE
	)

	# Stored parameters should be used
	sampled1 = sampler$sample("x1")
	expect_sampler_output_structure(sampled1, task, nrows = 50)

	# Override at call time
	sampled2 = sampler$sample("x1", verbose = TRUE)
	expect_sampler_output_structure(sampled2, task, nrows = 50)

	# Stored parameters remain unchanged
	expect_equal(sampler$param_set$values$verbose, FALSE)
	expect_equal(sampler$param_set$values$parallel, FALSE)
	expect_equal(sampler$param_set$values$conditioning_set, "x2")
})

test_that("ConditionalARFSampler conditioning_set parameter behavior", {
	skip_if_not_installed("arf")
	library(mlr3)

	task = tgen("friedman1")$generate(n = 100)
	test_conditioning_set_behavior(ConditionalARFSampler, task, verbose = FALSE)
})

test_that("ConditionalARFSampler preserves feature types", {
	skip_if_not_installed("arf")
	test_sampler_feature_types(ConditionalARFSampler, verbose = FALSE)
})

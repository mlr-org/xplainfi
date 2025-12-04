# Tests for ConditionalCtreeSampler

test_that("ConditionalCtreeSampler initialization works", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = ConditionalCtreeSampler$new(task)

	expect_s3_class(sampler, "ConditionalCtreeSampler")
	expect_s3_class(sampler, "ConditionalSampler")
	expect_equal(sampler$label, "Conditional Inference Tree Conditional Sampler")
	expect_s3_class(sampler$param_set, "ParamSet")

	# Check default parameters
	expect_equal(sampler$param_set$values$mincriterion, 0.95)
	expect_equal(sampler$param_set$values$minsplit, 20L)
	expect_equal(sampler$param_set$values$minbucket, 7L)
	expect_equal(sampler$param_set$values$use_cache, TRUE)

	# Check tree cache initialized
	expect_type(sampler$tree_cache, "environment")
})

test_that("ConditionalCtreeSampler works with custom parameters", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)

	sampler = ConditionalCtreeSampler$new(
		task,
		mincriterion = 0.90,
		minsplit = 10L,
		minbucket = 5L,
		use_cache = FALSE
	)

	expect_equal(sampler$param_set$values$mincriterion, 0.90)
	expect_equal(sampler$param_set$values$minsplit, 10L)
	expect_equal(sampler$param_set$values$minbucket, 5L)
	expect_equal(sampler$param_set$values$use_cache, FALSE)
})

test_that("ConditionalCtreeSampler sampling works", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = ConditionalCtreeSampler$new(task)
	data = task$data()

	# Marginal sampling
	expect_marginal_sampling(sampler, feature = "important1", row_ids = 1:20)

	# Conditional sampling
	expect_conditional_sampling(
		sampler,
		feature = "important2",
		conditioning_set = "important1",
		row_ids = 1:20
	)

	# Multiple features
	expect_conditional_sampling(
		sampler,
		feature = c("important2", "important3"),
		conditioning_set = "important1",
		row_ids = 1:20
	)

	# Multiple conditioning features
	expect_conditional_sampling(
		sampler,
		feature = "important3",
		conditioning_set = c("important1", "important2"),
		row_ids = 1:20
	)
})

test_that("ConditionalCtreeSampler sample_newdata works", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = ConditionalCtreeSampler$new(task)
	test_data = task$data(rows = 1:10)

	sampled = sampler$sample_newdata(
		feature = c("important2", "important3"),
		newdata = test_data,
		conditioning_set = "important1"
	)

	expect_sampler_output_structure(sampled, task, nrows = 10)
	expect_feature_type_consistency(sampled, task)
	expect_conditioning_preserved(sampled, test_data, "important1")
})

test_that("ConditionalCtreeSampler caching works", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	test_data = task$data(rows = 1:10)

	# With caching enabled
	sampler_cached = ConditionalCtreeSampler$new(task, use_cache = TRUE)

	# First call should build tree
	expect_length(ls(sampler_cached$tree_cache), 0)
	sampled1 = sampler_cached$sample_newdata(
		feature = "important2",
		newdata = test_data,
		conditioning_set = "important1"
	)
	expect_length(ls(sampler_cached$tree_cache), 1)

	# Second call should use cached tree
	sampled2 = sampler_cached$sample_newdata(
		feature = "important2",
		newdata = test_data,
		conditioning_set = "important1"
	)
	expect_length(ls(sampler_cached$tree_cache), 1)

	# Without caching
	sampler_uncached = ConditionalCtreeSampler$new(task, use_cache = FALSE)
	sampled3 = sampler_uncached$sample_newdata(
		feature = "important2",
		newdata = test_data,
		conditioning_set = "important1"
	)
	expect_length(ls(sampler_uncached$tree_cache), 0)
})

test_that("ConditionalCtreeSampler handles single observation", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = ConditionalCtreeSampler$new(task)
	test_data = task$data(rows = 1)

	sampled = sampler$sample_newdata(
		feature = "important2",
		newdata = test_data,
		conditioning_set = "important1"
	)

	expect_sampler_output_structure(sampled, task, nrows = 1)
	expect_conditioning_preserved(sampled, test_data, "important1")
})

test_that("ConditionalCtreeSampler is reproducible with seed", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = ConditionalCtreeSampler$new(task)
	test_data = task$data(rows = 1:10)

	sampled1 = withr::with_seed(123, {
		sampler$sample_newdata(
			feature = "important2",
			newdata = test_data,
			conditioning_set = "important1"
		)
	})

	sampled2 = withr::with_seed(123, {
		sampler$sample_newdata(
			feature = "important2",
			newdata = test_data,
			conditioning_set = "important1"
		)
	})

	expect_identical(sampled1$important2, sampled2$important2)
})

test_that("ConditionalCtreeSampler conditioning_set parameter behavior", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	test_conditioning_set_behavior(ConditionalCtreeSampler, task)
})

test_that("ConditionalCtreeSampler preserves feature types", {
	test_sampler_feature_types(ConditionalCtreeSampler)
})

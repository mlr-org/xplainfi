# Tests for MarginalPermutationSampler

test_that("MarginalPermutationSampler initialization works", {
	library(mlr3)
	task = tgen("circle", d = 5)$generate(n = 100)
	sampler = MarginalPermutationSampler$new(task)

	expect_s3_class(sampler, "MarginalPermutationSampler")
	expect_s3_class(sampler, "MarginalSampler")
	expect_equal(sampler$label, "Permutation sampler")
	expect_s3_class(sampler$param_set, "ParamSet")
})

test_that("MarginalPermutationSampler sampling works", {
	library(mlr3)
	task = tgen("circle", d = 5)$generate(n = 100)
	sampler = MarginalPermutationSampler$new(task)
	data = task$data()

	# Test single feature sampling
	sampled_data = sampler$sample("x1")

	expect_sampler_output_structure(sampled_data, task, nrows = 100)
	expect_feature_type_consistency(sampled_data, task)

	# Permuted values come from original distribution
	expect_setequal(sampled_data$x1, data$x1)

	# Other features unchanged
	expect_conditioning_preserved(sampled_data, data, c("x2", "x3", "x4", "x5"))
})

test_that("MarginalPermutationSampler handles multiple features", {
	library(mlr3)
	task = tgen("circle", d = 5)$generate(n = 100)
	sampler = MarginalPermutationSampler$new(task)
	data = task$data()

	features = c("x1", "x2", "x3")
	sampled_data = sampler$sample(features)

	expect_sampler_output_structure(sampled_data, task, nrows = 100)
	expect_feature_type_consistency(sampled_data, task)

	# Permuted values come from original distribution
	for (feat in features) {
		expect_setequal(sampled_data[[feat]], data[[feat]])
	}

	# Non-sampled features unchanged
	expect_conditioning_preserved(sampled_data, data, c("x4", "x5"))
})

test_that("MarginalPermutationSampler works with different task types", {
	library(mlr3)

	# Regression task
	task_regr = tgen("circle", d = 4)$generate(n = 100)
	sampler_regr = MarginalPermutationSampler$new(task_regr)
	sampled_regr = sampler_regr$sample("x1")
	expect_sampler_output_structure(sampled_regr, task_regr, nrows = 100)

	# Binary classification task
	task_classif = tsk("sonar")
	sampler_classif = MarginalPermutationSampler$new(task_classif)
	sampled_classif = sampler_classif$sample("V1")
	expect_sampler_output_structure(sampled_classif, task_classif, nrows = task_classif$nrow)

	# Multiclass classification task
	task_multi = tsk("iris")
	sampler_multi = MarginalPermutationSampler$new(task_multi)
	sampled_multi = sampler_multi$sample("Sepal.Length")
	expect_sampler_output_structure(sampled_multi, task_multi, nrows = 150)
})

test_that("MarginalPermutationSampler preserves feature types", {
	test_sampler_feature_types(MarginalPermutationSampler)
})

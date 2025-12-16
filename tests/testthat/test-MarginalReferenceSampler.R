# Tests for MarginalReferenceSampler

test_that("MarginalReferenceSampler initialization works", {
	task = tgen("circle", d = 5)$generate(n = 100)
	sampler = MarginalReferenceSampler$new(task)

	expect_s3_class(sampler, "MarginalReferenceSampler")
	expect_s3_class(sampler, "MarginalSampler")
	expect_equal(sampler$label, "Marginal reference sampler")
	expect_s3_class(sampler$param_set, "ParamSet")

	# Reference data should be stored
	expect_true(data.table::is.data.table(sampler$reference_data))
	expect_equal(nrow(sampler$reference_data), 100)
	expect_equal(names(sampler$reference_data), task$feature_names)
})

test_that("MarginalReferenceSampler sampling works", {
	task = tgen("circle", d = 5)$generate(n = 100)
	sampler = MarginalReferenceSampler$new(task)
	data = task$data()

	# Test single feature sampling
	sampled_data = sampler$sample("x1")

	expect_sampler_output_structure(sampled_data, task, nrows = 100)
	expect_feature_type_consistency(sampled_data, task)

	# Sampled values should come from reference data
	expect_true(all(sampled_data$x1 %in% sampler$reference_data$x1))
})

test_that("MarginalReferenceSampler handles multiple features", {
	task = tgen("circle", d = 5)$generate(n = 100)
	sampler = MarginalReferenceSampler$new(task)
	data = task$data()

	features = c("x1", "x2", "x3")
	sampled_data = sampler$sample(features)

	expect_sampler_output_structure(sampled_data, task, nrows = 100)
	expect_feature_type_consistency(sampled_data, task)

	# Sampled values should come from reference data
	for (feat in features) {
		expect_true(all(sampled_data[[feat]] %in% sampler$reference_data[[feat]]))
	}
})

test_that("MarginalReferenceSampler with n_samples", {
	task = tgen("circle", d = 5)$generate(n = 100)

	# Create sampler with subsampled reference data
	sampler = MarginalReferenceSampler$new(task, n_samples = 50L)

	expect_equal(nrow(sampler$reference_data), 50)

	sampled_data = sampler$sample("x1", row_ids = 1:10)

	expect_sampler_output_structure(sampled_data, task, nrows = 10)

	# All sampled values should be from the task (reference is subset of task)
	expect_true(all(sampled_data$x1 %in% task$data()$x1))
})

test_that("MarginalReferenceSampler sample_newdata works", {
	task = tgen("circle", d = 5)$generate(n = 100)
	sampler = MarginalReferenceSampler$new(task)

	newdata = tgen("circle", d = 5)$generate(n = 20)$data()
	sampled_data = sampler$sample_newdata("x1", newdata = newdata)

	expect_sampler_output_structure(sampled_data, task, nrows = 20)

	# Sampled values should come from reference data
	expect_true(all(sampled_data$x1 %in% sampler$reference_data$x1))

	# Original newdata should be unchanged
	expect_equal(nrow(newdata), 20)
})

test_that("MarginalReferenceSampler preserves within-row dependencies", {
	set.seed(123)
	n = 100
	x1 = rnorm(n)
	data = data.table::data.table(
		x1 = x1,
		x2 = x1 * 2, # Perfect correlation
		x3 = x1 * 3,
		y = x1 + rnorm(n, sd = 0.1)
	)
	task = as_task_regr(data, target = "y")

	sampler = MarginalReferenceSampler$new(task)

	# Sample multiple correlated features
	sampled_data = sampler$sample(c("x1", "x2", "x3"), row_ids = 1:50)

	# Within each sampled row, the relationships should be preserved
	expect_true(all(abs(sampled_data$x2 - sampled_data$x1 * 2) < 1e-10))
	expect_true(all(abs(sampled_data$x3 - sampled_data$x1 * 3) < 1e-10))
})

test_that("MarginalReferenceSampler vs MarginalPermutationSampler difference", {
	set.seed(123)
	n = 100
	x1 = rnorm(n)
	data = data.table::data.table(
		x1 = x1,
		x2 = x1 * 2 + rnorm(n, sd = 0.1),
		y = x1 + rnorm(n, sd = 0.1)
	)
	task = as_task_regr(data, target = "y")

	# MarginalReferenceSampler preserves within-row correlation
	marginal_ref = MarginalReferenceSampler$new(task)
	sampled_ref = marginal_ref$sample(c("x1", "x2"))
	cor_ref = cor(sampled_ref$x1, sampled_ref$x2)

	# MarginalPermutationSampler breaks all correlations
	permutation = MarginalPermutationSampler$new(task)
	set.seed(456)
	sampled_perm = permutation$sample(c("x1", "x2"))
	cor_perm = cor(sampled_perm$x1, sampled_perm$x2)

	# MarginalReferenceSampler should preserve correlation better
	expect_gt(abs(cor_ref), abs(cor_perm) * 0.5)
})

test_that("MarginalReferenceSampler works with different task types", {

	# Regression task
	task_regr = tgen("circle", d = 4)$generate(n = 100)
	sampler_regr = MarginalReferenceSampler$new(task_regr)
	sampled_regr = sampler_regr$sample("x1")
	expect_sampler_output_structure(sampled_regr, task_regr, nrows = 100)

	# Binary classification task
	task_classif = tsk("sonar")
	sampler_classif = MarginalReferenceSampler$new(task_classif)
	sampled_classif = sampler_classif$sample("V1")
	expect_sampler_output_structure(sampled_classif, task_classif, nrows = task_classif$nrow)

	# Multiclass classification task
	task_multi = tsk("iris")
	sampler_multi = MarginalReferenceSampler$new(task_multi)
	sampled_multi = sampler_multi$sample("Sepal.Length")
	expect_sampler_output_structure(sampled_multi, task_multi, nrows = 150)
})

test_that("MarginalReferenceSampler handles n_samples edge cases", {
	task = tgen("circle", d = 5)$generate(n = 100)

	# n_samples larger than task size
	sampler = MarginalReferenceSampler$new(task, n_samples = 200L)
	expect_equal(nrow(sampler$reference_data), 100) # Capped at task size

	# n_samples = 1
	sampler_small = MarginalReferenceSampler$new(task, n_samples = 1L)
	expect_equal(nrow(sampler_small$reference_data), 1)
})

test_that("MarginalReferenceSampler preserves feature types", {
	test_sampler_feature_types(MarginalReferenceSampler)
})

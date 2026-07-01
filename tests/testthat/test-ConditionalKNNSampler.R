# Tests for ConditionalKNNSampler

test_that("ConditionalKNNSampler initialization works", {
	task = tgen("friedman1")$generate(n = 100)
	sampler = ConditionalKNNSampler$new(task, k = 5L)

	expect_s3_class(sampler, "ConditionalKNNSampler")
	expect_s3_class(sampler, "ConditionalSampler")
	expect_equal(sampler$label, "k-Nearest Neighbors Conditional Sampler")
	expect_s3_class(sampler$param_set, "ParamSet")

	# Check k parameter
	expect_equal(sampler$param_set$values$k, 5L)
})

test_that("ConditionalKNNSampler works with default k", {
	task = tgen("friedman1")$generate(n = 100)
	sampler = ConditionalKNNSampler$new(task) # Default k = 5L

	expect_equal(sampler$param_set$values$k, 5L)
})

test_that("ConditionalKNNSampler sampling works", {
	task = tgen("friedman1")$generate(n = 100)
	sampler = ConditionalKNNSampler$new(task, k = 5L)

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
})

test_that("ConditionalKNNSampler sample_newdata works", {
	task = tgen("friedman1")$generate(n = 100)
	sampler = ConditionalKNNSampler$new(task, k = 5L)
	test_data = task$data(rows = 1:10)

	sampled = sampler$sample_newdata(
		feature = c("important2", "important3"),
		newdata = test_data,
		conditioning_set = "important1"
	)

	expect_sampler_output_structure(sampled, task, nrows = 10)
	expect_feature_type_consistency(sampled, task)
	expect_non_sampled_unchanged(sampled, test_data, "important1")
})

test_that("ConditionalKNNSampler handles different k values", {
	task = tgen("friedman1")$generate(n = 100)
	test_data = task$data(rows = 1:10)

	# Test with k=1 (nearest neighbor)
	sampler_k1 = ConditionalKNNSampler$new(task, k = 1L)
	expect_equal(sampler_k1$param_set$values$k, 1L)

	# Test with k=20
	sampler_k20 = ConditionalKNNSampler$new(task, k = 20L)
	expect_equal(sampler_k20$param_set$values$k, 20L)

	# Both should work
	sampled_k1 = sampler_k1$sample_newdata(
		feature = "important2",
		newdata = test_data,
		conditioning_set = "important1"
	)
	sampled_k20 = sampler_k20$sample_newdata(
		feature = "important2",
		newdata = test_data,
		conditioning_set = "important1"
	)

	expect_sampler_output_structure(sampled_k1, task, nrows = 10)
	expect_sampler_output_structure(sampled_k20, task, nrows = 10)
})

test_that("ConditionalKNNSampler handles k > n_train", {
	task = tgen("friedman1")$generate(n = 50)
	sampler = ConditionalKNNSampler$new(task, k = 100L)
	test_data = task$data(rows = 1:5)

	# Should still work (uses all available training data)
	sampled = sampler$sample_newdata(
		feature = "important2",
		newdata = test_data,
		conditioning_set = "important1"
	)

	expect_sampler_output_structure(sampled, task, nrows = 5)
})

test_that("ConditionalKNNSampler is reproducible with seed", {
	task = tgen("friedman1")$generate(n = 100)
	sampler = ConditionalKNNSampler$new(task, k = 5L)
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

test_that("ConditionalKNNSampler conditioning_set parameter behavior", {
	task = tgen("friedman1")$generate(n = 100)
	test_conditioning_set_behavior(ConditionalKNNSampler, task, k = 5L)
})

test_that("ConditionalKNNSampler preserves feature types", {
	skip_if_not_installed("gower")
	test_sampler_feature_types(ConditionalKNNSampler, k = 5L)
})

test_that("ConditionalKNNSampler works with categorical features using Gower distance", {
	skip_if_not_installed("gower")

	task = tsk("penguins")
	data = task$data()
	sampler = ConditionalKNNSampler$new(task, k = 5L)
	test_data = task$data(rows = 1:10)

	# Sample conditioning on categorical feature
	sampled = sampler$sample_newdata(
		feature = "bill_length",
		newdata = test_data,
		conditioning_set = "island"
	)

	expect_sampler_output_structure(sampled, task, nrows = 10)
	expect_non_sampled_unchanged(sampled, test_data, "island")

	# Sampled values should come from training data
	expect_true(all(sampled$bill_length %in% data$bill_length))
})

test_that("ConditionalKNNSampler works with mixed numeric and categorical conditioning", {
	skip_if_not_installed("gower")

	task = tsk("penguins")
	sampler = ConditionalKNNSampler$new(task, k = 5L)
	test_data = task$data(rows = 1:10)

	# Sample conditioning on both numeric and factor features
	sampled = sampler$sample_newdata(
		feature = "bill_length",
		newdata = test_data,
		conditioning_set = c("island", "body_mass")
	)

	expect_sampler_output_structure(sampled, task, nrows = 10)
	expect_non_sampled_unchanged(sampled, test_data, c("island", "body_mass"))
})

test_that("ConditionalKNNSampler obeys draw-major order under samples_per_row > 1", {
	set.seed(123L)
	n = 20L
	dt = data.table::data.table(
		y = rnorm(n),
		x1 = rnorm(n),
		x2 = rnorm(n),
		tag = seq_len(n) + 0.5
	)
	task = mlr3::as_task_regr(dt, target = "y")
	sampler = ConditionalKNNSampler$new(task, conditioning_set = c("x2", "tag"), k = 3L)

	expect_draw_major_row_order(
		sampler,
		task,
		feature = "x1",
		tag_column = "tag",
		samples_per_row = 4L
	)
})

test_that("ConditionalKNNSampler Gower branch obeys draw-major order (samples_per_row > 1)", {
	skip_if_not_installed("gower")
	set.seed(503L)
	n = 20L
	dt = data.table::data.table(
		y = rnorm(n),
		x1 = rnorm(n),
		g = factor(sample(c("a", "b", "c"), n, replace = TRUE)),
		tag = seq_len(n) + 0.5
	)
	task = mlr3::as_task_regr(dt, target = "y")
	# factor in conditioning set -> Gower distance branch
	sampler = ConditionalKNNSampler$new(task, conditioning_set = "g", k = 3L)

	expect_draw_major_row_order(
		sampler,
		task,
		feature = "x1",
		tag_column = "tag",
		samples_per_row = 4L
	)
})

test_that("ConditionalKNNSampler marginal-fallback branch obeys draw-major order (samples_per_row > 1)", {
	set.seed(617L)
	n = 20L
	dt = data.table::data.table(
		y = rnorm(n),
		x1 = rnorm(n),
		x2 = rnorm(n),
		tag = seq_len(n) + 0.5
	)
	task = mlr3::as_task_regr(dt, target = "y")
	# conditioning_set = character(0) is non-NULL so it is stored in param_set;
	# the helper's $sample() call (which passes conditioning_set = NULL) resolves via
	# resolve_param(NULL, stored = character(0), default) -> character(0), hitting the
	# length(conditioning_set) == 0 marginal-fallback branch at ConditionalKNNSampler.R:125.
	sampler = ConditionalKNNSampler$new(task, conditioning_set = character(0), k = 3L)

	expect_draw_major_row_order(
		sampler,
		task,
		feature = "x1",
		tag_column = "tag",
		samples_per_row = 4L
	)
})

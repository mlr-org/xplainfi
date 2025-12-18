# Tests for KnockoffSampler

test_that("KnockoffSampler initialization works", {
	skip_if_not_installed("knockoff")

	n = 100
	task = tgen("friedman1")$generate(n = n)
	sampler = KnockoffSampler$new(task)

	expect_s3_class(sampler, "KnockoffSampler")
	expect_equal(sampler$label, "Knockoff sampler")
	expect_s3_class(sampler$param_set, "ParamSet")
	expect_true("knockoff_fun" %in% sampler$param_set$ids())

	# Check that knockoff matrix was created
	expect_true(data.table::is.data.table(sampler$x_tilde))
	expect_equal(nrow(sampler$x_tilde), n)
	# x_tilde has feature columns + internal ..row_id column
	expect_equal(ncol(sampler$x_tilde), length(task$feature_names) + 1L)
	expect_equal(setdiff(names(sampler$x_tilde), "..row_id"), task$feature_names)
})

test_that("KnockoffSampler sampling works", {
	skip_if_not_installed("knockoff")

	task = tgen("friedman1")$generate(n = 100)
	sampler = KnockoffSampler$new(task)
	data = task$data()

	# Test single feature sampling
	sampled_data = sampler$sample("important1")

	expect_sampler_output_structure(sampled_data, task, nrows = 100)
	expect_feature_type_consistency(sampled_data, task)

	# Sampled feature values are from knockoff matrix
	expect_identical(sampled_data$important1, sampler$x_tilde$important1)
})

test_that("KnockoffSampler handles multiple features", {
	skip_if_not_installed("knockoff")

	task = tgen("friedman1")$generate(n = 100)
	sampler = KnockoffSampler$new(task)
	data = task$data()

	features = c("important1", "important2")
	sampled_data = sampler$sample(features)

	expect_sampler_output_structure(sampled_data, task, nrows = 100)
	expect_feature_type_consistency(sampled_data, task)

	# Check that sampled feature values are from knockoff matrix
	for (feat in features) {
		expect_identical(sampled_data[[feat]], sampler$x_tilde[[feat]])
	}
})

test_that("KnockoffSampler works with different numeric tasks", {
	skip_if_not_installed("knockoff")

	# Friedman1 regression task (all numeric)
	task_friedman = tgen("friedman1")$generate(n = 100)
	sampler_friedman = KnockoffSampler$new(task_friedman)
	sampled_friedman = sampler_friedman$sample("important1")
	expect_sampler_output_structure(sampled_friedman, task_friedman, nrows = 100)

	# Circle task with specified dimensions (all numeric)
	task_circle = tgen("circle", d = 4)$generate(n = 80)
	sampler_circle = KnockoffSampler$new(task_circle)
	sampled_circle = sampler_circle$sample("x1")
	expect_sampler_output_structure(sampled_circle, task_circle, nrows = 80)

	# Custom numeric task
	set.seed(123)
	custom_data = data.table::data.table(
		x1 = rnorm(50),
		x2 = runif(50),
		x3 = rexp(50),
		y = rnorm(50)
	)
	task_custom = as_task_regr(custom_data, target = "y")
	sampler_custom = KnockoffSampler$new(task_custom)
	sampled_custom = sampler_custom$sample("x1")
	expect_sampler_output_structure(sampled_custom, task_custom, nrows = 50)
})

test_that("KnockoffSampler knockoff matrix properties", {
	skip_if_not_installed("knockoff")

	task = tgen("friedman1")$generate(n = 100)
	sampler = KnockoffSampler$new(task)

	# Knockoff matrix should have same dimensions as feature matrix (plus ..row_id column)
	feature_data = task$data(cols = task$feature_names)
	expect_equal(nrow(sampler$x_tilde), nrow(feature_data))
	expect_equal(ncol(sampler$x_tilde), ncol(feature_data) + 1L)
	expect_equal(setdiff(names(sampler$x_tilde), "..row_id"), names(feature_data))

	# Knockoff values should be numeric (since task is all numeric)
	expect_true(all(sapply(sampler$x_tilde, is.numeric)))

	# Knockoff values should be finite
	expect_true(all(sapply(sampler$x_tilde, function(x) all(is.finite(x)))))
})

test_that("KnockoffSampler custom knockoff function", {
	skip_if_not_installed("knockoff")

	task = tgen("friedman1")$generate(n = 50)

	# Custom knockoff function that returns scaled versions
	custom_knockoff_fun = function(x) {
		x_matrix = as.matrix(x)
		noise = matrix(rnorm(prod(dim(x_matrix)), sd = 0.1), nrow = nrow(x_matrix))
		return(x_matrix + noise)
	}

	sampler = KnockoffSampler$new(task, knockoff_fun = custom_knockoff_fun)

	expect_true(is.function(sampler$param_set$values$knockoff_fun))
	expect_true(data.table::is.data.table(sampler$x_tilde))
	expect_equal(nrow(sampler$x_tilde), 50)
	expect_equal(ncol(sampler$x_tilde), length(task$feature_names) + 1L)

	data = task$data()
	sampled_data = sampler$sample("important1")

	expect_true(data.table::is.data.table(sampled_data))
	expect_equal(nrow(sampled_data), 50)
	expect_false(identical(sampled_data$important1, data$important1))
	expect_identical(sampled_data$important1, sampler$x_tilde$important1)
})

test_that("KnockoffSampler reproducibility", {
	skip_if_not_installed("knockoff")

	task = tgen("friedman1")$generate(n = 80)

	set.seed(123)
	sampler1 = KnockoffSampler$new(task)

	set.seed(123)
	sampler2 = KnockoffSampler$new(task)

	# Dimensions should be identical
	expect_equal(dim(sampler1$x_tilde), dim(sampler2$x_tilde))
	expect_equal(names(sampler1$x_tilde), names(sampler2$x_tilde))
	expect_true("..row_id" %in% names(sampler1$x_tilde))

	# Sampling should be consistent within each sampler
	sampled1a = sampler1$sample("important1")
	sampled1b = sampler1$sample("important1")
	expect_identical(sampled1a$important1, sampled1b$important1)
})

test_that("KnockoffSampler edge cases", {
	skip_if_not_installed("knockoff")

	# Test with minimum viable task (single feature)
	single_feature_data = data.table::data.table(
		x1 = rnorm(30),
		y = rnorm(30)
	)
	task_single = as_task_regr(single_feature_data, target = "y")

	expect_warning({
		sampler_single = KnockoffSampler$new(task_single)
	})

	expect_equal(ncol(sampler_single$x_tilde), 2L) # x1 + ..row_id
	expect_equal(setdiff(names(sampler_single$x_tilde), "..row_id"), "x1")

	sampled_single = sampler_single$sample("x1")
	expect_true(data.table::is.data.table(sampled_single))
	expect_equal(nrow(sampled_single), 30)
})

test_that("KnockoffSampler fails with non-numeric features", {
	skip_if_not_installed("knockoff")

	# Factor features
	data_with_factor = data.table::data.table(
		x1 = rnorm(50),
		x2 = factor(sample(c("A", "B", "C"), 50, replace = TRUE)),
		x3 = rnorm(50),
		y = rnorm(50)
	)
	task_factor = as_task_regr(data_with_factor, target = "y")
	expect_error(KnockoffSampler$new(task_factor), class = "error")

	# Character features
	data_with_char = data.table::data.table(
		x1 = rnorm(50),
		x2 = sample(letters[1:5], 50, replace = TRUE),
		x3 = rnorm(50),
		y = rnorm(50)
	)
	task_char = as_task_regr(data_with_char, target = "y")
	expect_error(KnockoffSampler$new(task_char), class = "error")
})

test_that("KnockoffGaussianSampler works with all-numeric features", {
	skip_if_not_installed("knockoff")

	task = tgen("friedman1")$generate(n = 100)
	sampler = KnockoffGaussianSampler$new(task)

	expect_s3_class(sampler, "KnockoffGaussianSampler")
	expect_s3_class(sampler, "KnockoffSampler")
	expect_equal(sampler$label, "Gaussian Knockoff sampler")

	expect_true(data.table::is.data.table(sampler$x_tilde))
	expect_equal(nrow(sampler$x_tilde), 100)
	expect_equal(ncol(sampler$x_tilde), length(task$feature_names) + 1L)

	data = task$data()
	sampled_data = sampler$sample("important1")

	expect_sampler_output_structure(sampled_data, task, nrows = 100)
	expect_identical(sampled_data$important1, sampler$x_tilde$important1)

	# Test row_ids
	expect_equal(nrow(sampler$sample("important1", row_ids = 1:10)), 10)
	expect_equal(nrow(sampler$sample("important1", row_ids = 32)), 1)

	# Non-sampled features unchanged
	non_sampled = c(
		"important2",
		"important3",
		"important4",
		"important5",
		"unimportant1",
		"unimportant2",
		"unimportant3",
		"unimportant4",
		"unimportant5"
	)
	expect_equal(
		sampler$sample("important1", row_ids = 32)[, ..non_sampled],
		data[32, ..non_sampled]
	)

	# Multiple features
	features = c("important1", "important2", "important3")
	sampled_multi = sampler$sample(features)

	for (feat in features) {
		expect_false(identical(sampled_multi[[feat]], data[[feat]]))
		expect_identical(sampled_multi[[feat]], sampler$x_tilde[[feat]])
	}
})

test_that("KnockoffGaussianSampler fails with factor features", {
	skip_if_not_installed("knockoff")

	data_with_factor = data.table::data.table(
		x1 = rnorm(50),
		x2 = factor(sample(c("A", "B", "C"), 50, replace = TRUE)),
		x3 = rnorm(50),
		y = rnorm(50)
	)
	task_factor = as_task_regr(data_with_factor, target = "y")

	expect_error(
		KnockoffGaussianSampler$new(task_factor),
		regexp = "following unsupported feature types"
	)
})

test_that("KnockoffSampler works with multiple iterations", {
	skip_if_not_installed("knockoff")

	task = sim_dgp_ewald(n = 100)

	withr::with_seed(42, {
		sampler_default = KnockoffSampler$new(task = task)
	})
	withr::with_seed(42, {
		sampler_1 = KnockoffSampler$new(task = task, iters = 1)
	})
	sampler_5 = KnockoffSampler$new(task = task, iters = 5)

	# x_tilde generated with same seed, same length
	expect_equal(
		sampler_default$sample("x1"),
		sampler_1$sample("x1")
	)

	x_orig = task$data(rows = 2)
	x_sampled = sampler_1$sample("x1", row_ids = 2)

	# Other values are the same (excluding sampled feature x1)
	non_sampled_cols = setdiff(names(x_sampled), "x1")
	expect_equal(x_orig[, ..non_sampled_cols], x_sampled[, ..non_sampled_cols])

	# Multiple samples
	x_orig = task$data(rows = c(1, 1, 1))
	x_sampled = sampler_5$sample("x1", row_ids = c(1, 1, 1))

	expect_equal(x_sampled$x2, x_orig$x2)

	# Sampling more than available
	expect_warning(
		sampled_1_repeats <- sampler_1$sample("x1", row_ids = c(1, 1, 1)),
		regexp = "sample with replacement"
	)

	expect_equal(
		sampled_1_repeats,
		data.table::rbindlist(replicate(3, sampler_1$sample("x1", row_ids = 1), simplify = FALSE))
	)
})

test_that("KnockoffGaussianSampler preserves feature types", {
	skip_if_not_installed("knockoff")
	test_sampler_feature_types(KnockoffGaussianSampler)
})

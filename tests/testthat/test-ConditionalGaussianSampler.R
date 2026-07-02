# Tests for ConditionalGaussianSampler

test_that("ConditionalGaussianSampler initialization works", {
  task = tgen("friedman1")$generate(n = 100)
  sampler = ConditionalGaussianSampler$new(task)

  expect_s3_class(sampler, "ConditionalGaussianSampler")
  expect_s3_class(sampler, "ConditionalSampler")
  expect_equal(sampler$label, "Gaussian Conditional Sampler")
  expect_s3_class(sampler$param_set, "ParamSet")

  # Check stored statistics
  checkmate::expect_numeric(sampler$mu, len = task$n_features, any.missing = FALSE)
  checkmate::expect_matrix(sampler$sigma, nrows = task$n_features, ncols = task$n_features)
  expect_true(isSymmetric(sampler$sigma))
})

test_that("ConditionalGaussianSampler rejects non-numeric tasks", {
  task = tsk("penguins")
  expect_error(ConditionalGaussianSampler$new(task), "unsupported feature types")
})

test_that("ConditionalGaussianSampler sampling works", {
  task = tgen("friedman1")$generate(n = 100)
  sampler = ConditionalGaussianSampler$new(task)

  # Marginal sampling
  expect_marginal_sampling(sampler, feature = "important1", row_ids = 1:20)

  # Conditional sampling with single conditioning feature
  expect_conditional_sampling(
    sampler,
    feature = "important2",
    conditioning_set = "important1",
    row_ids = 1:20
  )

  # Conditional sampling with multiple features
  expect_conditional_sampling(
    sampler,
    feature = c("important2", "important3"),
    conditioning_set = "important1",
    row_ids = 1:20
  )
})

test_that("ConditionalGaussianSampler sample_newdata works", {
  task = tgen("friedman1")$generate(n = 100)
  sampler = ConditionalGaussianSampler$new(task)
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

test_that("ConditionalGaussianSampler handles single observation", {
  task = tgen("friedman1")$generate(n = 100)
  sampler = ConditionalGaussianSampler$new(task)
  test_data = task$data(rows = 1)

  sampled = sampler$sample_newdata(
    feature = "important2",
    newdata = test_data,
    conditioning_set = "important1"
  )

  expect_sampler_output_structure(sampled, task, nrows = 1)
  expect_non_sampled_unchanged(sampled, test_data, "important1")
})

test_that("ConditionalGaussianSampler is reproducible with seed", {
  task = tgen("friedman1")$generate(n = 100)
  sampler = ConditionalGaussianSampler$new(task)
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

test_that("ConditionalGaussianSampler conditioning_set parameter behavior", {
  task = tgen("friedman1")$generate(n = 100)
  test_conditioning_set_behavior(ConditionalGaussianSampler, task)
})

test_that("ConditionalGaussianSampler preserves feature types", {
  test_sampler_feature_types(ConditionalGaussianSampler)
})

test_that("ConditionalGaussianSampler obeys draw-major order under samples_per_row > 1", {
  set.seed(123L)
  n = 20L
  dt = data.table::data.table(
    y = rnorm(n),
    x1 = rnorm(n),
    x2 = rnorm(n),
    tag = seq_len(n) + 0.5
  )
  task = mlr3::as_task_regr(dt, target = "y")
  sampler = ConditionalGaussianSampler$new(task, conditioning_set = c("x2", "tag"))

  expect_draw_major_row_order(
    sampler,
    task,
    feature = "x1",
    tag_column = "tag",
    samples_per_row = 4L
  )
})

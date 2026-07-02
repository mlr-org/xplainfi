# Tests for MarginalPermutationSampler

test_that("MarginalPermutationSampler initialization works", {
  task = tgen("circle", d = 5)$generate(n = 100)
  sampler = MarginalPermutationSampler$new(task)

  expect_s3_class(sampler, "MarginalPermutationSampler")
  expect_s3_class(sampler, "MarginalSampler")
  expect_equal(sampler$label, "Permutation sampler")
  expect_s3_class(sampler$param_set, "ParamSet")
})

test_that("MarginalPermutationSampler sampling works", {
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
  expect_non_sampled_unchanged(sampled_data, data, c("x2", "x3", "x4", "x5"))
})

test_that("MarginalPermutationSampler handles multiple features", {
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
  expect_non_sampled_unchanged(sampled_data, data, c("x4", "x5"))
})

test_that("MarginalPermutationSampler works with different task types", {
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

test_that("MarginalPermutationSampler obeys draw-major order under samples_per_row > 1", {
  set.seed(123L)
  n = 10L
  dt = data.table::data.table(
    y = rnorm(n),
    x = rnorm(n),
    tag = seq_len(n) + 0.5
  )
  task = mlr3::as_task_regr(dt, target = "y")
  sampler = MarginalPermutationSampler$new(task)

  expect_draw_major_row_order(
    sampler,
    task,
    feature = "x",
    tag_column = "tag",
    samples_per_row = 4L
  )
})

test_that("$sample() does not mutate the task data", {
  task = sim_dgp_independent(n = 50)
  before = task$data(rows = task$row_ids)[["important1"]]

  sampler = MarginalPermutationSampler$new(task)
  out = sampler$sample("important1", row_ids = task$row_ids)

  # task is unchanged after sampling (sampler must work on a disposable copy)
  expect_identical(task$data(rows = task$row_ids)[["important1"]], before)
  checkmate::expect_data_table(out, nrows = task$nrow)
})

test_that("$sample_newdata() does not mutate the caller's newdata", {
  task = sim_dgp_independent(n = 50)
  sampler = MarginalPermutationSampler$new(task)

  newdata = task$data()
  snapshot = data.table::copy(newdata)

  sampler$sample_newdata("important1", newdata = newdata)

  expect_identical(newdata, snapshot)
})

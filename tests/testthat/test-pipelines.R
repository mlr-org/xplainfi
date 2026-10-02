# =============================================================================
# Learners wrapped in a GraphLearner with preprocessing PipeOps
#
# po("encode") changes the feature set the model sees, while importance is
# always reported on the task's features.
# =============================================================================

encode_learner = function() {
  `%>>%` = mlr3pipelines::`%>>%`
  as_learner(mlr3pipelines::po("encode") %>>% lrn("regr.rpart"))
}

# Small task with one multi-level factor feature, which po("encode") expands.
task_with_factor = function(n = 150L) {
  set.seed(4092)
  grp = factor(sample(letters[1:4], n, replace = TRUE))
  x1 = rnorm(n)
  x2 = rnorm(n)
  y = 2 * x1 + as.integer(grp) + rnorm(n)
  as_task_regr(data.table::data.table(y, x1, x2, grp), target = "y", id = "factor_task")
}

test_that("PFI works with GraphLearner using po('encode')", {
  skip_if_not_installed("mlr3pipelines")

  task = task_with_factor()
  pfi = PFI$new(
    task = task,
    learner = encode_learner(),
    measure = msr("regr.mse"),
    n_repeats = 2L
  )
  pfi$compute()

  expect_importance_dt(pfi$importance(), task$feature_names)
  expect_gt(pfi$importance()[feature == "grp", importance], 0)
})

test_that("CFI works with GraphLearner using po('encode')", {
  skip_if_not_installed("mlr3pipelines")
  skip_if_not_installed("FNN")

  task = task_with_factor()
  cfi = CFI$new(
    task = task,
    learner = encode_learner(),
    measure = msr("regr.mse"),
    sampler = ConditionalKNNSampler$new(task),
    n_repeats = 2L
  )
  cfi$compute()

  expect_importance_dt(cfi$importance(), task$feature_names)
  expect_obs_loss_dt(cfi$obs_loss(), task$feature_names)
})

test_that("LOCO refits a GraphLearner using po('encode')", {
  skip_if_not_installed("mlr3pipelines")

  task = task_with_factor()
  loco = LOCO$new(
    task = task,
    learner = encode_learner(),
    measure = msr("regr.mse")
  )
  loco$compute()

  expect_importance_dt(loco$importance(), task$feature_names)
})

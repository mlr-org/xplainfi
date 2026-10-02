# =============================================================================
# PFI Tests using higher-level test helpers
# =============================================================================

# -----------------------------------------------------------------------------
# Basic functionality
# -----------------------------------------------------------------------------

test_that("PFI default behavior with minimal parameters", {
  test_default_behavior(PFI, task_type = "regr", n_repeats = 1L)
})

test_that("PFI basic workflow with classification", {
  task = tgen("2dnormals")$generate(n = 100)

  pfi = PFI$new(
    task = task,
    learner = lrn("classif.rpart", predict_type = "prob"),
    measure = msr("classif.ce"),
    n_repeats = 1L
  )
  checkmate::expect_r6(pfi, c("FeatureImportanceMethod", "PFI"))

  pfi$compute()
  expect_method_output(pfi)
})

test_that("PFI featureless learner produces zero importance", {
  test_featureless_zero_importance(PFI, task_type = "classif", n_repeats = 1L)
})

# -----------------------------------------------------------------------------
# Repeats and scores
# -----------------------------------------------------------------------------

test_that("PFI multiple repeats and scores structure", {
  task = tgen("friedman1")$generate(n = 200)

  test_n_repeats_and_scores(
    PFI,
    task = task,
    learner = lrn("regr.rpart"),
    measure = msr("regr.mse"),
    n_repeats = 2L
  )
})

test_that("PFI single feature", {
  task = tgen("friedman1")$generate(n = 200)

  test_single_feature(
    PFI,
    task = task,
    learner = lrn("regr.rpart"),
    measure = msr("regr.mse"),
    feature = "important4",
    n_repeats = 2L
  )
})

# -----------------------------------------------------------------------------
# Relation parameter
# -----------------------------------------------------------------------------

test_that("PFI difference vs ratio relations", {
  task = tgen("2dnormals")$generate(n = 100)

  test_relation_parameter(
    PFI,
    task = task,
    learner = lrn("classif.rpart", predict_type = "prob"),
    measure = msr("classif.ce"),
    n_repeats = 1L
  )
})

# -----------------------------------------------------------------------------
# Sensible results
# -----------------------------------------------------------------------------

test_that("PFI friedman1 produces sensible ranking", {
  test_friedman1_sensible_ranking(PFI, n_repeats = 5L)
})

# -----------------------------------------------------------------------------
# Grouped importance
# -----------------------------------------------------------------------------

test_that("PFI with feature groups", {
  task = tgen("friedman1")$generate(n = 200)

  groups = list(
    important_group = c("important1", "important2", "important3"),
    unimportant_group = c("unimportant1", "unimportant2")
  )

  test_grouped_importance(
    PFI,
    task = task,
    learner = lrn("regr.rpart"),
    measure = msr("regr.mse"),
    groups = groups,
    expected_classes = c("FeatureImportanceMethod", "PFI"),
    n_repeats = 1L
  )
})

# -----------------------------------------------------------------------------
# PFI-specific tests (not covered by generic helpers)
# -----------------------------------------------------------------------------

test_that("PFI scores and obs_losses agree", {
  task = tgen("friedman1")$generate(n = 200)

  pfi = PFI$new(
    task = task,
    learner = lrn("regr.rpart"),
    measure = msr("regr.mse"),
    resampling = rsmp("cv", folds = 3),
    n_repeats = 2
  )

  pfi$compute()

  importance_agg = pfi$importance()
  importance_scores = pfi$scores()[, .(iter_rsmp, iter_repeat, feature, importance)][
    order(iter_rsmp, iter_repeat, feature)
  ]
  importance_obs_loss = pfi$obs_loss()

  expect_equal(
    importance_agg,
    importance_scores[, list(importance = mean(importance)), by = "feature"],
    ignore_attr = TRUE
  )

  # Aggregate squared errors to get mse per iteration
  obs_agg = importance_obs_loss[,
    list(importance = mean(obs_importance)),
    by = c("iter_rsmp", "iter_repeat", "feature")
  ][order(iter_rsmp, iter_repeat, feature)]

  expect_equal(importance_scores, obs_agg, tolerance = sqrt(.Machine$double.eps))
})

test_that("PFI obs_loss uses specified measure, not default", {
  # Regression test for bug where pred$obs_loss() was called without
  # passing the measure, causing it to use the default measure (regr.mse)
  # instead of the user-specified one. With a non-default measure like
  # regr.mae, this caused obs_loss values to be NULL/wrong.
  task = mlr3::tsk("mtcars")

  pfi = PFI$new(
    task = task,
    learner = lrn("regr.rpart"),
    measure = msr("regr.mae"),
    n_repeats = 1L
  )

  pfi$compute()

  obs_losses = pfi$obs_loss()
  expect_obs_loss_dt(obs_losses, features = task$feature_names)

  # obs_loss values should be absolute errors (MAE), not squared errors (MSE).
  # Verify by checking that aggregating obs losses matches the score-level
  # importance (which correctly uses the specified measure).
  scores = pfi$scores()[, .(iter_rsmp, feature, importance)][order(iter_rsmp, feature)]

  obs_agg = obs_losses[,
    list(importance = mean(obs_importance)),
    by = c("iter_rsmp", "feature")
  ][order(iter_rsmp, feature)]

  expect_equal(scores, obs_agg, tolerance = sqrt(.Machine$double.eps))
})

# -----------------------------------------------------------------------------
# weight_fun
# -----------------------------------------------------------------------------

test_that("PFI with constant weight_fun reproduces unweighted scores and reports ess", {
  set.seed(1201)
  task = sim_dgp_independent(n = 200)
  learner = lrn("regr.rpart")
  resampling = rsmp("holdout")$instantiate(task)
  learner$train(task, row_ids = resampling$train_set(1))

  pfi = PFI$new(task, learner, msr("regr.mse"), resampling = resampling, n_repeats = 2L)
  pfi_w = PFI$new(
    task,
    learner,
    msr("regr.mse"),
    resampling = resampling,
    n_repeats = 2L,
    weight_fun = function(data, feature) rep(2, nrow(data))
  )
  set.seed(5)
  pfi$compute()
  set.seed(5)
  pfi_w$compute()

  expect_equal(pfi_w$scores()$importance, pfi$scores()$importance)
  n_test = length(resampling$test_set(1))
  expect_equal(pfi_w$scores()$ess, rep(n_test, nrow(pfi_w$scores())))
  expect_null(pfi$scores()$ess)
  checkmate::expect_numeric(pfi_w$obs_loss()$weight, lower = 1, upper = 1, any.missing = FALSE)
  expect_null(pfi$obs_loss()$weight)
})

test_that("PFI weight_fun validation", {
  task = sim_dgp_independent(n = 100)
  expect_error(
    PFI$new(task, lrn("regr.rpart"), msr("regr.mse"), weight_fun = function(x) 1),
    "weight_fun"
  )
  expect_error(
    PFI$new(task, lrn("regr.rpart"), msr("regr.maxae"), weight_fun = function(data, feature) 1),
    "weights"
  )
  pfi = PFI$new(
    task,
    lrn("regr.rpart"),
    msr("regr.mse"),
    n_repeats = 1L,
    weight_fun = function(data, feature) rep(-1, nrow(data))
  )
  expect_error(pfi$compute(), ">= 0")
})

test_that("PFI reweight re-scores stored predictions", {
  set.seed(2603)
  task = sim_dgp_independent(n = 200)
  learner = lrn("regr.rpart")
  resampling = rsmp("holdout")$instantiate(task)
  learner$train(task, row_ids = resampling$train_set(1))
  n_test = length(resampling$test_set(1))

  pfi = PFI$new(task, learner, msr("regr.mse"), resampling = resampling, n_repeats = 2L)
  expect_error(pfi$reweight(NULL), "compute")
  pfi$compute()
  scores = pfi$scores()
  obs = pfi$obs_loss()

  expect_equal(pfi$reweight(NULL)$scores(), scores)
  pfi$reweight(function(data, feature) rep(2, nrow(data)))
  expect_equal(pfi$scores()$importance, scores$importance)
  expect_equal(pfi$scores()$ess, rep(n_test, nrow(scores)))
  checkmate::expect_numeric(pfi$obs_loss()$weight, lower = 1, upper = 1, len = nrow(obs))
  expect_equal(pfi$reweight(NULL)$scores(), scores)
  expect_equal(pfi$obs_loss(), obs)

  expect_error(pfi$reweight(function(x) 1), "weight_fun")
  pfi_maxae = PFI$new(task, learner, msr("regr.maxae"), resampling = resampling, n_repeats = 1L)
  pfi_maxae$compute()
  expect_error(pfi_maxae$reweight(function(data, feature) rep(1, nrow(data))), "weights")
})

test_that("PFI reweight with ARF weights equals CFI weighting estimator", {
  skip_if_not_installed("arf")
  set.seed(7031)
  task = sim_dgp_correlated(n = 300, r = 0.9)
  learner = lrn("regr.rpart")
  resampling = rsmp("holdout")$instantiate(task)
  learner$train(task, row_ids = resampling$train_set(1))
  sampler = ConditionalARFSampler$new(task, num_trees = 10L, verbose = FALSE)

  pfi = PFI$new(task, learner, msr("regr.mse"), resampling = resampling, n_repeats = 3L)
  cfi = CFI$new(
    task,
    learner,
    msr("regr.mse"),
    resampling = resampling,
    sampler = sampler,
    n_repeats = 3L,
    estimator = "weighting"
  )
  set.seed(11)
  pfi$compute()
  set.seed(11)
  cfi$compute()

  expect_equal(pfi$reweight(weights_arf(sampler))$scores(), cfi$scores())
  checkmate::expect_numeric(pfi$reweight(weights_arf(sampler, "x2"))$scores()$ess, lower = 1, upper = 100)
})

# -----------------------------------------------------------------------------
# normalize
# -----------------------------------------------------------------------------

test_that("PFI per_observation normalization with constant weights reproduces unweighted scores", {
  set.seed(4821)
  task = sim_dgp_independent(n = 200)
  learner = lrn("regr.rpart")
  resampling = rsmp("holdout")$instantiate(task)
  learner$train(task, row_ids = resampling$train_set(1))

  pfi = PFI$new(task, learner, msr("regr.mse"), resampling = resampling, n_repeats = 3L)
  pfi$compute()
  scores = pfi$scores()

  pfi$reweight(function(data, feature) rep(0.5, nrow(data)), normalize = "per_observation")
  expect_equal(pfi$param_set$values$normalize, "per_observation")
  expect_equal(pfi$scores()$importance, scores$importance)
  expect_equal(pfi$scores()$ess, rep(3, nrow(scores)))
  checkmate::expect_numeric(pfi$obs_loss()$weight, lower = 1, upper = 1, any.missing = FALSE)
})

test_that("PFI scores and obs_losses agree under both normalizations with ARF weights", {
  skip_if_not_installed("arf")
  set.seed(1503)
  task = sim_dgp_correlated(n = 300, r = 0.9)
  sampler = ConditionalARFSampler$new(task, num_trees = 10L, verbose = FALSE)
  pfi = PFI$new(task, lrn("regr.rpart"), msr("regr.mse"), n_repeats = 5L, weight_fun = weights_arf(sampler))
  pfi$compute()

  for (mode in c("global", "per_observation")) {
    pfi$reweight(weights_arf(sampler), normalize = mode)
    scores = pfi$scores()[, .(iter_rsmp, iter_repeat, feature, importance)][order(iter_rsmp, iter_repeat, feature)]
    obs_agg = pfi$obs_loss()[,
      list(importance = mean(obs_importance)),
      by = c("iter_rsmp", "iter_repeat", "feature")
    ][order(iter_rsmp, iter_repeat, feature)]
    expect_equal(scores, obs_agg, tolerance = sqrt(.Machine$double.eps))
  }
  checkmate::expect_numeric(pfi$scores()$ess, lower = 1, upper = 5, any.missing = FALSE)
})

test_that("PFI per_observation and global normalization agree on correlated DGP", {
  skip_if_not_installed("arf")
  set.seed(9107)
  task = sim_dgp_correlated(n = 500, r = 0.9)
  sampler = ConditionalARFSampler$new(task, num_trees = 20L, verbose = FALSE)
  pfi = PFI$new(task, lrn("regr.rpart"), msr("regr.mse"), n_repeats = 10L, features = "x1")
  pfi$compute()
  imp_global = pfi$reweight(weights_arf(sampler))$importance()$importance
  imp_per_obs = pfi$reweight(weights_arf(sampler), normalize = "per_observation")$importance()$importance
  expect_equal(imp_per_obs, imp_global, tolerance = 0.3)
})

test_that("PFI per_observation normalization requires a mean-decomposable measure", {
  task = sim_dgp_independent(n = 100)
  w = function(data, feature) rep(1, nrow(data))
  pfi = PFI$new(
    task,
    lrn("regr.rpart"),
    msr("regr.rmse"),
    n_repeats = 1L,
    weight_fun = w,
    normalize = "per_observation"
  )
  expect_error(pfi$compute(), "regr.rmse")
  expect_error(
    PFI$new(task, lrn("regr.rpart"), msr("regr.maxae"), weight_fun = w, normalize = "per_observation"),
    "weights"
  )
  expect_error(PFI$new(task, lrn("regr.rpart"), msr("regr.mse"), normalize = "nope"), "normalize")
})

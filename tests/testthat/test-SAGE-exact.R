# =============================================================================
# Exact SAGE estimator tests (estimator = "exact")
# =============================================================================
#
# The exact estimator enumerates all 2^p coalitions and computes the Shapley
# decomposition in closed form, so it has no coalition-sampling error. It is
# validated against an independent brute-force Shapley computation and used as a
# ground-truth reference that the sampling estimators must converge to.

# -----------------------------------------------------------------------------
# Basic functionality
# -----------------------------------------------------------------------------

test_that("MarginalSAGE exact estimator works for regression and classification", {
  set.seed(3163)
  task_regr = sim_dgp_independent(n = 150)
  sage_regr = MarginalSAGE$new(
    task = task_regr,
    learner = lrn("regr.rpart"),
    estimator = "exact",
    n_samples = 20L
  )
  expect_identical(sage_regr$param_set$values$estimator, "exact")
  expect_null(sage_regr$param_set$values$n_permutations)
  expect_null(sage_regr$param_set$values$n_coalitions)
  sage_regr$compute()
  expect_importance_dt(sage_regr$importance(), features = sage_regr$features)
  # No coalition-sampling error, so recomputation is deterministic.
  imp1 = sage_regr$importance()$importance
  sage_regr$compute()
  expect_equal(sage_regr$importance()$importance, imp1, tolerance = 1e-12)

  task_binary = tgen("2dnormals")$generate(n = 100)
  sage_binary = MarginalSAGE$new(
    task = task_binary,
    learner = lrn("classif.rpart", predict_type = "prob"),
    estimator = "exact",
    n_samples = 20L
  )
  sage_binary$compute()
  expect_importance_dt(sage_binary$importance(), features = sage_binary$features)

  task_multi = tgen("cassini")$generate(n = 100)
  sage_multi = MarginalSAGE$new(
    task = task_multi,
    learner = lrn("classif.rpart", predict_type = "prob"),
    estimator = "exact",
    n_samples = 20L
  )
  sage_multi$compute()
  expect_importance_dt(sage_multi$importance(), features = sage_multi$features)
  expect_length(task_multi$class_names, 3L)
})

test_that("ConditionalSAGE exact estimator works with Gaussian sampler", {
  set.seed(5417)
  task = sim_dgp_correlated(n = 120)
  sage = ConditionalSAGE$new(
    task = task,
    learner = lrn("regr.rpart"),
    sampler = ConditionalGaussianSampler$new(task),
    estimator = "exact",
    n_samples = 20L
  )
  expect_identical(sage$param_set$values$estimator, "exact")
  sage$compute()
  expect_importance_dt(sage$importance(), features = sage$features)
})

test_that("MarginalSAGE exact with a single feature returns the total", {
  task = tgen("friedman1")$generate(n = 120)
  sage = MarginalSAGE$new(
    task = task,
    learner = lrn("regr.rpart"),
    features = "important4",
    estimator = "exact",
    n_samples = 20L
  )
  sage$compute()
  imp = sage$importance()
  expect_importance_dt(imp, features = "important4")
  expect_equal(nrow(imp), 1L)
})

# -----------------------------------------------------------------------------
# Correctness: matches independent brute-force Shapley
# -----------------------------------------------------------------------------

test_that("exact estimator matches an independent brute-force Shapley computation", {
  set.seed(202)
  task = tgen("friedman1")$generate(n = 200)
  task$select(c("important1", "important2", "important4", "unimportant1"))
  learner = lrn("regr.rpart")

  sage = MarginalSAGE$new(
    task = task,
    learner = learner,
    measure = msr("regr.mse"),
    estimator = "exact",
    n_samples = 30L
  )
  sage$compute()
  exact_est = sage$importance()

  # Independent reference: shared brute-force enumeration (helper-importance.R).
  phi = brute_force_shapley(sage, task)
  feats = sage$features

  est = exact_est[match(feats, feature), importance]
  # Both compute the exact Shapley values of the same value function.
  expect_equal(est, unname(phi), tolerance = 1e-8)
})

test_that("sampling estimators converge to the exact estimator", {
  set.seed(808)
  task = tgen("friedman1")$generate(n = 200)
  task$select(c("important1", "important2", "important4", "unimportant1"))
  learner = lrn("regr.rpart")
  measure = msr("regr.mse")
  resampling = rsmp("holdout")
  resampling$instantiate(task)

  # Share the reference subsample so all estimators target the same value function.
  set.seed(1)
  exact = MarginalSAGE$new(task, learner, measure, resampling = resampling, estimator = "exact", n_samples = 40L)
  exact$compute()

  set.seed(1)
  kernel = MarginalSAGE$new(
    task,
    learner,
    measure,
    resampling = resampling,
    estimator = "kernel",
    n_coalitions = 2000L,
    n_samples = 40L
  )
  kernel$compute()

  cmp = merge(exact$importance(), kernel$importance(), by = "feature")
  # Totals match exactly (efficiency); values match up to Monte Carlo error.
  expect_equal(sum(cmp$importance.x), sum(cmp$importance.y), tolerance = 1e-8)
  expect_lt(max(abs(cmp$importance.x - cmp$importance.y)), 0.05)
})

# -----------------------------------------------------------------------------
# Guards and argument misuse
# -----------------------------------------------------------------------------

test_that("exact estimator aborts above the max_features cap", {
  task = tgen("friedman1")$generate(n = 60) # 10 features
  learner = lrn("regr.rpart")

  expect_error(
    MarginalSAGE$new(task, learner, estimator = "exact", max_features = 6L),
    "max_features"
  )
  # Default cap (12) accommodates the 10-feature task.
  expect_no_error(MarginalSAGE$new(task, learner, estimator = "exact"))
  # Explicitly raising the cap also works.
  expect_no_error(MarginalSAGE$new(task, learner, estimator = "exact", max_features = 10L))
})

test_that("exact estimator rejects sampling budgets", {
  task = sim_dgp_independent(n = 60)
  learner = lrn("regr.rpart")

  expect_error(
    MarginalSAGE$new(task, learner, estimator = "exact", n_permutations = 5L),
    "no sampling budget"
  )
  expect_error(
    MarginalSAGE$new(task, learner, estimator = "exact", n_coalitions = 50L),
    "no sampling budget"
  )
})

test_that("plot_convergence errors informatively for the exact estimator", {
  task = sim_dgp_independent(n = 80)
  sage = MarginalSAGE$new(task, lrn("regr.rpart"), estimator = "exact", n_samples = 20L)
  sage$compute()
  expect_null(sage$convergence_history)
  expect_error(sage$plot_convergence(), "not applicable to the exact estimator")
})

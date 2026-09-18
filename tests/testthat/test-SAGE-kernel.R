# Kernel SAGE estimator (estimator = "kernel"): unbiased KernelSHAP (Covert & Lee 2021,
# Eq. 9) on the stochastic SAGE game, i.e. one test observation per coalition draw, as in
# the reference Python `sage` package.

test_that("sage_kernel_A matches the enumerated E[z z^T] under the size distribution", {
  for (m in 2:6) {
    k = seq_len(m - 1L)
    p = sage_kernel_size_probs(m)
    expect_equal(sum(p), 1)
    # enumerate all coalitions of each size, weight uniformly within size
    A = matrix(0, m, m)
    for (i in k) {
      subsets = combn(m, i, simplify = FALSE)
      for (S in subsets) {
        z = as.numeric(seq_len(m) %in% S)
        A = A + p[i] / length(subsets) * tcrossprod(z)
      }
    }
    expect_equal(sage_kernel_A(m), A, tolerance = 1e-12)
    expect_equal(diag(sage_kernel_A(m)), rep(0.5, m))
  }
})

test_that("sage_kernel_solve_constrained recovers an additive game exactly", {
  m = 5L
  beta = c(2, -1, 0.5, 0, 3)
  A = sage_kernel_A(m)
  # for v(z) = z^T beta, b = E[z z^T] beta = A beta and the constraint holds with equality
  phi = sage_kernel_solve_constrained(solve(A), as.numeric(A %*% beta), sum(beta))
  expect_equal(phi, beta, tolerance = 1e-12)
})

test_that("MarginalSAGE kernel estimator works for regression and classification", {
  set.seed(4711)
  task = sim_dgp_independent(n = 150)
  sage = MarginalSAGE$new(task, lrn("regr.rpart"), estimator = "kernel", n_coalitions = 300L, n_samples = 20L)
  expect_identical(sage$param_set$values$estimator, "kernel")
  expect_equal(sage$param_set$values$n_coalitions, 300L)
  expect_null(sage$param_set$values$n_permutations)
  sage$compute()
  expect_importance_dt(sage$importance(), features = sage$features)

  budget = sage$budget
  expect_equal(budget$unit, "coalition draws")
  expect_equal(budget$used, 300)
  expect_equal(budget$n_evals, 2 + 2 * 300)
  n_test = length(sage$resample_result$resampling$test_set(1))
  expect_equal(budget$n_rows, (2 * n_test + 2 * 300) * 20)
  expect_false(budget$converged)
  # chunks of 512 draws: one checkpoint here
  expect_equal(nrow(sage$convergence_history), length(sage$features))
  checkmate::expect_numeric(sage$convergence_history$se, lower = 0, finite = TRUE)
  conv = sage$convergence()
  checkmate::expect_data_table(conv, nrows = length(sage$features))
  expect_setequal(colnames(conv), c("feature", "importance", "se", "ratio"))
  expect_equal(conv$importance, sage$importance()[match(conv$feature, feature), importance])
  expect_equal(unique(conv$ratio), max(conv$se) / diff(range(conv$importance)))
  expect_setequal(colnames(sage$convergence_history), c("budget", "n_evals", "n_rows", "feature", "importance", "se"))

  task_binary = tgen("2dnormals")$generate(n = 100)
  sage_binary = MarginalSAGE$new(
    task_binary,
    lrn("classif.rpart", predict_type = "prob"),
    msr("classif.logloss"),
    estimator = "kernel",
    n_coalitions = 100L,
    n_samples = 20L
  )
  sage_binary$compute()
  expect_importance_dt(sage_binary$importance(), features = sage_binary$features)

  task_multi = tgen("cassini")$generate(n = 100)
  sage_multi = MarginalSAGE$new(
    task_multi,
    lrn("classif.rpart", predict_type = "prob"),
    msr("classif.logloss"),
    estimator = "kernel",
    n_coalitions = 100L,
    n_samples = 20L
  )
  sage_multi$compute()
  expect_importance_dt(sage_multi$importance(), features = sage_multi$features)
})

test_that("MarginalSAGE kernel with a single feature returns the total", {
  task = tgen("friedman1")$generate(n = 120)
  sage = MarginalSAGE$new(task, lrn("regr.rpart"), features = "important4", estimator = "kernel", n_samples = 20L)
  sage$compute()
  expect_importance_dt(sage$importance(), features = "important4")
  expect_equal(sage$budget$used, 0)
})

test_that("kernel estimator converges to the exact estimator", {
  set.seed(808)
  task = tgen("friedman1")$generate(n = 200)
  task$select(c("important1", "important2", "important4", "unimportant1"))
  learner = lrn("regr.rpart")
  resampling = rsmp("holdout")$instantiate(task)

  # Share the reference subsample so both estimators target the same value function.
  set.seed(1097)
  exact = MarginalSAGE$new(task, learner, resampling = resampling, estimator = "exact", n_samples = 20L)
  exact$compute()
  set.seed(1097)
  kernel = MarginalSAGE$new(
    task,
    learner,
    resampling = resampling,
    estimator = "kernel",
    n_coalitions = 8192L,
    n_samples = 20L
  )
  kernel$compute()

  cmp = merge(exact$importance(), kernel$importance(), by = "feature")
  # Totals match exactly (efficiency). Values match up to the Monte Carlo error of the
  # per-observation game, which for squared error on a tree is still 5% to 20% of the
  # spread at this budget (it shrinks with 1 / sqrt(n_coalitions); verified to ~1% at 65k).
  expect_equal(sum(cmp$importance.x), sum(cmp$importance.y), tolerance = 1e-8)
  spread = diff(range(cmp$importance.x))
  expect_lt(max(abs(cmp$importance.x - cmp$importance.y)), 0.3 * spread)

  # Log loss per observation is far less variable, so the agreement is tight.
  task_c = tgen("2dnormals")$generate(n = 200)
  learner_c = lrn("classif.rpart", predict_type = "prob")
  resampling_c = rsmp("holdout")$instantiate(task_c)
  set.seed(3)
  exact_c = MarginalSAGE$new(
    task_c,
    learner_c,
    msr("classif.logloss"),
    resampling_c,
    estimator = "exact",
    n_samples = 20L
  )
  exact_c$compute()
  set.seed(3)
  kernel_c = MarginalSAGE$new(
    task_c,
    learner_c,
    msr("classif.logloss"),
    resampling_c,
    estimator = "kernel",
    n_coalitions = 2048L,
    n_samples = 20L
  )
  kernel_c$compute()
  cmp_c = merge(exact_c$importance(), kernel_c$importance(), by = "feature")
  expect_lt(max(abs(cmp_c$importance.x - cmp_c$importance.y)), 0.05 * diff(range(cmp_c$importance.x)))
})

test_that("kernel estimator plots convergence and reuses its budget across resampling iterations", {
  set.seed(99)
  task = sim_dgp_independent(n = 120)
  sage = MarginalSAGE$new(
    task,
    lrn("regr.rpart"),
    resampling = rsmp("cv", folds = 2),
    estimator = "kernel",
    n_coalitions = 600L,
    n_samples = 10L
  )
  sage$compute()
  expect_equal(nrow(sage$scores()), 2 * length(sage$features))
  expect_equal(sage$convergence_history[, unique(budget)], c(512, 600))
  skip_if_not_installed("ggplot2")
  expect_s3_class(sage$plot_convergence(), "ggplot")
})

test_that("kernel standard errors are calibrated against replicate runs", {
  # Fixed model, test set, and reference subsample; only the coalition draws and the
  # test observations paired with them vary, which is exactly what the SE claims to cover.
  set.seed(808)
  task = tgen("friedman1")$generate(n = 200)
  task$select(c("important1", "important2", "important4", "unimportant1"))
  resampling = rsmp("holdout")$instantiate(task)
  set.seed(11)
  sage = MarginalSAGE$new(
    task,
    lrn("regr.rpart"),
    resampling = resampling,
    estimator = "kernel",
    n_coalitions = 1024L,
    n_samples = 20L
  )
  runs = rbindlist(lapply(1:20, function(k) {
    set.seed(1000 + k)
    sage$compute()
    sage$convergence()
  }))
  calib = runs[, list(ratio = mean(se) / sd(importance)), by = "feature"]
  checkmate::expect_numeric(calib$ratio, lower = 0.6, upper = 1.5)
})

test_that("kernel early stopping stops on the criterion and warns when the budget is exhausted", {
  set.seed(2718)
  task = sim_dgp_independent(n = 120)
  sage = MarginalSAGE$new(task, lrn("regr.rpart"), estimator = "kernel", n_coalitions = 4096L, n_samples = 10L)

  sage$compute(early_stopping = TRUE, se_threshold = 0.5) # loose: first checkpoint suffices
  expect_true(sage$converged)
  expect_equal(sage$budget$used, 512)
  expect_lt(sage$convergence()$ratio[1], 0.5)

  expect_warning(sage$compute(early_stopping = TRUE, se_threshold = 1e-4), "did not converge")
  expect_false(sage$converged)
  expect_equal(sage$budget$used, 4096)

  # remaining resampling iterations reuse the budget the first one stopped at
  sage_cv = MarginalSAGE$new(
    task,
    lrn("regr.rpart"),
    resampling = rsmp("cv", folds = 2),
    estimator = "kernel",
    n_coalitions = 4096L,
    n_samples = 10L,
    early_stopping = TRUE,
    se_threshold = 0.5
  )
  sage_cv$compute()
  expect_true(sage_cv$converged)
  expect_equal(sage_cv$budget$used, 512)
  expect_equal(nrow(sage_cv$scores()), 2 * length(sage_cv$features))
})

test_that("kernel estimator guards its arguments", {
  task = sim_dgp_independent(n = 60)
  learner = lrn("regr.rpart")

  expect_error(MarginalSAGE$new(task, learner, estimator = "kernel", n_permutations = 5L), "n_coalitions")
  expect_error(MarginalSAGE$new(task, learner, n_coalitions = 50L), "n_permutations")
  expect_error(MarginalSAGE$new(task, learner, estimator = "exact", n_coalitions = 50L), "no sampling budget")
  expect_error(MarginalSAGE$new(task, learner, msr("regr.rsq"), estimator = "kernel"), "observation-wise loss")
  expect_warning(MarginalSAGE$new(task, learner, estimator = "kernel", min_permutations = 5L), "ignored")
  expect_no_warning(MarginalSAGE$new(task, learner, estimator = "kernel", early_stopping = TRUE, se_threshold = 0.1))
  expect_error(ConditionalSAGE$new(task, learner, estimator = "kernel"), "should be one of")

  sage = MarginalSAGE$new(task, learner, estimator = "kernel", n_coalitions = 20L, n_samples = 10L)
  expect_warning(sage$compute(check_interval = 2L), "ignored")
  expect_error(suppressWarnings(sage$n_permutations <- 5L), "only valid")
})

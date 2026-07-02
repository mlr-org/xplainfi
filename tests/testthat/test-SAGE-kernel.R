# =============================================================================
# Kernel SAGE estimator tests (estimator = "kernel")
# =============================================================================
#
# The kernel estimator is orthogonal to the marginal/conditional imputer axis,
# so it is exercised through both MarginalSAGE and ConditionalSAGE. Correctness
# is anchored on the regression estimator recovering exact Shapley values of the
# empirical value function (small feature sets) and agreeing with the
# permutation estimator (larger, Monte Carlo tolerance).

# -----------------------------------------------------------------------------
# Basic functionality
# -----------------------------------------------------------------------------

test_that("MarginalSAGE kernel estimator works across task types", {
  # Regression
  task_regr = sim_dgp_independent(n = 150)
  sage_regr = MarginalSAGE$new(
    task = task_regr,
    learner = lrn("regr.rpart"),
    estimator = "kernel",
    n_coalitions = 100L,
    n_samples = 20L
  )
  checkmate::expect_r6(sage_regr, c("FeatureImportanceMethod", "SAGE", "MarginalSAGE"))
  expect_identical(sage_regr$estimator, "kernel")
  expect_null(sage_regr$n_permutations)
  expect_identical(sage_regr$n_coalitions, 100L)
  sage_regr$compute()
  expect_importance_dt(sage_regr$importance(), features = sage_regr$features)

  # Binary classification
  task_binary = tgen("2dnormals")$generate(n = 100)
  sage_binary = MarginalSAGE$new(
    task = task_binary,
    learner = lrn("classif.rpart", predict_type = "prob"),
    estimator = "kernel",
    n_coalitions = 80L,
    n_samples = 20L
  )
  sage_binary$compute()
  expect_importance_dt(sage_binary$importance(), features = sage_binary$features)

  # Multiclass classification
  task_multi = tgen("cassini")$generate(n = 100)
  sage_multi = MarginalSAGE$new(
    task = task_multi,
    learner = lrn("classif.rpart", predict_type = "prob"),
    estimator = "kernel",
    n_coalitions = 80L,
    n_samples = 20L
  )
  sage_multi$compute()
  expect_importance_dt(sage_multi$importance(), features = sage_multi$features)
  expect_length(task_multi$class_names, 3L)
})

test_that("ConditionalSAGE kernel estimator works with Gaussian sampler", {
  task = sim_dgp_correlated(n = 120)
  sage = ConditionalSAGE$new(
    task = task,
    learner = lrn("regr.rpart"),
    sampler = ConditionalGaussianSampler$new(task),
    estimator = "kernel",
    n_coalitions = 100L,
    n_samples = 20L
  )
  checkmate::expect_r6(sage, c("FeatureImportanceMethod", "SAGE", "ConditionalSAGE"))
  expect_identical(sage$estimator, "kernel")
  sage$compute()
  expect_importance_dt(sage$importance(), features = sage$features)
})

test_that("MarginalSAGE kernel featureless learner produces zero importance", {
  test_featureless_zero_importance(
    MarginalSAGE,
    task_type = "regr",
    estimator = "kernel",
    n_coalitions = 64L,
    n_samples = 20L
  )
})

test_that("MarginalSAGE kernel with a single feature returns the total", {
  task = tgen("friedman1")$generate(n = 120)
  sage = MarginalSAGE$new(
    task = task,
    learner = lrn("regr.rpart"),
    features = "important4",
    estimator = "kernel",
    n_coalitions = 32L,
    n_samples = 20L
  )
  sage$compute()
  imp = sage$importance()
  expect_importance_dt(imp, features = "important4")
  expect_equal(nrow(imp), 1L)
  checkmate::expect_number(imp$importance, finite = TRUE)
})

# -----------------------------------------------------------------------------
# Correctness: recovers exact Shapley of the empirical value function
# -----------------------------------------------------------------------------

test_that("kernel estimator recovers exact Shapley on a small feature set", {
  # With few features the value function can be fully enumerated and exact
  # Shapley values computed by brute force. The regression estimator must
  # converge to these (up to Monte Carlo error) and satisfy the efficiency /
  # sum-to-total constraint exactly.
  set.seed(0xC0FFEE)
  # Three features keep the 2^m enumeration cheap and let the estimator converge
  # tightly at a modest budget. n_samples only shapes the (self-consistent) value
  # function, so it does not affect the kernel-vs-exact gap.
  task = tgen("friedman1")$generate(n = 150)
  task$select(c("important1", "important2", "important4"))
  learner = lrn("regr.rpart")

  sage = MarginalSAGE$new(
    task = task,
    learner = learner,
    measure = msr("regr.mse"),
    estimator = "kernel",
    n_coalitions = 1000L,
    n_samples = 20L
  )
  sage$compute()
  kern = sage$importance()

  # Reconstruct exact Shapley from the SAME value function the estimator uses.
  feats = sage$features
  m = length(feats)
  priv = sage$.__enclos_env__$private
  rr = sage$resample_result
  learner_fitted = rr$learners[[1]]
  test_dt = task$data(rows = rr$resampling$test_set(1))

  subsets = unlist(lapply(0:m, function(k) combn(m, k, simplify = FALSE)), recursive = FALSE)
  coals = lapply(subsets, function(idx) feats[idx])
  losses = priv$.evaluate_coalitions_batch(learner_fitted, test_dt, coals, NULL)

  key = function(idx) if (length(idx) == 0L) "E" else paste(sort(idx), collapse = ",")
  vmap = new.env()
  for (i in seq_along(subsets)) {
    assign(key(subsets[[i]]), losses[i], envir = vmap)
  }
  baseline = get("E", envir = vmap)
  vfun = function(idx) baseline - get(key(idx), envir = vmap)

  # Exact Shapley by averaging marginal contributions over all m! orderings.
  gen_perms = function(x) {
    if (length(x) == 1L) {
      return(list(x))
    }
    out = list()
    for (i in seq_along(x)) {
      for (p in gen_perms(x[-i])) {
        out[[length(out) + 1L]] = c(x[i], p)
      }
    }
    out
  }
  perms = gen_perms(seq_len(m))
  phi = numeric(m)
  for (p in perms) {
    prev = vfun(integer(0))
    S = integer(0)
    for (j in p) {
      S = c(S, j)
      cur = vfun(S)
      phi[j] = phi[j] + (cur - prev)
      prev = cur
    }
  }
  phi = phi / length(perms)
  names(phi) = feats

  kern_ordered = kern[match(feats, feature), importance]
  # Efficiency constraint holds exactly (same total, same closed-form solve).
  expect_equal(sum(kern_ordered), sum(phi), tolerance = 1e-8)
  # Estimated values converge to exact Shapley within Monte Carlo tolerance.
  expect_lt(max(abs(kern_ordered - phi)), 0.15)
})

test_that("kernel and permutation estimators agree on important features", {
  set.seed(1234)
  task = tgen("friedman1")$generate(n = 250)
  learner = lrn("regr.rpart")
  measure = msr("regr.mse")

  # For the two estimators to solve the *exact* same game they must share both the
  # holdout split and the marginal reference subsample. The split is fixed by an
  # instantiated resampling; the reference subsample is drawn with RNG inside
  # `$new`, so seed identically before each constructor.
  resampling = rsmp("holdout")
  resampling$instantiate(task)

  set.seed(1234)
  sage_perm = MarginalSAGE$new(
    task,
    learner,
    measure,
    resampling = resampling,
    n_permutations = 30L,
    n_samples = 30L
  )
  sage_perm$compute()

  set.seed(1234)
  sage_kern = MarginalSAGE$new(
    task,
    learner,
    measure,
    resampling = resampling,
    estimator = "kernel",
    n_coalitions = 600L,
    n_samples = 30L
  )
  sage_kern$compute()

  perm = sage_perm$importance()[order(feature)]
  kern = sage_kern$importance()[order(feature)]

  # Both estimators solve the same game, so the efficiency total matches exactly.
  expect_equal(sum(perm$importance), sum(kern$importance), tolerance = 1e-8)
  # Rankings agree strongly across the ten features.
  expect_gt(cor(perm$importance, kern$importance), 0.9)
})

# -----------------------------------------------------------------------------
# Reproducibility
# -----------------------------------------------------------------------------

test_that("MarginalSAGE kernel is reproducible with the same seed", {
  task = tgen("2dnormals")$generate(n = 100)
  learner = lrn("classif.rpart", predict_type = "prob")
  measure = msr("classif.ce")

  set.seed(99)
  sage1 = MarginalSAGE$new(task, learner, measure, estimator = "kernel", n_coalitions = 128L)
  sage1$compute()

  set.seed(99)
  sage2 = MarginalSAGE$new(task, learner, measure, estimator = "kernel", n_coalitions = 128L)
  sage2$compute()

  expect_equal(sage1$importance()$importance, sage2$importance()$importance, tolerance = 1e-10)
})

# -----------------------------------------------------------------------------
# Cross-estimator argument misuse
# -----------------------------------------------------------------------------

test_that("kernel estimator rejects n_permutations and permutation rejects n_coalitions", {
  task = sim_dgp_independent(n = 60)
  learner = lrn("regr.rpart")

  # n_permutations is permutation-only
  expect_error(
    MarginalSAGE$new(task, learner, estimator = "kernel", n_permutations = 10L),
    "n_permutations"
  )
  expect_error(
    ConditionalSAGE$new(
      task,
      learner,
      sampler = ConditionalGaussianSampler$new(task),
      estimator = "kernel",
      n_permutations = 10L
    ),
    "n_permutations"
  )

  # n_coalitions is kernel-only
  expect_error(
    MarginalSAGE$new(task, learner, estimator = "permutation", n_coalitions = 100L),
    "n_coalitions"
  )
  expect_error(
    MarginalSAGE$new(task, learner, n_coalitions = 100L),
    "n_coalitions"
  )
})

test_that("kernel estimator rejects invalid n_coalitions and estimator", {
  task = sim_dgp_independent(n = 60)
  learner = lrn("regr.rpart")

  expect_error(MarginalSAGE$new(task, learner, estimator = "kernel", n_coalitions = 0L))
  expect_error(MarginalSAGE$new(task, learner, estimator = "kernel", n_coalitions = -5L))
  expect_error(MarginalSAGE$new(task, learner, estimator = "bogus"))
})

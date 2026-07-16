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
  set.seed(1723)
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
  expect_identical(sage_regr$param_set$values$estimator, "kernel")
  expect_null(sage_regr$param_set$values$n_permutations)
  expect_identical(sage_regr$param_set$values$n_coalitions, 100L)
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
  set.seed(3881)
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
  expect_identical(sage$param_set$values$estimator, "kernel")
  sage$compute()
  expect_importance_dt(sage$importance(), features = sage$features)
})

test_that("MarginalSAGE kernel featureless learner produces zero importance", {
  set.seed(947)
  test_featureless_zero_importance(
    MarginalSAGE,
    task_type = "regr",
    estimator = "kernel",
    n_coalitions = 64L,
    n_samples = 20L
  )
})

test_that("MarginalSAGE kernel with a single feature returns the total", {
  set.seed(4271)
  task = tgen("friedman1")$generate(n = 120)
  resampling = rsmp("holdout")$instantiate(task)

  # The m = 1 branch short-circuits to phi = v(full); with a shared split and
  # reference subsample it must match the exact estimator to machine precision.
  set.seed(4271)
  sage = MarginalSAGE$new(
    task = task,
    learner = lrn("regr.rpart"),
    features = "important4",
    resampling = resampling,
    estimator = "kernel",
    n_coalitions = 32L,
    n_samples = 20L
  )
  sage$compute()
  imp = sage$importance()
  expect_importance_dt(imp, features = "important4")
  expect_equal(nrow(imp), 1L)
  checkmate::expect_number(imp$importance, finite = TRUE)

  set.seed(4271)
  exact = MarginalSAGE$new(
    task = task,
    learner = lrn("regr.rpart"),
    features = "important4",
    resampling = resampling,
    estimator = "exact",
    n_samples = 20L
  )
  exact$compute()
  expect_equal(imp$importance, exact$importance()$importance, tolerance = 1e-10)

  # A single feature has no coalition-sampling error, so no SE and no montecarlo CIs.
  checkmate::expect_scalar_na(unique(sage$scores()$se))
  expect_error(sage$importance(ci_method = "montecarlo"), "none are available")
})

# -----------------------------------------------------------------------------
# Correctness: recovers exact Shapley of the empirical value function
# -----------------------------------------------------------------------------

test_that("kernel estimator recovers exact Shapley on a small feature set", {
  # skip_if_not_installed() also loads the mlr3learners namespace, which is what
  # registers "regr.ranger" in the learner dictionary (setup.R only loads mlr3).
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("ranger")
  # With few features the value function can be fully enumerated and exact
  # Shapley values computed by brute force. The regression estimator must
  # converge to these (up to Monte Carlo error) and satisfy the efficiency /
  # sum-to-total constraint exactly.
  #
  # The anchor deliberately uses m = 5 features and a learned game with real
  # interactions: at m = 3 the per-size and per-coalition Shapley kernel weights
  # coincide, and on (near-)additive games any full-support weighting recovers
  # the Shapley values, so either setting would mask a wrong size distribution
  # (see sage_kernel_size_probs). Calibrated separation here: correct weights
  # ~0.001 max abs error, per-coalition weights ~0.008.
  set.seed(0xC0FFEE)
  task = sim_dgp_interactions(n = 200)
  learner = lrn("regr.ranger", num.trees = 25)

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

  # Reconstruct exact Shapley from the SAME value function the estimator uses
  # (shared brute-force reference in helper-importance.R).
  phi = brute_force_shapley(sage, task)
  feats = sage$features

  kern_ordered = kern[match(feats, feature), importance]
  # Efficiency constraint holds exactly (same total, same closed-form solve).
  expect_equal(sum(kern_ordered), sum(phi), tolerance = 1e-8)
  # Tight enough to reject both a wrong coalition-size distribution (~0.008)
  # and a regression to the much higher-variance exact-A estimator.
  expect_lt(max(abs(kern_ordered - phi)), 0.004)
})

test_that("ConditionalSAGE kernel agrees with the exact estimator", {
  set.seed(9241)
  task = sim_dgp_correlated(n = 200)
  learner = lrn("regr.rpart")
  resampling = rsmp("holdout")$instantiate(task)
  sampler = ConditionalGaussianSampler$new(task)

  # The conditional value function is itself stochastic (the sampler redraws per
  # coalition), so both estimates carry sampler noise on top of the kernel
  # estimator's coalition-sampling error; the tolerance reflects that.
  set.seed(11)
  exact = ConditionalSAGE$new(
    task,
    learner,
    resampling = resampling,
    sampler = sampler,
    estimator = "exact",
    n_samples = 30L
  )
  exact$compute()
  set.seed(11)
  kern = ConditionalSAGE$new(
    task,
    learner,
    resampling = resampling,
    sampler = sampler,
    estimator = "kernel",
    n_coalitions = 400L,
    n_samples = 30L
  )
  kern$compute()

  cmp = merge(exact$importance(), kern$importance(), by = "feature")
  expect_gt(cor(cmp$importance.x, cmp$importance.y), 0.98)
  expect_lt(max(abs(cmp$importance.x - cmp$importance.y)), 0.15)
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
# Sampling-budget defaults and guidance
# -----------------------------------------------------------------------------

test_that("budget defaults adapt to the feature count", {
  task5 = sim_dgp_independent(n = 60) # 5 features
  task10 = mlr3::tgen("friedman1")$generate(n = 60) # 10 features
  learner = lrn("regr.rpart")

  # Permutation: capped at 10, reduced to keep 1 + n_permutations * m at or below 2^(m-1).
  expect_identical(MarginalSAGE$new(task10, learner)$param_set$values$n_permutations, 10L)
  expect_identical(MarginalSAGE$new(task5, learner)$param_set$values$n_permutations, 3L)

  # Kernel: 5 * m draws (matching the permutation default's evaluation budget),
  # capped at 512 and at 2^(m-2) (half the enumeration cost) on small feature sets.
  expect_identical(
    MarginalSAGE$new(task10, learner, estimator = "kernel")$param_set$values$n_coalitions,
    50L
  )
  expect_identical(
    MarginalSAGE$new(task5, learner, estimator = "kernel")$param_set$values$n_coalitions,
    8L
  )
})

test_that("compute points to the exact estimator when the budget reaches enumeration cost", {
  skip_if_not_installed("rlang") # reset_message_verbosity() for the once-per-session message
  set.seed(409)
  task = sim_dgp_independent(n = 60) # 5 features, 2^5 = 32 coalitions
  learner = lrn("regr.rpart")
  measure = msr("regr.mse")
  resampling = rsmp("holdout")

  old = xplain_opt(verbose = TRUE)
  withr::defer(xplain_opt(verbose = old$verbose))

  # The message fires once per session and estimator; reset so reruns stay deterministic.
  rlang::reset_message_verbosity("xplainfi_sage_budget_permutation")
  sage_perm = MarginalSAGE$new(task, learner, measure, resampling, n_permutations = 10L, n_samples = 10L)
  expect_message(sage_perm$compute(), "exact") # 1 + 10 * 5 = 51 >= 32

  rlang::reset_message_verbosity("xplainfi_sage_budget_kernel")
  sage_kern = MarginalSAGE$new(
    task,
    learner,
    measure,
    resampling,
    estimator = "kernel",
    n_coalitions = 20L,
    n_samples = 10L
  )
  expect_message(sage_kern$compute(), "exact") # 2 + 2 * 20 = 42 >= 32

  # Adaptive defaults stay below enumeration and compute silently.
  rlang::reset_message_verbosity("xplainfi_sage_budget_permutation")
  sage_def = MarginalSAGE$new(task, learner, measure, resampling, n_samples = 10L)
  expect_no_message(sage_def$compute(), message = "exact")

  # Gated below 3 features, where any usable budget exceeds the 2^m = 4 coalitions.
  rlang::reset_message_verbosity("xplainfi_sage_budget_kernel")
  task2 = mlr3::tgen("2dnormals")$generate(n = 60)
  sage2 = MarginalSAGE$new(
    task2,
    lrn("classif.rpart", predict_type = "prob"),
    msr("classif.ce"),
    rsmp("holdout"),
    estimator = "kernel",
    n_samples = 10L
  )
  expect_no_message(sage2$compute(), message = "exact")
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

# -----------------------------------------------------------------------------
# Monte Carlo confidence intervals (issue #71)
# -----------------------------------------------------------------------------

test_that("kernel estimator reports Monte Carlo standard errors", {
  set.seed(613)
  task = sim_dgp_independent(n = 150)
  sage = MarginalSAGE$new(task, lrn("regr.rpart"), estimator = "kernel", n_coalitions = 120L, n_samples = 20L)
  sage$compute()

  # SE surfaced on the per-iteration scores and populated (not NA) in the history.
  scores = sage$scores()
  checkmate::expect_subset("se", names(scores))
  checkmate::expect_numeric(scores$se, any.missing = FALSE, lower = 0)

  hist_end = sage$convergence_history[n_permutations == max(n_permutations)]
  checkmate::expect_numeric(hist_end$se, any.missing = FALSE, lower = 0)
})

test_that("kernel ci_method = 'montecarlo' returns valid Wald intervals", {
  set.seed(7411)
  task = sim_dgp_independent(n = 200)
  sage = MarginalSAGE$new(task, lrn("regr.rpart"), estimator = "kernel", n_coalitions = 150L, n_samples = 20L)
  sage$compute()

  imp = sage$importance(ci_method = "montecarlo")
  checkmate::expect_subset(c("se", "conf_lower", "conf_upper"), names(imp))
  # Convergence intervals carry no hypothesis test (see docs): no statistic or p.value.
  checkmate::expect_disjunct(c("statistic", "p.value"), names(imp))
  checkmate::expect_numeric(imp$se, any.missing = FALSE, lower = 0)
  checkmate::expect_numeric(imp$conf_lower, any.missing = FALSE, finite = TRUE)
  checkmate::expect_numeric(imp$conf_upper, any.missing = FALSE, finite = TRUE)
  # Point estimate lies inside its own two-sided interval.
  checkmate::expect_numeric(imp$importance - imp$conf_lower, lower = 0, any.missing = FALSE)
  checkmate::expect_numeric(imp$conf_upper - imp$importance, lower = 0, any.missing = FALSE)
  # p_adjust has no p-values to act on and is rejected.
  expect_error(sage$importance(ci_method = "montecarlo", p_adjust = "holm"), "p_adjust")
})

test_that("montecarlo CI width widens with higher confidence and one-sided is unbounded", {
  set.seed(2953)
  # Needs a non-additive game: the default kernel variant is exact on additive games,
  # so an additive DGP drives all SEs to machine noise (exactly 0 on some platforms).
  task = sim_dgp_interactions(n = 200)
  sage = MarginalSAGE$new(task, lrn("regr.rpart"), estimator = "kernel", n_coalitions = 150L, n_samples = 20L)
  sage$compute()

  imp90 = sage$importance(ci_method = "montecarlo", conf_level = 0.90)
  imp99 = sage$importance(ci_method = "montecarlo", conf_level = 0.99)
  w90 = imp90$conf_upper - imp90$conf_lower
  w99 = imp99$conf_upper - imp99$conf_lower
  # Width is 2 * z * se, so a higher level strictly widens the interval
  # wherever the SE is positive.
  checkmate::expect_numeric(w90, lower = 1e-8, any.missing = FALSE)
  expect_gt(min(w99 - w90), 0)

  imp_greater = sage$importance(ci_method = "montecarlo", alternative = "greater")
  expect_identical(unique(imp_greater$conf_upper), Inf)
  checkmate::expect_numeric(imp_greater$conf_lower, finite = TRUE)
})

test_that("permutation estimator also supports montecarlo CIs", {
  set.seed(8161)
  task = sim_dgp_independent(n = 150)
  sage = MarginalSAGE$new(
    task,
    lrn("regr.rpart"),
    estimator = "permutation",
    n_permutations = 15L,
    n_samples = 20L,
    early_stopping = FALSE
  )
  sage$compute()

  checkmate::expect_subset("se", names(sage$scores()))
  imp = sage$importance(ci_method = "montecarlo")
  checkmate::expect_numeric(imp$se, any.missing = FALSE, lower = 0)
  checkmate::expect_subset(c("conf_lower", "conf_upper"), names(imp))
})

test_that("exact estimator rejects montecarlo CIs", {
  task = sim_dgp_independent(n = 150)
  sage = MarginalSAGE$new(task, lrn("regr.rpart"), estimator = "exact", n_samples = 20L)
  sage$compute()

  checkmate::expect_scalar_na(unique(sage$scores()$se))
  expect_error(sage$importance(ci_method = "montecarlo"), "Monte Carlo standard errors")
})

test_that("kernel Monte Carlo SEs are calibrated against replicate variability", {
  # The delta-method SEs must match the actual run-to-run spread of the point
  # estimates when only the coalition draws vary: fixed split, fixed reference
  # subsample (seed before the constructor), deterministic rpart fit, and a
  # non-additive learned game (on additive games the original variant is exact
  # per draw and both spread and SE collapse to numerical noise).
  # This is the failure mode a structural check cannot catch: an SE off by a
  # constant factor passes non-negativity, reproducibility, and ordering tests.
  set.seed(881)
  task = tgen("friedman1")$generate(n = 150)
  task$select(c("important1", "important2", "important3", "important4"))
  learner = lrn("regr.rpart")
  measure = msr("regr.mse")
  resampling = rsmp("holdout")$instantiate(task)

  run_variant = function(variant, reps = 15L) {
    ests = list()
    ses = list()
    for (i in seq_len(reps)) {
      set.seed(505)
      s = MarginalSAGE$new(
        task,
        learner,
        measure,
        resampling = resampling,
        estimator = "kernel",
        kernel_variant = variant,
        n_coalitions = 64L,
        n_samples = 20L
      )
      set.seed(7000 + i)
      s$compute()
      sc = s$scores()
      setorder(sc, feature)
      ests[[i]] = sc$importance
      ses[[i]] = sc$se
    }
    list(
      emp = apply(do.call(rbind, ests), 2, sd),
      se = colMeans(do.call(rbind, ses))
    )
  }

  for (variant in c("original", "unbiased")) {
    r = run_variant(variant)
    # Calibrated ratios (observed 0.95-1.6 over 15 replicates); a factor-of-two
    # miscalibration in either direction fails.
    checkmate::expect_numeric(r$se / r$emp, lower = 0.5, upper = 2, any.missing = FALSE)
  }
})

test_that("kernel Monte Carlo SEs are reproducible with the same seed", {
  task = sim_dgp_independent(n = 150)
  learner = lrn("regr.rpart")

  set.seed(2718)
  sage1 = MarginalSAGE$new(task, learner, estimator = "kernel", n_coalitions = 128L, n_samples = 20L)
  sage1$compute()
  set.seed(2718)
  sage2 = MarginalSAGE$new(task, learner, estimator = "kernel", n_coalitions = 128L, n_samples = 20L)
  sage2$compute()

  expect_equal(
    sage1$importance(ci_method = "montecarlo")$se,
    sage2$importance(ci_method = "montecarlo")$se,
    tolerance = 1e-10
  )
})

# -----------------------------------------------------------------------------
# Unbiased kernel variant (sage package parity)
# -----------------------------------------------------------------------------

test_that("kernel unbiased variant works and reports montecarlo CIs", {
  set.seed(5309)
  task = sim_dgp_independent(n = 150)
  sage = MarginalSAGE$new(
    task,
    lrn("regr.rpart"),
    estimator = "kernel",
    kernel_variant = "unbiased",
    n_coalitions = 200L,
    n_samples = 20L
  )
  expect_identical(sage$param_set$values$kernel_variant, "unbiased")
  sage$compute()
  expect_importance_dt(sage$importance(), features = sage$features)

  imp = sage$importance(ci_method = "montecarlo")
  checkmate::expect_numeric(imp$se, any.missing = FALSE, lower = 0)
  checkmate::expect_numeric(imp$conf_lower, any.missing = FALSE, finite = TRUE)
  checkmate::expect_numeric(imp$conf_upper, any.missing = FALSE, finite = TRUE)
})

test_that("unbiased variant converges to exact Shapley of the value function", {
  set.seed(0xBEEF)
  task = tgen("friedman1")$generate(n = 150)
  task$select(c("important1", "important2", "important4"))

  sage = MarginalSAGE$new(
    task = task,
    learner = lrn("regr.rpart"),
    measure = msr("regr.mse"),
    estimator = "kernel",
    kernel_variant = "unbiased",
    n_coalitions = 2000L,
    n_samples = 20L
  )
  sage$compute()
  est = sage$importance()[match(sage$features, feature), importance]
  phi = brute_force_shapley(sage, task)

  expect_equal(sum(est), sum(phi), tolerance = 1e-8)
  # The unbiased (exact-A) estimator is far noisier than the original variant at
  # equal budgets, so the bound is looser than the original-variant anchor; a bias
  # in the b-only moment path would still miss by a multiple of this.
  expect_lt(max(abs(est - phi)), 0.5)
})

test_that("kernel variants share the efficiency constraint given the same seed", {
  task = sim_dgp_independent(n = 150)
  learner = lrn("regr.rpart")
  resampling = rsmp("holdout")$instantiate(task)

  set.seed(6021)
  sage_orig = MarginalSAGE$new(
    task,
    learner,
    resampling = resampling,
    estimator = "kernel",
    n_coalitions = 200L,
    n_samples = 20L
  )
  sage_orig$compute()
  set.seed(6021)
  sage_unb = MarginalSAGE$new(
    task,
    learner,
    resampling = resampling,
    estimator = "kernel",
    kernel_variant = "unbiased",
    n_coalitions = 200L,
    n_samples = 20L
  )
  sage_unb$compute()

  # Both solve the same constrained problem on the same draws, so the
  # sum-to-total (efficiency) constraint yields identical totals.
  expect_equal(
    sum(sage_orig$importance()$importance),
    sum(sage_unb$importance()$importance),
    tolerance = 1e-8
  )
})

test_that("kernel_variant is gated to the kernel estimator and validated", {
  task = sim_dgp_independent(n = 60)
  learner = lrn("regr.rpart")

  expect_error(
    MarginalSAGE$new(task, learner, estimator = "permutation", kernel_variant = "unbiased"),
    "kernel_variant"
  )
  expect_error(
    MarginalSAGE$new(task, learner, estimator = "exact", kernel_variant = "unbiased"),
    "kernel_variant"
  )
  expect_error(MarginalSAGE$new(task, learner, estimator = "kernel", kernel_variant = "bogus"))
})

# -----------------------------------------------------------------------------
# Regression tests for review findings (2026-07)
# -----------------------------------------------------------------------------

test_that("standardize = TRUE does not mutate stored scores", {
  set.seed(6389)
  task = sim_dgp_independent(n = 150)
  sage = MarginalSAGE$new(task, lrn("regr.rpart"), estimator = "kernel", n_coalitions = 64L, n_samples = 20L)
  sage$compute()
  before = data.table::copy(sage$scores())
  sage$importance(standardize = TRUE)
  sage$importance(standardize = TRUE, ci_method = "montecarlo")
  expect_equal(sage$scores(), before)
})

test_that("montecarlo rejects unknown arguments", {
  set.seed(911)
  task = sim_dgp_independent(n = 120)
  sage = MarginalSAGE$new(task, lrn("regr.rpart"), estimator = "kernel", n_coalitions = 48L, n_samples = 15L)
  sage$compute()
  # Non-prefix typo: prefix typos like `conf_leve` partial-match the formal and
  # are caught by R itself, so only arguments landing in `...` need the guard.
  expect_error(sage$importance(ci_method = "montecarlo", alterantive = "greater"), "Unknown argument")
})

test_that("montecarlo aborts when SEs are unavailable (single permutation)", {
  set.seed(1051)
  task = sim_dgp_independent(n = 120)
  sage = MarginalSAGE$new(task, lrn("regr.rpart"), n_permutations = 1L, n_samples = 15L)
  sage$compute()
  checkmate::expect_scalar_na(unique(sage$scores()$se))
  expect_error(sage$importance(ci_method = "montecarlo"), "none are available")
})

test_that("kernel falls back to the exact design matrix when the sampled one is singular", {
  set.seed(6151)
  task = tgen("friedman1")$generate(n = 100) # 10 features
  sage = MarginalSAGE$new(
    task,
    lrn("regr.rpart"),
    estimator = "kernel",
    # A single paired draw cannot identify a 10x10 design matrix.
    n_coalitions = 1L,
    n_samples = 10L
  )
  expect_warning(sage$compute(), "singular at the end of the coalition budget")

  # The final point estimate exists via the exact-A fallback ...
  checkmate::expect_numeric(sage$scores()$importance, any.missing = FALSE, finite = TRUE)
  # ... but the running history reports NA rather than wild values, SEs are
  # unavailable, and montecarlo CIs abort accordingly.
  checkmate::expect_scalar_na(unique(sage$convergence_history$importance))
  checkmate::expect_scalar_na(unique(sage$scores()$se))
  expect_error(sage$importance(ci_method = "montecarlo"), "none are available")
})

test_that("montecarlo warns when SEs are missing for some resampling iterations", {
  set.seed(4643)
  task = sim_dgp_independent(n = 150)
  sage = MarginalSAGE$new(
    task,
    lrn("regr.rpart"),
    resampling = rsmp("subsampling", repeats = 2),
    estimator = "kernel",
    n_coalitions = 48L,
    n_samples = 15L
  )
  sage$compute()
  # Simulate one iteration failing to produce SEs (e.g. an under-identified budget).
  sage$.__enclos_env__$private$.scores[iter_rsmp == 1L, se := NA_real_]
  expect_warning(
    expect_warning(sage$importance(ci_method = "montecarlo"), "missing for 1 resampling iteration"),
    "pooled across"
  )
})

test_that("montecarlo warns when pooling across resampling iterations", {
  set.seed(733)
  task = sim_dgp_independent(n = 150)
  sage = MarginalSAGE$new(
    task,
    lrn("regr.rpart"),
    resampling = rsmp("subsampling", repeats = 2),
    estimator = "kernel",
    n_coalitions = 48L,
    n_samples = 15L
  )
  sage$compute()
  expect_warning(sage$importance(ci_method = "montecarlo"), "pooled across")
})

test_that("estimator configuration lives in the param_set", {
  set.seed(6007)
  task = sim_dgp_independent(n = 120)
  sage = MarginalSAGE$new(task, lrn("regr.rpart"), estimator = "kernel", n_coalitions = 64L, n_samples = 15L)
  expect_identical(sage$param_set$values$n_coalitions, 64L)
  sage$param_set$values$n_coalitions = 32L
  sage$compute()
  expect_identical(sage$n_permutations_used, 32L)
})

test_that("permutation-only controls warn for other estimators", {
  task = sim_dgp_independent(n = 100)
  learner = lrn("regr.rpart")
  expect_warning(
    MarginalSAGE$new(task, learner, estimator = "kernel", se_threshold = 0.5, n_samples = 15L),
    "only used by"
  )
  expect_warning(
    MarginalSAGE$new(task, learner, estimator = "kernel", max_features = 5L, n_samples = 15L),
    "max_features"
  )
  set.seed(5527)
  sage = MarginalSAGE$new(task, learner, estimator = "kernel", n_coalitions = 16L, n_samples = 10L)
  expect_warning(sage$compute(early_stopping = TRUE), "only used by")
})

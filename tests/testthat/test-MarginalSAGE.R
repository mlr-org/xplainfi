# =============================================================================
# MarginalSAGE Tests
# =============================================================================

# -----------------------------------------------------------------------------
# Basic functionality
# -----------------------------------------------------------------------------

test_that("MarginalSAGE default behavior with minimal parameters", {
	# Use small params for test speed
	test_default_behavior(MarginalSAGE, task_type = "regr", n_permutations = 2L, n_samples = 20L)
})

test_that("MarginalSAGE works with classification tasks", {
	# Binary classification
	task_binary = tgen("2dnormals")$generate(n = 100)
	sage_binary = MarginalSAGE$new(
		task = task_binary,
		learner = lrn("classif.rpart", predict_type = "prob"),
		n_permutations = 2L
	)
	checkmate::expect_r6(sage_binary, c("FeatureImportanceMethod", "SAGE", "MarginalSAGE"))
	sage_binary$compute()
	expect_importance_dt(sage_binary$importance(), features = sage_binary$features)

	# Multiclass classification
	task_multi = tgen("cassini")$generate(n = 100)
	sage_multi = MarginalSAGE$new(
		task = task_multi,
		learner = lrn("classif.rpart", predict_type = "prob"),
		n_permutations = 2L
	)
	sage_multi$compute()
	expect_importance_dt(sage_multi$importance(), features = sage_multi$features)
	expect_length(task_multi$class_names, 3L)
})

test_that("MarginalSAGE featureless learner produces zero importance", {
	# Use small params for test speed
	test_featureless_zero_importance(
		MarginalSAGE,
		task_type = "regr",
		n_permutations = 2L,
		n_samples = 20L
	)
})

# -----------------------------------------------------------------------------
# Sensible results
# -----------------------------------------------------------------------------

test_that("MarginalSAGE friedman1 produces sensible ranking", {
	# Use small params for test speed
	test_friedman1_sensible_ranking(MarginalSAGE, n = 200L, n_permutations = 2L, n_samples = 20L)
})

# -----------------------------------------------------------------------------
# Resampling
# -----------------------------------------------------------------------------

test_that("MarginalSAGE with cross-validation resampling", {
	task = tgen("friedman1")$generate(n = 200)

	sage = MarginalSAGE$new(
		task = task,
		learner = lrn("regr.rpart"),
		resampling = rsmp("cv", folds = 3),
		n_permutations = 2L
	)
	sage$compute()

	expect_importance_dt(sage$importance(), features = sage$features)
	checkmate::expect_data_table(
		sage$scores(),
		types = c("integer", "character", "numeric"),
		nrows = sage$resampling$iters * length(sage$features),
		ncols = 3,
		any.missing = FALSE
	)
})

# -----------------------------------------------------------------------------
# Single feature
# -----------------------------------------------------------------------------

test_that("MarginalSAGE with single feature", {
	task = tgen("friedman1")$generate(n = 100)

	sage = MarginalSAGE$new(
		task = task,
		learner = lrn("regr.rpart"),
		features = "important4",
		n_permutations = 2L
	)
	sage$compute()

	expect_importance_dt(sage$importance(), features = "important4")
	expect_equal(nrow(sage$importance()), 1L)
})

# -----------------------------------------------------------------------------
# n_samples parameter
# -----------------------------------------------------------------------------

test_that("MarginalSAGE with custom n_samples", {
	task = tgen("friedman1")$generate(n = 200)

	sage = MarginalSAGE$new(
		task = task,
		learner = lrn("regr.rpart"),
		n_samples = 30L,
		n_permutations = 2L
	)
	sage$compute()

	expect_importance_dt(sage$importance(), features = sage$features)
})

# -----------------------------------------------------------------------------
# Reproducibility
# -----------------------------------------------------------------------------

test_that("MarginalSAGE reproducibility with same seed", {
	task = tgen("2dnormals")$generate(n = 100)
	learner = lrn("classif.rpart", predict_type = "prob")
	measure = msr("classif.ce")

	set.seed(42)
	sage1 = MarginalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		n_permutations = 3L
	)
	sage1$compute()
	result1 = sage1$importance()

	set.seed(42)
	sage2 = MarginalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		n_permutations = 3L
	)
	sage2$compute()
	result2 = sage2$importance()

	# Results should be identical with same seed
	expect_equal(result1$importance, result2$importance, tolerance = 1e-10)
})

# -----------------------------------------------------------------------------
# Parameter validation
# -----------------------------------------------------------------------------

test_that("MarginalSAGE parameter validation", {
	task = tgen("friedman1")$generate(n = 50)
	learner = lrn("regr.rpart")

	# n_permutations must be positive integer
	expect_error(MarginalSAGE$new(task = task, learner = learner, n_permutations = 0L))
	expect_error(MarginalSAGE$new(task = task, learner = learner, n_permutations = -1L))
})

test_that("MarginalSAGE requires predict_type='prob' for classification", {
	task = tgen("2dnormals")$generate(n = 50)

	# Should error for classification without predict_type = "prob"
	expect_error(
		MarginalSAGE$new(
			task = task,
			learner = lrn("classif.rpart", predict_type = "response")
		),
		"Classification learners require probability predictions for SAGE."
	)
})

# -----------------------------------------------------------------------------
# Convergence tracking
# -----------------------------------------------------------------------------

test_that("MarginalSAGE SE tracking in convergence_history", {
	task = tgen("friedman1")$generate(n = 30)
	learner = lrn("regr.rpart")
	measure = msr("regr.mse")

	sage = MarginalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		n_permutations = 6L,
		n_samples = 20L
	)

	# Compute with early stopping to get convergence history
	sage$compute(early_stopping = TRUE, se_threshold = 0.05, check_interval = 2L)

	# Check that convergence_history exists and has SE column
	expect_false(is.null(sage$convergence_history))
	expect_contains(colnames(sage$convergence_history), "se")

	# Check structure of convergence_history
	expected_cols = c("n_permutations", "feature", "importance", "se")
	expect_setequal(colnames(sage$convergence_history), expected_cols)

	# SE values should be non-negative and finite
	se_values = sage$convergence_history$se
	checkmate::expect_numeric(se_values, lower = 0, finite = TRUE)

	# For each feature, SE should be in a reasonable range
	for (feat in unique(sage$convergence_history$feature)) {
		feat_data = sage$convergence_history[feature == feat]
		feat_data = feat_data[order(n_permutations)]

		if (nrow(feat_data) > 1) {
			# Just check that SE values are in a reasonable range and not exploding
			expect_lt(max(feat_data$se), 10)
			expect_lt(max(abs(diff(feat_data$se))), 5)
		}
	}

	# All features should be represented in convergence history
	expect_setequal(
		unique(sage$convergence_history$feature),
		sage$features
	)
})

test_that("MarginalSAGE SE-based convergence detection", {
	skip_on_cran() # ~1s - tests early stopping feature, not core SAGE

	task = tgen("friedman1")$generate(n = 100)
	learner = lrn("regr.rpart")
	measure = msr("regr.mse")

	sage = MarginalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		n_permutations = 10L,
		n_samples = 20L
	)

	# Test with very loose SE threshold (should trigger convergence easily)
	sage$compute(
		early_stopping = TRUE,
		se_threshold = 100.0,
		min_permutations = 5L,
		check_interval = 1L
	)

	# Should converge early because SE will be well below 100.0
	expect_true(sage$converged)
	expect_lte(sage$n_permutations_used, 10L)

	# Reset for next test
	sage$reset()

	# Test with very strict SE threshold (should not converge)
	sage$compute(
		early_stopping = TRUE,
		se_threshold = 0.001,
		min_permutations = 5L,
		check_interval = 1L
	)

	# With very strict SE threshold, should not converge early
	expect_false(sage$converged)

	# Test with moderate SE threshold
	sage$reset()

	sage$compute(
		early_stopping = TRUE,
		se_threshold = 0.1,
		min_permutations = 5L,
		check_interval = 1L
	)

	# Should have convergence history with SE tracking regardless of convergence
	expect_false(is.null(sage$convergence_history))
	expect_contains(colnames(sage$convergence_history), "se")
})

test_that("MarginalSAGE .sage_baseline equals empty-coalition loss", {
	withr::local_seed(20260517)
	task = sim_dgp_independent(n = 120)
	learner = lrn("regr.rpart")
	sage = MarginalSAGE$new(task = task, learner = learner, n_permutations = 2L, n_samples = 20L)
	rr = assemble_rr(task, learner, sage$resampling, store_models = TRUE)
	test_dt = task$data(rows = rr$resampling$test_set(1))

	priv = sage$.__enclos_env__$private
	baseline = priv$.sage_baseline(rr$learners[[1]], test_dt, batch_size = 5000L)
	# Empty coalition routed through the existing batch evaluator
	via_batch = priv$.evaluate_coalitions_batch(
		rr$learners[[1]],
		test_dt,
		list(character(0)),
		5000L
	)[1]

	checkmate::expect_number(baseline, finite = TRUE)
	expect_equal(baseline, via_batch)
})

test_that("compute_chunk_partial reproduces sequential checkpoint math", {
	withr::local_seed(424242)
	task = sim_dgp_independent(n = 150)
	learner = lrn("regr.rpart")
	sage = MarginalSAGE$new(task = task, learner = learner, n_permutations = 6L, n_samples = 25L)
	rr = assemble_rr(task, learner, sage$resampling, store_models = TRUE)
	test_dt = task$data(rows = rr$resampling$test_set(1))
	priv = sage$.__enclos_env__$private

	withr::local_seed(7)
	perms = replicate(6L, sample(sage$features), simplify = FALSE)
	baseline = priv$.sage_baseline(rr$learners[[1]], test_dt, 5000L)

	# One chunk of all 6 permutations
	full = sage$compute_chunk_partial(rr$learners[[1]], test_dt, perms, baseline, 5000L)
	# Two chunks of 3 — additive reduction must match
	a = sage$compute_chunk_partial(rr$learners[[1]], test_dt, perms[1:3], baseline, 5000L)
	b = sage$compute_chunk_partial(rr$learners[[1]], test_dt, perms[4:6], baseline, 5000L)

	expect_equal(full$n, 6L)
	expect_equal(a$sv + b$sv, full$sv, tolerance = 1e-10)
	expect_equal(a$sv_sq + b$sv_sq, full$sv_sq, tolerance = 1e-10)
	checkmate::expect_numeric(full$sv, names = "named", any.missing = FALSE)
	expect_setequal(names(full$sv), sage$features)
})

test_that("MarginalSAGE refactor preserves results under sequential fallback", {
	withr::local_options(xplainfi.sequential = TRUE)

	run_sage = function() {
		withr::local_seed(13)
		task = sim_dgp_independent(n = 160)
		sage = MarginalSAGE$new(
			task = task,
			learner = lrn("regr.rpart"),
			n_permutations = 8L,
			n_samples = 25L,
			early_stopping = FALSE
		)
		sage$compute()
		sage
	}

	a = run_sage()
	b = run_sage()

	expect_importance_dt(a$importance(), features = a$features)
	expect_equal(sort(names(a$scores())), sort(c("iter_rsmp", "feature", "importance")))
	# Deterministic under sequential + fixed seed: two runs identical.
	expect_equal(a$importance()$importance, b$importance()$importance, tolerance = 1e-10)
})

test_that("MarginalSAGE iters > 1 keeps per-iter schema under sequential fallback", {
	withr::local_options(xplainfi.sequential = TRUE)
	withr::local_seed(99)
	task = sim_dgp_independent(n = 160)
	sage = MarginalSAGE$new(
		task = task,
		learner = lrn("regr.rpart"),
		resampling = rsmp("cv", folds = 3),
		n_permutations = 4L,
		n_samples = 20L,
		early_stopping = FALSE
	)
	sage$compute()
	scores = sage$scores()
	expect_setequal(unique(scores$iter_rsmp), 1:3)
	expect_importance_dt(sage$importance(), features = sage$features)
})

test_that("MarginalSAGE parallel (future) matches sequential within MC tolerance", {
	skip_on_cran()
	skip_if_not_installed("future")

	# SAGE's permutation set, the simulated task, and the internal
	# resample() all consume the RNG stream, so a fair parallel-vs-
	# sequential comparison must seed the *entire* pipeline (task +
	# construction + compute) identically for each run. With the same
	# permutation set the parallel reduction must reproduce sequential
	# exactly -- a stronger check than rank agreement, and one that is
	# not fooled by MC ties among the near-zero noise features.
	run_sage = function(sequential) {
		set.seed(2026)
		task = sim_dgp_independent(n = 200)
		withr::local_options(xplainfi.sequential = sequential)
		sage = MarginalSAGE$new(
			task = task,
			learner = lrn("regr.rpart"),
			n_permutations = 40L,
			n_samples = 30L,
			early_stopping = FALSE
		)
		sage$compute()
		sage
	}

	seq_sage = run_sage(TRUE)

	old_plan = future::plan("multisession", workers = 2)
	withr::defer(future::plan(old_plan))
	par_sage = run_sage(FALSE)

	expect_importance_dt(par_sage$importance(), features = par_sage$features)
	expect_setequal(par_sage$scores()$feature, seq_sage$scores()$feature)
	# Same permutation set => parallel reduction must reproduce the
	# sequential importances exactly, and therefore the rankings too.
	expect_equal(
		par_sage$importance()$importance,
		seq_sage$importance()$importance
	)
	expect_equal(
		rank(par_sage$importance()$importance),
		rank(seq_sage$importance()$importance)
	)
})

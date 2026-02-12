# =============================================================================
# Tests for pretrained learner support
# =============================================================================

# -----------------------------------------------------------------------------
# assert_pretrained() unit tests
# -----------------------------------------------------------------------------

test_that("assert_pretrained returns FALSE for untrained learner", {
	task = tsk("mtcars")
	learner = lrn("regr.rpart")
	resampling = rsmp("holdout")$instantiate(task)

	expect_false(assert_pretrained(learner, task, resampling))
})

test_that("assert_pretrained returns TRUE for valid pretrained setup", {
	task = tsk("mtcars")
	learner = lrn("regr.rpart")
	learner$train(task)
	resampling = rsmp("holdout")$instantiate(task)

	expect_true(assert_pretrained(learner, task, resampling))
})

test_that("assert_pretrained errors when trained learner has multi-fold resampling", {
	task = tsk("mtcars")
	learner = lrn("regr.rpart")
	learner$train(task)
	resampling = rsmp("cv", folds = 3)$instantiate(task)

	expect_error(assert_pretrained(learner, task, resampling), "not compatible")
})

# -----------------------------------------------------------------------------
# PFI with pretrained learner
# -----------------------------------------------------------------------------

test_that("PFI works with pretrained learner", {
	task = tsk("mtcars")
	learner = lrn("regr.rpart")
	resampling = rsmp("holdout")$instantiate(task)
	measure = msr("regr.mse")

	learner$train(task)

	pfi = PFI$new(
		task = task,
		learner = learner,
		measure = measure,
		resampling = resampling,
		n_repeats = 3L
	)

	pfi$compute()
	expect_importance_dt(pfi$importance(), features = pfi$features)
	checkmate::expect_r6(pfi$resample_result, "ResampleResult")

	# Original learner must still have its model after compute
	expect_false(is.null(learner$model))
})

test_that("PFI pretrained and non-pretrained produce comparable results", {
	set.seed(123)
	task = tsk("mtcars")
	resampling = rsmp("holdout")$instantiate(task)
	measure = msr("regr.mse")

	# Pretrained path
	learner_pre = lrn("regr.rpart")
	learner_pre$train(task)

	set.seed(1)
	pfi_pre = PFI$new(
		task = task,
		learner = learner_pre,
		measure = measure,
		resampling = resampling,
		n_repeats = 5L
	)
	pfi_pre$compute()

	# Non-pretrained path (fresh learner, same resampling)
	set.seed(1)
	pfi_fresh = PFI$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = measure,
		resampling = resampling,
		n_repeats = 5L
	)
	pfi_fresh$compute()

	# Both should produce valid importance tables with same features
	expect_importance_dt(pfi_pre$importance(), features = pfi_pre$features)
	expect_importance_dt(pfi_fresh$importance(), features = pfi_fresh$features)
	expect_setequal(pfi_pre$importance()$feature, pfi_fresh$importance()$feature)
})

test_that("PFI with pretrained learner errors on non-instantiated resampling", {
	task = tsk("mtcars")
	learner = lrn("regr.rpart")
	learner$train(task)
	measure = msr("regr.mse")

	expect_error(
		PFI$new(
			task = task,
			learner = learner,
			measure = measure,
			resampling = rsmp("holdout")
		),
		"instantiated"
	)
})

test_that("PFI with pretrained learner errors on multi-fold resampling at construction", {
	task = tsk("mtcars")
	learner = lrn("regr.rpart")
	learner$train(task)
	measure = msr("regr.mse")

	expect_error(
		PFI$new(
			task = task,
			learner = learner,
			measure = measure,
			resampling = rsmp("cv", folds = 3)$instantiate(task),
			n_repeats = 2L
		),
		"not compatible"
	)
})

test_that("PFI with pretrained learner works for classification", {
	task = tsk("penguins")
	learner = lrn("classif.rpart", predict_type = "prob")
	resampling = rsmp("holdout")$instantiate(task)
	measure = msr("classif.ce")

	learner$train(task)

	pfi = PFI$new(
		task = task,
		learner = learner,
		measure = measure,
		resampling = resampling,
		n_repeats = 3L
	)

	pfi$compute()
	expect_importance_dt(pfi$importance(), features = pfi$features)
})

# -----------------------------------------------------------------------------
# MarginalSAGE with pretrained learner
# -----------------------------------------------------------------------------

test_that("MarginalSAGE works with pretrained learner", {
	task = tsk("mtcars")
	learner = lrn("regr.rpart")
	resampling = rsmp("holdout")$instantiate(task)
	measure = msr("regr.mse")

	learner$train(task)

	sage = MarginalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		resampling = resampling,
		n_permutations = 2L,
		n_samples = 20L
	)

	sage$compute()
	expect_importance_dt(sage$importance(), features = sage$features)
	checkmate::expect_r6(sage$resample_result, "ResampleResult")
})

test_that("MarginalSAGE pretrained and non-pretrained produce comparable results", {
	set.seed(123)
	task = tsk("mtcars")
	resampling = rsmp("holdout")$instantiate(task)
	measure = msr("regr.mse")

	# Pretrained path
	learner_pre = lrn("regr.rpart")
	learner_pre$train(task)

	set.seed(1)
	sage_pre = MarginalSAGE$new(
		task = task,
		learner = learner_pre,
		measure = measure,
		resampling = resampling,
		n_permutations = 3L,
		n_samples = 20L
	)
	sage_pre$compute()

	# Non-pretrained path
	set.seed(1)
	sage_fresh = MarginalSAGE$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = measure,
		resampling = resampling,
		n_permutations = 3L,
		n_samples = 20L
	)
	sage_fresh$compute()

	expect_importance_dt(sage_pre$importance(), features = sage_pre$features)
	expect_importance_dt(sage_fresh$importance(), features = sage_fresh$features)
	expect_setequal(sage_pre$importance()$feature, sage_fresh$importance()$feature)
})

test_that("MarginalSAGE with pretrained learner errors on non-instantiated resampling", {
	task = tsk("mtcars")
	learner = lrn("regr.rpart")
	learner$train(task)
	measure = msr("regr.mse")

	expect_error(
		MarginalSAGE$new(
			task = task,
			learner = learner,
			measure = measure,
			resampling = rsmp("holdout"),
			n_permutations = 2L,
			n_samples = 20L
		),
		"instantiated"
	)
})

test_that("MarginalSAGE with pretrained learner errors on multi-fold resampling at construction", {
	task = tsk("mtcars")
	learner = lrn("regr.rpart")
	learner$train(task)
	measure = msr("regr.mse")

	expect_error(
		MarginalSAGE$new(
			task = task,
			learner = learner,
			measure = measure,
			resampling = rsmp("cv", folds = 3)$instantiate(task),
			n_permutations = 2L,
			n_samples = 20L
		),
		"not compatible"
	)
})

test_that("MarginalSAGE with pretrained learner works for classification", {
	task = tsk("penguins")
	learner = lrn("classif.rpart", predict_type = "prob")
	resampling = rsmp("holdout")$instantiate(task)
	measure = msr("classif.ce")

	learner$train(task)

	sage = MarginalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		resampling = resampling,
		n_permutations = 2L,
		n_samples = 20L
	)

	sage$compute()
	expect_importance_dt(sage$importance(), features = sage$features)
})

# -----------------------------------------------------------------------------
# LOCO warns for pretrained learner (refit-based methods)
# -----------------------------------------------------------------------------

test_that("LOCO warns when given a pretrained learner", {
	task = tsk("mtcars")
	learner = lrn("regr.rpart")
	learner$train(task)

	expect_warning(
		LOCO$new(
			task = task,
			learner = learner,
			measure = msr("regr.mse")
		),
		"already trained"
	)
})

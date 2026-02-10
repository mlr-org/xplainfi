test_that("importance() accepts all ci_method values", {
	task = sim_dgp_independent(n = 100)

	pfi = PFI$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		resampling = rsmp("subsampling", repeats = 5),
		n_repeats = 2
	)

	pfi$compute()

	# Test that all variance methods work
	imp_none = pfi$importance(ci_method = "none")
	imp_raw = pfi$importance(ci_method = "raw")
	expect_warning(
		imp_nb <- pfi$importance(ci_method = "nadeau_bengio")
	)
	imp_quantile = pfi$importance(ci_method = "quantile")

	expect_importance_dt(imp_none, features = pfi$features)
	expect_importance_dt(imp_raw, features = pfi$features)
	expect_importance_dt(imp_nb, features = pfi$features)
	expect_importance_dt(imp_quantile, features = pfi$features)
})

test_that("ci_method='none' produces no variance columns", {
	task = sim_dgp_independent(n = 100)

	pfi = PFI$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		resampling = rsmp("subsampling", repeats = 5),
		n_repeats = 2
	)

	pfi$compute()
	imp_none = pfi$importance(ci_method = "none")

	# Check that only feature and importance columns exist
	expect_equal(names(imp_none), c("feature", "importance"))
})

test_that("raw CIs are narrower than nadeau_bengio corrected CIs", {
	task = sim_dgp_independent(n = 200)

	pfi = PFI$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		resampling = rsmp("subsampling", repeats = 11, ratio = 0.8),
		n_repeats = 3
	)

	pfi$compute()

	# Use two-sided to compare finite CI widths
	imp_raw = pfi$importance(ci_method = "raw", alternative = "two.sided")
	imp_nb = pfi$importance(ci_method = "nadeau_bengio", alternative = "two.sided")

	# Calculate CI widths
	width_raw = imp_raw$conf_upper - imp_raw$conf_lower
	width_nb = imp_nb$conf_upper - imp_nb$conf_lower

	# Raw CIs should be narrower than corrected ones on average
	# Compare the mean widths instead of individual features
	# The nadeau_bengio correction factor should make CIs wider on average
	expect_true(mean(width_nb) > mean(width_raw))
})

test_that("nadeau_bengio correction requires appropriate resampling", {
	task = sim_dgp_independent(n = 100)

	# Cross-validation is not supported for nadeau_bengio
	pfi = PFI$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		resampling = rsmp("cv", folds = 3),
		n_repeats = 2
	)

	pfi$compute()

	# Should error for unsupported resampling
	expect_warning(
		pfi$importance(ci_method = "nadeau_bengio"),
		regexp = "recommended for resampling types"
	)

	# But raw variance should still work
	imp_raw = pfi$importance(ci_method = "raw")
	expect_importance_dt(imp_raw, features = pfi$features)
})

test_that("confidence level parameter works correctly", {
	task = sim_dgp_independent(n = 100)

	pfi = PFI$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		resampling = rsmp("subsampling", repeats = 5),
		n_repeats = 2
	)

	pfi$compute()

	# Test different confidence levels with two-sided CIs to compare widths
	imp_90 = pfi$importance(ci_method = "raw", conf_level = 0.90, alternative = "two.sided")
	imp_95 = pfi$importance(ci_method = "raw", conf_level = 0.95, alternative = "two.sided")
	imp_99 = pfi$importance(ci_method = "raw", conf_level = 0.99, alternative = "two.sided")

	# Calculate CI widths
	width_90 = imp_90$conf_upper - imp_90$conf_lower
	width_95 = imp_95$conf_upper - imp_95$conf_lower
	width_99 = imp_99$conf_upper - imp_99$conf_lower

	# Higher confidence level should produce wider CIs (on average)
	expect_true(mean(width_90) < mean(width_95))
	expect_true(mean(width_95) < mean(width_99))
})

test_that("variance estimation works with bootstrap resampling", {
	task = sim_dgp_independent(n = 100)

	pfi = PFI$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		resampling = rsmp("bootstrap", repeats = 11),
		n_repeats = 2
	)

	pfi$compute()

	# Both raw and nadeau_bengio should work with bootstrap
	imp_raw = pfi$importance(ci_method = "raw")
	imp_nb = pfi$importance(ci_method = "nadeau_bengio")

	expect_importance_dt(imp_raw, features = pfi$features)
	expect_importance_dt(imp_nb, features = pfi$features)

	# Verify variance columns exist
	expect_true(all(c("se", "conf_lower", "conf_upper") %in% names(imp_raw)))
	expect_true(all(c("se", "conf_lower", "conf_upper") %in% names(imp_nb)))
})

test_that("quantile variance method works", {
	task = sim_dgp_independent(n = 200)

	pfi = PFI$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		resampling = rsmp("subsampling", repeats = 5),
		n_repeats = 2
	)

	pfi$compute()

	# Use two-sided for testing finite CI bounds
	imp_quantile = pfi$importance(ci_method = "quantile", alternative = "two.sided")

	# Check structure
	expect_importance_dt(imp_quantile, features = pfi$features)

	# Verify all inference columns exist (consistent with other methods)
	expected_cols = c("feature", "importance", "se", "statistic", "p.value", "conf_lower", "conf_upper")
	expect_true(all(expected_cols %in% names(imp_quantile)))

	# All CIs should be valid intervals (two-sided has finite bounds)
	expect_true(all(imp_quantile$conf_lower <= imp_quantile$conf_upper))

	# Point estimates should be between lower and upper bounds (or close)
	# Due to using mean vs quantiles, this is not guaranteed but usually holds
	expect_true(all(
		imp_quantile$importance >= imp_quantile$conf_lower |
			abs(imp_quantile$importance - imp_quantile$conf_lower) < 0.01
	))
	expect_true(all(
		imp_quantile$importance <= imp_quantile$conf_upper |
			abs(imp_quantile$importance - imp_quantile$conf_upper) < 0.01
	))
})

test_that("quantile CIs differ from parametric methods", {
	task = sim_dgp_independent(n = 200)

	pfi = PFI$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		resampling = rsmp("subsampling", repeats = 15),
		n_repeats = 3
	)

	pfi$compute()

	# Use two-sided to compare finite CI bounds
	imp_raw = pfi$importance(ci_method = "raw", alternative = "two.sided")
	imp_quantile = pfi$importance(ci_method = "quantile", alternative = "two.sided")

	# Point estimates should be the same (both use mean)
	expect_equal(imp_raw$importance, imp_quantile$importance)

	# CIs should generally differ between methods
	# (quantile is non-parametric, raw assumes normality)
	expect_false(all(imp_raw$conf_lower == imp_quantile$conf_lower))
	expect_false(all(imp_raw$conf_upper == imp_quantile$conf_upper))
})

test_that("alternative='greater' produces one-sided CIs and tests", {
	task = sim_dgp_independent(n = 200)

	pfi = PFI$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		resampling = rsmp("subsampling", repeats = 11),
		n_repeats = 3
	)

	pfi$compute()

	# Test raw method with greater alternative
	imp_raw = pfi$importance(ci_method = "raw", alternative = "greater")

	# Should have statistic and p.value columns
	expect_true(all(c("statistic", "p.value") %in% names(imp_raw)))

	# Upper bound should be Inf for one-sided
	expect_true(all(is.infinite(imp_raw$conf_upper)))
	expect_true(all(imp_raw$conf_upper > 0)) # Inf, not -Inf

	# Lower bound should be finite
	expect_true(all(is.finite(imp_raw$conf_lower)))

	# Test nadeau_bengio with greater alternative
	imp_nb = pfi$importance(ci_method = "nadeau_bengio", alternative = "greater")
	expect_true(all(is.infinite(imp_nb$conf_upper)))
	expect_true(all(c("statistic", "p.value") %in% names(imp_nb)))

	# Test quantile with greater alternative
	imp_quantile = pfi$importance(ci_method = "quantile", alternative = "greater")
	expect_true(all(is.infinite(imp_quantile$conf_upper)))
})

test_that("alternative='two.sided' produces two-sided CIs and tests", {
	task = sim_dgp_independent(n = 200)

	pfi = PFI$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		resampling = rsmp("subsampling", repeats = 11),
		n_repeats = 3
	)

	pfi$compute()

	# Test raw method with two.sided alternative
	imp_raw = pfi$importance(ci_method = "raw", alternative = "two.sided")

	# Should have statistic and p.value columns
	expect_true(all(c("statistic", "p.value") %in% names(imp_raw)))

	# Both bounds should be finite
	expect_true(all(is.finite(imp_raw$conf_lower)))
	expect_true(all(is.finite(imp_raw$conf_upper)))

	# Test nadeau_bengio
	imp_nb = pfi$importance(ci_method = "nadeau_bengio", alternative = "two.sided")
	expect_true(all(is.finite(imp_nb$conf_upper)))
	expect_true(all(c("statistic", "p.value") %in% names(imp_nb)))

	# Test quantile
	imp_quantile = pfi$importance(ci_method = "quantile", alternative = "two.sided")
	expect_true(all(is.finite(imp_quantile$conf_upper)))
})

test_that("two-sided p-values are larger than one-sided for positive importance", {
	task = sim_dgp_independent(n = 200)

	pfi = PFI$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		resampling = rsmp("subsampling", repeats = 11),
		n_repeats = 3
	)

	pfi$compute()

	imp_greater = pfi$importance(ci_method = "raw", alternative = "greater")
	imp_twosided = pfi$importance(ci_method = "raw", alternative = "two.sided")

	# For features with positive importance, two-sided p-values should be ~2x one-sided
	positive_mask = imp_greater$importance > 0
	if (any(positive_mask)) {
		expect_true(all(
			imp_twosided$p.value[positive_mask] >= imp_greater$p.value[positive_mask]
		))
	}

	# Test statistics should be identical regardless of alternative
	expect_equal(imp_greater$statistic, imp_twosided$statistic)
})

test_that("default alternative is 'greater' (one-sided)", {
	task = sim_dgp_independent(n = 100)

	pfi = PFI$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse"),
		resampling = rsmp("subsampling", repeats = 5),
		n_repeats = 2
	)

	pfi$compute()

	# Default should be one-sided
	imp_default = pfi$importance(ci_method = "raw")
	imp_greater = pfi$importance(ci_method = "raw", alternative = "greater")

	expect_equal(imp_default$conf_upper, imp_greater$conf_upper)
	expect_equal(imp_default$p.value, imp_greater$p.value)
})

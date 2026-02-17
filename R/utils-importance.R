# Importance Aggregation Methods ----
# These functions provide different methods for aggregating importance scores
# across resampling iterations and computing confidence intervals.

# Multiplicity Adjustment Helpers ----

#' Compute adjusted alpha for CI construction
#'
#' Only Bonferroni has a clean per-comparison alpha (alpha/k) for CI coverage.
#' For all other methods, CIs use the nominal alpha unchanged.
#'
#' @param alpha nominal significance level (1 - conf_level)
#' @param p_adjust p-value adjustment method name
#' @param k number of comparisons (features)
#' @return adjusted alpha for CI construction
#' @noRd
adjust_ci_alpha = function(alpha, p_adjust, k) {
	if (p_adjust == "bonferroni" && k > 1) {
		alpha / k
	} else {
		alpha
	}
}

#' Apply p-value adjustment to importance results
#'
#' @param dt data.table with a p.value column
#' @param p_adjust p-value adjustment method name
#' @return dt (modified in place) with adjusted p-values
#' @noRd
adjust_pvalues = function(dt, p_adjust) {
	p.value <- NULL
	if (p_adjust != "none" && "p.value" %in% names(dt)) {
		dt[, p.value := stats::p.adjust(p.value, method = p_adjust)]
	}
	dt
}

#' Warn if any test observation appears in more than one resampling test set
#'
#' Both CPI and Lei et al. inference assume unique test observations.
#' The iter_repeat dimension (refit/sampling repeats) is averaged over and
#' irrelevant here — what matters is whether a row_id appears in multiple
#' resampling iterations (iter_rsmp), e.g. with subsampling.
#'
#' @param obs_loss_data data.table with columns row_ids, iter_rsmp
#' @noRd
check_unique_test_obs = function(obs_loss_data) {
	N <- NULL
	obs_per_rsmp = unique(obs_loss_data[, .(row_ids, iter_rsmp)])
	dupes = obs_per_rsmp[, .N, by = "row_ids"][N > 1]

	if (nrow(dupes) >= 1) {
		cli::cli_warn(c(
			"Found {.val {nrow(dupes)}} observation{?s} appearing in multiple test sets.",
			x = "Inference assumes unique test observations; duplicates may invalidate inference.",
			i = "Use a resampling strategy where each observation appears at most once in the test set(s)."
		))
	}
}

#' No variance estimation - just aggregated performance
#' @param scores ([data.table::data.table]) with feature and importance columns
#' @param aggregator (`function`) to aggregate importance scores
#' @param conf_level (`numeric(1)`) ignored for this method
#' @noRd
importance_none = function(scores, aggregator, conf_level) {
	# The data.table NSE tax
	importance <- NULL

	agg_importance = scores[,
		list(importance = aggregator(importance)),
		by = "feature"
	]
	agg_importance
}

#' Raw variance estimation without correction
#'
#' Standard t-test on resampling scores. CIs are too narrow due to non-independence.
#' SE = sd(scores) / sqrt(n), t-statistic with df = n - 1.
#'
#' @param scores data.table with feature, importance, iter_rsmp columns
#' @param aggregator function to aggregate importance scores
#' @param conf_level confidence level for intervals
#' @param alternative "greater" (one-sided) or "two.sided"
#' @param n_iters number of resampling iterations
#' @noRd
importance_raw = function(scores, aggregator, conf_level, alternative, n_iters, p_adjust = "none") {
	# The data.table NSE tax
	importance <- se <- statistic <- p.value <- NULL
	agg_importance = scores[,
		list(importance = aggregator(importance)),
		by = "feature"
	]

	# Aggregate within resamplings first to get one row per resampling iter
	means_rsmp = scores[,
		list(importance = mean(importance)),
		by = c("iter_rsmp", "feature")
	]

	sds = means_rsmp[,
		list(se = sqrt(stats::var(importance) / n_iters)),
		by = "feature"
	]

	agg_importance = agg_importance[sds, on = "feature"]

	# t-test: H0: importance = 0
	df = n_iters - 1
	agg_importance[, statistic := importance / se]

	k = nrow(agg_importance)
	ci_alpha = adjust_ci_alpha(1 - conf_level, p_adjust, k)

	if (alternative == "greater") {
		agg_importance[, p.value := stats::pt(statistic, df = df, lower.tail = FALSE)]
		quant = stats::qt(1 - ci_alpha, df = df)
		agg_importance[, let(
			conf_lower = importance - quant * se,
			conf_upper = Inf
		)]
	} else {
		agg_importance[, p.value := 2 * stats::pt(abs(statistic), df = df, lower.tail = FALSE)]
		quant = stats::qt(1 - ci_alpha / 2, df = df)
		agg_importance[, let(
			conf_lower = importance - quant * se,
			conf_upper = importance + quant * se
		)]
	}

	adjust_pvalues(agg_importance, p_adjust)
}

#' Nadeau & Bengio (2003) corrected variance estimation
#'
#' Corrected t-test accounting for correlation between resampling iterations.
#' Correction factor: (1/n_iters + n_test/n_train). SE = sqrt(factor * var(scores)).
#'
#' @param scores data.table with feature, importance, iter_rsmp columns
#' @param aggregator function to aggregate importance scores
#' @param conf_level confidence level for intervals
#' @param alternative "greater" (one-sided) or "two.sided"
#' @param resampling mlr3 Resampling object (for test/train ratio)
#' @param n_iters number of resampling iterations
#' @noRd
importance_nadeau_bengio = function(
	scores,
	aggregator,
	conf_level,
	alternative,
	resampling,
	n_iters,
	p_adjust = "none"
) {
	# The data.table NSE tax
	importance <- se <- statistic <- p.value <- NULL

	# Validate resampling type
	if (!(resampling$id %in% c("bootstrap", "subsampling")) | resampling$iters < 10) {
		cli::cli_warn(c(
			"Resampling is of type {.val {resampling$id}} with {.val {resampling$iters}} iterations.",
			i = "The Nadeau & Bengio corrected t-test is recommended for resampling types {.val {c('bootstrap', 'subsampling')}} with >= 10 iterations"
		))
	}

	if (resampling$id == "bootstrap") {
		test_train_ratio = 0.632
	} else {
		# Calculate test/train ratio for subsampling
		ratio = resampling$param_set$values$ratio
		n = resampling$task_nrow
		n1 = round(ratio * n)
		n2 = n - n1
		test_train_ratio = n2 / n1
	}

	# Nadeau & Bengio adjustment factor
	adjustment_factor = 1 / n_iters + test_train_ratio

	agg_importance = scores[,
		list(importance = aggregator(importance)),
		by = "feature"
	]

	# Aggregate within resamplings first
	means_rsmp = scores[,
		list(importance = mean(importance)),
		by = c("iter_rsmp", "feature")
	]

	sds = means_rsmp[,
		list(se = sqrt(adjustment_factor * stats::var(importance))),
		by = "feature"
	]

	agg_importance = agg_importance[sds, on = "feature"]

	# t-test: H0: importance = 0, se is corrected by Nadeau & Bengio factor
	df = n_iters - 1
	agg_importance[, statistic := importance / se]

	k = nrow(agg_importance)
	ci_alpha = adjust_ci_alpha(1 - conf_level, p_adjust, k)

	if (alternative == "greater") {
		agg_importance[, p.value := stats::pt(statistic, df = df, lower.tail = FALSE)]
		quant = stats::qt(1 - ci_alpha, df = df)
		agg_importance[, let(
			conf_lower = importance - quant * se,
			conf_upper = Inf
		)]
	} else {
		agg_importance[, p.value := 2 * stats::pt(abs(statistic), df = df, lower.tail = FALSE)]
		quant = stats::qt(1 - ci_alpha / 2, df = df)
		agg_importance[, let(
			conf_lower = importance - quant * se,
			conf_upper = importance + quant * se
		)]
	}

	adjust_pvalues(agg_importance, p_adjust)
}

#' Empirical quantile-based confidence intervals
#'
#' Non-parametric inference from resampling distribution. CIs from empirical quantiles.
#' Does not provide p-values or test statistics as these would require a different
#' methodology than the quantile-based CIs.
#'
#' @param scores data.table with feature, importance, iter_rsmp columns
#' @param aggregator function to aggregate importance scores
#' @param conf_level confidence level for intervals
#' @param alternative "greater" (one-sided) or "two.sided"
#' @noRd
importance_quantile = function(scores, aggregator, conf_level, alternative) {
	# The data.table NSE tax
	importance <- feature <- NULL

	# Aggregate within resamplings first to get one value per resampling iteration
	means_rsmp = scores[,
		list(importance = aggregator(importance)),
		by = c("iter_rsmp", "feature")
	]

	# For each feature, compute quantiles
	result_list = lapply(unique(means_rsmp$feature), function(feat) {
		feat_scores = means_rsmp[feature == feat, importance]

		# Point estimate using aggregator
		point_est = aggregator(feat_scores)

		# Compute empirical quantiles for CI
		alpha = 1 - conf_level
		if (alternative == "greater") {
			probs = alpha
			ci_lower = quantile(feat_scores, probs = probs, na.rm = TRUE)
			ci_upper = Inf
		} else {
			probs = c(alpha / 2, 1 - alpha / 2)
			ci_vals = quantile(feat_scores, probs = probs, na.rm = TRUE)
			ci_lower = ci_vals[1]
			ci_upper = ci_vals[2]
		}

		data.table(
			feature = feat,
			importance = point_est,
			conf_lower = ci_lower,
			conf_upper = ci_upper
		)
	})

	rbindlist(result_list)
}

#' Per-feature hypothesis tests on observation-wise importance scores
#'
#' Shared logic for CPI and Lei et al. inference. Runs a statistical test on the
#' observation-wise loss differences for each feature, computes p-values and CIs,
#' and applies multiplicity correction.
#'
#' SE is only reported for the t-test; for other tests it is `NA`.
#'
#' @param obs_loss_agg data.table with columns `feature`, `row_ids`, `obs_importance`
#'   (one row per observation per feature, already aggregated over repeats)
#' @param test "t", "wilcoxon", "fisher", or "binomial"
#' @param alternative "greater" or "two.sided"
#' @param conf_level confidence level for CI
#' @param p_adjust p-value adjustment method (any of stats::p.adjust.methods)
#' @param aggregator function to compute point estimate (default: mean)
#' @return data.table with feature, importance, se, statistic, p.value, conf_lower, conf_upper
#' @noRd
test_obs_importance = function(
	obs_loss_agg,
	test,
	alternative,
	conf_level,
	p_adjust,
	aggregator = mean
) {
	# The data.table NSE tax
	obs_importance <- feature <- NULL

	test_function = switch(
		test,
		t = stats::t.test,
		wilcoxon = stats::wilcox.test,
		fisher = fisher_test,
		binomial = binom_test
	)

	features_tested = unique(obs_loss_agg$feature)
	k = length(features_tested)
	ci_conf_level = 1 - adjust_ci_alpha(1 - conf_level, p_adjust, k)

	# SE is only meaningful for the t-test (sd / sqrt(n)).
	# For other tests (wilcoxon, fisher, binomial), SE is not well-defined.
	report_se = test == "t"

	result_list = lapply(features_tested, function(feat) {
		feat_obs = obs_loss_agg[feature == feat, obs_importance]

		if (mean(feat_obs) == 0) {
			if (alternative == "greater") {
				htest_result = list(
					statistic = 0,
					p.value = 1,
					conf.int = c(0, Inf)
				)
			} else {
				htest_result = list(
					statistic = 0,
					p.value = 1,
					conf.int = c(0, 0)
				)
			}
		} else {
			htest_result = test_function(
				feat_obs,
				alternative = alternative,
				conf.level = ci_conf_level,
				conf.int = TRUE
			)
		}

		data.table(
			feature = feat,
			importance = aggregator(feat_obs),
			se = if (report_se) sd(feat_obs) / sqrt(length(feat_obs)) else NA_real_,
			statistic = htest_result$statistic,
			p.value = htest_result$p.value,
			conf_lower = htest_result$conf.int[1],
			conf_upper = htest_result$conf.int[2]
		)
	})

	result = rbindlist(result_list, fill = TRUE)
	adjust_pvalues(result, p_adjust)
}

#' Conditional Predictive Impact (CPI) test
#'
#' Tests on observation-wise loss differences for CFI with knockoff samplers.
#' Available tests: t-test, Wilcoxon, Fisher permutation, binomial.
#' Fisher test uses Phipson & Smyth (2010) p-value correction.
#'
#' @param conf_level confidence level for CI
#' @param alternative "greater" (one-sided) or "two.sided"
#' @param test "t", "wilcoxon", "fisher", or "binomial"
#' @param p_adjust p-value adjustment method (any of stats::p.adjust.methods)
#' @param B number of replications for Fisher test
#' @param method_obj importance method object (needs $obs_loss())
#' @noRd
importance_cpi = function(
	conf_level,
	alternative = c("greater", "two.sided"),
	test = c("t", "wilcoxon", "fisher", "binomial"),
	p_adjust = "none",
	B = 1999,
	method_obj
) {
	# The data.table NSE tax
	N <- obs_importance <- feature <- NULL

	alternative = match.arg(alternative)

	# CPI requires observation-wise losses
	if (is.null(method_obj$obs_loss())) {
		cli::cli_abort(c(
			"CPI requires observation-wise losses.",
			i = "Ensure {.code measure} has an {.fun $obs_loss} method."
		))
	}
	if (class(method_obj)[[1]] != "CFI") {
		cli::cli_warn(c(
			"!" = "CPI is only known to yield valid inference for {.cls CFI}.",
			x = "Inference with {.cls PFI} is known to be invalid and other methods are not studied yet."
		))
	}
	# Get observation-wise importances (already computed as differences)
	obs_loss_data = method_obj$obs_loss(relation = "difference")
	check_unique_test_obs(obs_loss_data)
	# Aggregate over n_repeats to get one value per observation per feature
	obs_loss_agg = obs_loss_data[,
		list(obs_importance = mean(obs_importance)),
		by = c("row_ids", "feature")
	]

	test = match.arg(test)
	test_obs_importance(obs_loss_agg, test, alternative, conf_level, p_adjust)
}

#' Distribution-free inference for refit-based importance (Lei et al., 2018)
#'
#' Tests on observation-wise loss differences for WVIM/LOCO.
#' Available tests: Wilcoxon signed-rank (default), t-test, Fisher permutation, binomial.
#'
#' Lei et al. (2018) proposed this method specifically for LOCO with:
#' - L1 (absolute) loss as the measure
#' - Median as the aggregation function
#' - A test set where each observation appears at most once (e.g., holdout or CV)
#'
#' While the implementation generalizes these choices (allowing other measures,
#' aggregators, and resampling strategies), deviating from the original proposal
#' may invalidate the theoretical guarantees.
#'
#' @param conf_level confidence level for CI
#' @param alternative "greater" (one-sided) or "two.sided"
#' @param test "wilcoxon", "t", "fisher", or "binomial"
#' @param aggregator function to compute point estimate from obs_importance (default: median)
#' @param p_adjust p-value adjustment method (any of stats::p.adjust.methods)
#' @param B number of replications for Fisher test
#' @param method_obj importance method object (needs $obs_loss())
#' @noRd
importance_loco = function(
	conf_level,
	alternative = c("greater", "two.sided"),
	test = c("wilcoxon", "t", "fisher", "binomial"),
	aggregator = stats::median,
	p_adjust = "none",
	B = 1999,
	method_obj
) {
	# The data.table NSE tax
	N <- obs_importance <- feature <- NULL

	alternative = match.arg(alternative)

	# Lei et al. inference requires observation-wise losses
	if (is.null(method_obj$obs_loss())) {
		cli::cli_abort(c(
			"Lei et al. (2018) inference requires observation-wise losses.",
			i = "Ensure {.code measure} has an {.fun $obs_loss} method (i.e. is decomposable)."
		))
	}

	# Get observation-wise importances (already computed as differences)
	obs_loss_data = method_obj$obs_loss(relation = "difference")

	check_unique_test_obs(obs_loss_data)

	# Aggregate over n_repeats (iter_repeat) to get one value per observation per feature
	obs_loss_agg = obs_loss_data[,
		list(obs_importance = mean(obs_importance)),
		by = c("row_ids", "feature")
	]

	test = match.arg(test)
	test_obs_importance(obs_loss_agg, test, alternative, conf_level, p_adjust, aggregator)
}

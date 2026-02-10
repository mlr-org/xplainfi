# Importance Aggregation Methods ----
# These functions provide different methods for aggregating importance scores
# across resampling iterations and computing confidence intervals.

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
importance_raw = function(scores, aggregator, conf_level, alternative, n_iters) {
	# The data.table NSE tax
	importance <- se <- statistic <- NULL
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

	alpha = 1 - conf_level
	if (alternative == "greater") {
		agg_importance[, p.value := stats::pt(statistic, df = df, lower.tail = FALSE)]
		quant = stats::qt(1 - alpha, df = df)
		agg_importance[, let(
			conf_lower = importance - quant * se,
			conf_upper = Inf
		)]
	} else {
		agg_importance[, p.value := 2 * stats::pt(abs(statistic), df = df, lower.tail = FALSE)]
		quant = stats::qt(1 - alpha / 2, df = df)
		agg_importance[, let(
			conf_lower = importance - quant * se,
			conf_upper = importance + quant * se
		)]
	}

	agg_importance
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
	n_iters
) {
	# The data.table NSE tax
	importance <- se <- statistic <- NULL

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

	alpha = 1 - conf_level
	if (alternative == "greater") {
		agg_importance[, p.value := stats::pt(statistic, df = df, lower.tail = FALSE)]
		quant = stats::qt(1 - alpha, df = df)
		agg_importance[, let(
			conf_lower = importance - quant * se,
			conf_upper = Inf
		)]
	} else {
		agg_importance[, p.value := 2 * stats::pt(abs(statistic), df = df, lower.tail = FALSE)]
		quant = stats::qt(1 - alpha / 2, df = df)
		agg_importance[, let(
			conf_lower = importance - quant * se,
			conf_upper = importance + quant * se
		)]
	}

	agg_importance
}

#' Empirical quantile-based confidence intervals and p-values
#'
#' Non-parametric inference from resampling distribution. CIs from empirical quantiles.
#' P-value uses Phipson & Smyth (2010) formula: (b + 1) / (n + 1) where b is the count
#' of iterations with importance <= 0 (one-sided) or |importance| >= |observed| (two-sided).
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

	# For each feature, compute quantiles and empirical p-values
	result_list = lapply(unique(means_rsmp$feature), function(feat) {
		feat_scores = means_rsmp[feature == feat, importance]
		n = length(feat_scores)

		# Point estimate using aggregator
		point_est = aggregator(feat_scores)

		# Empirical SE
		se = sd(feat_scores) / sqrt(n)

		# Empirical test statistic (z-score like)
		statistic = point_est / se

		# Empirical p-value based on resampling distribution
		if (alternative == "greater") {
			# Proportion of iterations with importance <= 0
			p.value = (sum(feat_scores <= 0) + 1) / (n + 1)
		} else {
			# Two-sided: proportion of iterations at least as extreme
			p.value = (sum(abs(feat_scores) >= abs(point_est)) + 1) / (n + 1)
		}

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
			se = se,
			statistic = statistic,
			p.value = p.value,
			conf_lower = ci_lower,
			conf_upper = ci_upper
		)
	})

	rbindlist(result_list)
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
#' @param B number of replications for Fisher test
#' @param method_obj importance method object (needs $obs_loss())
#' @noRd
importance_cpi = function(
	conf_level,
	alternative = c("greater", "two.sided"),
	test = c("t", "wilcoxon", "fisher", "binomial"),
	B = 1999,
	method_obj
) {
	# The data.table NSE tax
	N <- obs_importance <- feature <- sd <- NULL

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
	# We need **at most** one row per feature and row_id for valid inference
	# so we aggregate over iter_rsmp
	dupes = obs_loss_data[, .N, by = c("feature", "row_ids", "iter_repeat")][N > 1]

	if (nrow(dupes) >= 1) {
		cli::cli_warn(c(
			"Resampling is of type {.val {method_obj$resampling$id}} with {.val {method_obj$resampling$iters}} iterations.",
			"Found {.val {length(unique(dupes[, row_ids]))}} duplicated observation{?s} in test sets",
			x = "Confidence intervals will have wrong coverage!",
			i = "CPI requires each observation to appear {.emph at most once} in the test set(s)",
			i = "Use holdout resampling to ensure valid inference"
		))
	}
	# Aggregating here over n_repeats
	obs_loss_agg = obs_loss_data[,
		list(obs_importance = mean(obs_importance)),
		by = c("row_ids", "feature")
	]

	test = match.arg(test)
	test_function = switch(
		test,
		t = stats::t.test,
		wilcoxon = stats::wilcox.test,
		fisher = fisher_test,
		binomial = binom_test
	)

	result_list = lapply(unique(obs_loss_agg$feature), function(feat) {
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
				conf.level = conf_level,
				conf.int = TRUE
			)
		}

		data.table(
			feature = feat,
			importance = mean(feat_obs),
			se = sd(feat_obs) / sqrt(length(feat_obs)),
			statistic = htest_result$statistic,
			p.value = htest_result$p.value,
			conf_lower = htest_result$conf.int[1],
			conf_upper = htest_result$conf.int[2]
		)
	})

	rbindlist(result_list, fill = TRUE)
}

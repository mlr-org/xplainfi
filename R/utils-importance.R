# Importance Aggregation Methods ----
# These functions provide different methods for aggregating importance scores
# across resampling iterations and computing confidence intervals.

#' No variance estimation - just aggregated performance
#' @param scores ([data.table::data.table]) with feature and importance columns
#' @param aggregator (`function`) to aggregate importance scores
#' @param conf_level (`numeric(1)`) ignored for this method
#' @noRd
importance_none = function(scores, aggregator, conf_level) {
	agg_importance = scores[,
		list(importance = aggregator(importance)),
		by = feature
	]
	agg_importance
}

#' Raw variance estimation without correction (too narrow CIs)
#' @param scores ([data.table::data.table]) with feature and importance columns, must include iter_rsmp
#' @param aggregator (`function`) to aggregate importance scores
#' @param conf_level (`numeric(1)`) confidence level for intervals
#' @param n_iters (`integer(1)`) number of resampling iterations
#' @noRd
importance_raw = function(scores, aggregator, conf_level, n_iters) {
	agg_importance = scores[,
		list(importance = aggregator(importance)),
		by = feature
	]

	# Aggregate within resamplings first to get one row per resampling iter
	means_rsmp = scores[,
		list(importance = mean(importance)),
		by = c("iter_rsmp", "feature")
	]

	sds = means_rsmp[,
		list(se = sqrt(var(importance) / n_iters)),
		by = feature
	]

	agg_importance = agg_importance[sds, on = "feature"]

	alpha = 1 - conf_level
	quant = qt(1 - alpha / 2, df = n_iters - 1)

	agg_importance[, let(
		conf_lower = importance - quant * se,
		conf_upper = importance + quant * se
	)]

	agg_importance
}

#' Nadeau & Bengio (2003) corrected variance estimation
#' @param scores ([data.table::data.table]) with feature and importance columns, must include iter_rsmp
#' @param aggregator (`function`) to aggregate importance scores
#' @param conf_level (`numeric(1)`) confidence level for intervals
#' @param resampling ([mlr3::Resampling]) resampling object
#' @param n_iters (`integer(1)`) number of resampling iterations
#' @noRd
importance_nadeau_bengio = function(scores, aggregator, conf_level, resampling, n_iters) {
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
		by = feature
	]

	# Aggregate within resamplings first
	means_rsmp = scores[,
		list(importance = mean(importance)),
		by = c("iter_rsmp", "feature")
	]

	sds = means_rsmp[,
		list(se = sqrt(adjustment_factor * var(importance))),
		by = feature
	]

	agg_importance = agg_importance[sds, on = "feature"]

	alpha = 1 - conf_level
	quant = qt(1 - alpha / 2, df = n_iters - 1)

	agg_importance[, let(
		conf_lower = importance - quant * se,
		conf_upper = importance + quant * se
	)]

	agg_importance
}

#' Empirical quantile-based confidence intervals
#' Uses quantile() to construct confidence-like intervals from resampling distribution
#' @param scores ([data.table::data.table]) with feature and importance columns, must include iter_rsmp
#' @param aggregator (`function`) to aggregate importance scores (used for point estimate)
#' @param conf_level (`numeric(1)`) confidence level for intervals
#' @noRd
importance_quantile = function(scores, aggregator, conf_level) {
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
		probs = c(alpha / 2, 1 - alpha / 2)
		ci_vals = quantile(feat_scores, probs = probs, na.rm = TRUE)

		data.table(
			feature = feat,
			importance = point_est,
			conf_lower = ci_vals[1],
			conf_upper = ci_vals[2]
		)
	})

	rbindlist(result_list)
}

#' Conditional Predictive Impact (CPI) using one-sided t-test
#' CPI is specifically designed for CFI with knockoff samplers
#' Based on Watson et al. (2021) and implemented in the cpi package
#' @param conf_level (`numeric(1)`) confidence level for one-sided CI
#' @param test (`character(1)`) Type of test to perform. Fisher is recommended
#' @param B (`integer(1)`) Number of replications for Fisher test
#' @param method_obj feature importance method object (needs $obs_loss(), $resampling, class info)
#' @noRd
importance_cpi = function(
	conf_level,
	test = c("t", "wilcoxon", "fisher", "binomial"),
	B = 1999,
	method_obj
) {
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
		fisher = fisher_one_sided,
		binomial = binom_one_sided
	)
	# For each feature, perform one-sided test (alternative = "greater")
	# H0: importance <= 0, H1: importance > 0
	result_list = lapply(unique(obs_loss_agg$feature), function(feat) {
		feat_obs = obs_loss_agg[feature == feat, obs_importance]

		if (mean(feat_obs) == 0) {
			htest_result = list(
				estimate = 0,
				statistic = 0,
				p.value = 1,
				conf.int = 0
			)
		} else {
			# One-sided test
			htest_result = test_function(
				feat_obs,
				alternative = "greater",
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
			conf_upper = Inf # One-sided test upper bound is infinity
		)
	})

	rbindlist(result_list, fill = TRUE)
}

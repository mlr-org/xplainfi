#' Fisher permutation test wrapper
#' Based on cpi pkg, extended to support two-sided tests
#' @param x numeric vector of observations
#' @param B number of permutations
#' @param alternative "greater" or "two.sided"
#' @param conf.level confidence level
#' @param ... ignored
#' @noRd
fisher_test = function(
	x,
	B = 1999,
	alternative = c("greater", "two.sided"),
	conf.level = 0.95,
	...
) {
	alternative = match.arg(alternative)
	orig_mean = mean(x)

	# B permutations
	perm_means = replicate(B, {
		signs = sample(c(-1, 1), length(x), replace = TRUE)
		mean(signs * x)
	})

	if (alternative == "greater") {
		p.value = (sum(perm_means >= orig_mean) + 1) / (B + 1)
		conf.int = c(orig_mean - quantile(perm_means, conf.level), Inf)
	} else {
		# two.sided
		p.value = (sum(abs(perm_means) >= abs(orig_mean)) + 1) / (B + 1)
		alpha = 1 - conf.level
		conf.int = orig_mean - quantile(perm_means, c(1 - alpha / 2, alpha / 2))
	}

	list(
		statistic = orig_mean,
		p.value = p.value,
		conf.int = conf.int
	)
}

#' Binomial test wrapper
#' Based on cpi pkg, extended to support two-sided tests
#' @param x numeric vector of observations
#' @param alternative "greater" or "two.sided"
#' @param conf.level confidence level
#' @param ... ignored
#' @noRd
binom_test = function(x, alternative = c("greater", "two.sided"), conf.level = 0.95, ...) {
	alternative = match.arg(alternative)
	stats::binom.test(sum(x > 0), length(x), alternative = alternative, conf.level = conf.level)
}

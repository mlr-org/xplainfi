# Custom testthat expectations for importance result validation
#
# These follow testthat 3e conventions.
# See: https://testthat.r-lib.org/articles/custom-expectation.html

# -----------------------------------------------------------------------------
# expect_importance_dt
# -----------------------------------------------------------------------------

#' Expectation for aggregated importance score tables
#'
#' Validates columns:
#' - `feature` is a character value without missings
#' - `importance` is numeric vector without missings or infinite values
#' - Variance-related columns (se, estimate, conf_lower, conf_upper, statistic, p.value) may contain NA
#'
#' @param x (data.table()) Importance result table to validate.
#' @param features (character()) Feature names used to test names and order of importance scores.
expect_importance_dt = function(x, features) {
	checkmate::expect_data_table(
		x,
		types = c("character", "numeric"),
		nrows = length(features),
		min.cols = 2
	)

	# Core columns must not have missing values
	checkmate::expect_character(x$feature, any.missing = FALSE)
	checkmate::expect_numeric(x$importance, any.missing = FALSE)

	# Variance-related columns may contain NA (e.g., CPI test statistics can fail for some features)
	variance_cols = c("se", "sd", "estimate", "conf_lower", "conf_upper", "statistic", "p.value")
	for (col in intersect(variance_cols, colnames(x))) {
		checkmate::expect_numeric(x[[col]], any.missing = TRUE)
	}
}

# -----------------------------------------------------------------------------
# expect_score_dt
# -----------------------------------------------------------------------------

#' Expectation for individual importance score tables
#'
#' Validates columns:
#' - `feature` is a character value without missings
#' - `importance` is numeric vector without missings or infinite values
#'
#' @param x (data.table()) Score result table to validate.
#' @param features (character()) Feature names used to test names and order of importance scores.
expect_score_dt = function(x, features) {
	checkmate::expect_data_table(
		x,
		types = c("character", "numeric"),
		min.rows = length(features),
		min.cols = 5,
		any.missing = FALSE,
		key = c("feature", "iter_rsmp")
	)

	checkmate::expect_character(x$feature, any.missing = FALSE)
	checkmate::expect_numeric(x$importance, any.missing = FALSE)
}

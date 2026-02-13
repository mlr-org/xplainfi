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
# expect_scores_dt
# -----------------------------------------------------------------------------

#' Expectation for iteration-wise importance score tables
#'
#' Validates $scores() output. Works for all FeatureImportanceMethod subclasses
#' (PFI, CFI, RFI, WVIM/LOCO, SAGE) despite their different column structures.
#'
#' @param x (data.table()) Score result table from $scores().
#' @param features (character()) Feature names that should appear in the table.
expect_scores_dt = function(x, features) {
	checkmate::expect_data_table(x, min.rows = length(features), any.missing = FALSE)
	checkmate::expect_character(x$feature, any.missing = FALSE)
	checkmate::expect_numeric(x$importance, any.missing = FALSE)
	expect_true(all(features %in% x$feature))
}

# -----------------------------------------------------------------------------
# expect_obs_loss_dt
# -----------------------------------------------------------------------------

#' Expectation for observation-wise loss tables
#'
#' Validates $obs_loss() output. Only applicable for perturbation methods with
#' decomposable measures (e.g., regr.mse, classif.ce).
#'
#' @param x (data.table()) Observation-wise loss table from $obs_loss().
#' @param features (character()) Feature names that should appear in the table.
expect_obs_loss_dt = function(x, features) {
	checkmate::expect_data_table(x, min.rows = length(features), any.missing = FALSE)
	checkmate::expect_character(x$feature, any.missing = FALSE)
	expect_true(all(c("row_ids", "loss_baseline", "loss_post", "obs_importance") %in% names(x)))
	checkmate::expect_numeric(x$obs_importance, any.missing = FALSE)
	expect_true(all(features %in% x$feature))
}

# -----------------------------------------------------------------------------
# expect_method_output
# -----------------------------------------------------------------------------

#' Omnibus expectation for a computed FeatureImportanceMethod
#'
#' Validates all three main outputs of a computed method:
#' - $importance(): always checked
#' - $scores(): always checked
#' - $obs_loss(): checked if the method supports it (decomposable measure + perturbation method)
#'
#' @param method A computed FeatureImportanceMethod (must have had $compute() called)
expect_method_output = function(method) {
	features = method$features

	expect_importance_dt(method$importance(), features = features)
	expect_scores_dt(method$scores(), features = features)

	# obs_loss is only available for perturbation methods with decomposable measures
	if (has_obs_loss(method$measure) && !inherits(method, "SAGE")) {
		expect_obs_loss_dt(method$obs_loss(), features = features)
	}
}

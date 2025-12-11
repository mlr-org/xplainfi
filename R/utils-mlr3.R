#' Check if a measure has an obs_loss
#' @noRd
#' @keywords internal
#'
#' @return logical(1)
has_obs_loss <- function(x) {
	res = FALSE
	if (inherits(x, "R6") && inherits(x, "Measure")) {
		if (utils::packageVersion("mlr3") >= package_version("1.3.0")) {
			res = "obs_loss" %in% x$properties
		} else {
			res = !is.null(x$obs_loss)
		}
	} else if (is.character(x)) {
		if (x %in% mlr3::mlr_measures$keys()) {
			res = has_obs_loss(mlr3::msr(x))
		}
	}

	isTRUE(res)
}

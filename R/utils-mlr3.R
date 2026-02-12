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

#' Check if a learner can be considered pretrained
#'
#' Returns `TRUE` if the learner has a model and the resampling is a compatible
#' single-iteration setup. Returns `FALSE` for untrained learners.
#' Errors if the learner is trained but the resampling is incompatible
#' (multi-fold or mismatched row IDs).
#'
#' Assumes `resampling` is already instantiated (enforced by
#' `FeatureImportanceMethod$initialize()` before this is called).
#'
#' @param learner,task,resampling ([mlr3::Learner], [mlr3::Task], [mlr3::Resampling])
#'
#' @return `logical(1)`: `TRUE` if pretrained and compatible, `FALSE` if untrained
#' @keywords internal
#' @noRd
assert_pretrained = function(learner, task, resampling) {
	# Untrained learner -> not pretrained, nothing to validate
	if (is.null(learner$model)) {
		return(FALSE)
	}

	# Learner is trained: resampling must have exactly 1 iteration
	if (resampling$iters != 1L) {
		cli::cli_abort(c(
			"Given {.code resampling} is not compatible with using a pre-trained {.cls Learner}",
			i = "If {.code learner} is pre-trained, {.code resampling} must have exactly 1 iteration (e.g. holdout)"
		))
	}

	# Resampling test row IDs must be a subset of task row IDs
	if (length(setdiff(resampling$test_set(1), task$row_ids)) > 0) {
		cli::cli_abort(c(
			"Provided {.code task} has row_ids not compatible with provided {.code resampling}",
			i = "Make sure {.code resampling} was instantiated on the correct {.code task}"
		))
	}

	TRUE
}

#' Create ResampleResult object
#'
#' @param task ([mlr3::Task])
#' @param learner ([mlr3::Learner]) Either untrained or trained learner. If trained, the `ResampleResult` will be constructed manually, otherwise [mlr3::resample()] is used.
#' @param resampling ([mlr3::Resampling]) If `learner` is trained, this must be an instantiated, single-iteration `Resampling`.
#' @param store_models,store_backends Passed to [mlr3::resample()] or [mlr3::as_resample_result()] respectively.
#'
#' @importFrom mlr3 as_resample_result
#' @noRd
#' @keywords internal
assemble_rr = function(
	task,
	learner,
	resampling,
	store_models = TRUE,
	store_backends = TRUE
) {
	if (assert_pretrained(learner = learner, task = task, resampling = resampling)) {
		if (xplain_opt("debug")) {
			cli::cli_alert_info("Using pretrained learner")
		}
		pred = learner$predict(task, row_ids = resampling$test_set(1))

		# Clone learner: as_resample_result() clones internally but resets the model
		# on the object it receives, which would wipe the user's original via R6 reference
		mlr3::as_resample_result(
			x = list(list(test = pred)),
			task = task,
			learners = list(learner$clone()),
			resampling = resampling
		)
	} else {
		if (xplain_opt("debug")) {
			cli::cli_alert_info("Using {.fun resample}")
		}
		resample(
			task,
			learner,
			resampling,
			store_models = store_models,
			store_backends = store_backends
		)
	}
}

#' Create a resampling with all data being test data
#'
#' Utility for use with a pretrained learner in importance methods which support it
#'
#' Note that the resulting Resampling will have an
#' empty train set, making it useless for any
#' other purpose than the use with a pretrained learner.
#' @param task ([mlr3::Task])
#' @return [mlr3::Resampling] with an empty `train_set` and a single `test_set` identical to all of the given `Task`.
#' @export
#' @examples
#' library(mlr3)
#' # Create custom task from some data.frame
#' custom_task <- as_task_regr(mtcars, target = "mpg")
#' # Create matching Resampling with all-test data
#' resampling_custom <- rsmp_all_test(custom_task)
rsmp_all_test = function(task) {
	mlr3::assert_task(task)

	rsmp("custom")$instantiate(
		task,
		train_sets = list(integer(0)),
		test_sets = list(task$row_ids)
	)
}

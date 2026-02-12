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
#' Also checks if resampling is single-iteration and instatiated
#' and the task row_ids are compatible with the resampling.
#' The task check `mlr3::assert_learnable(learner, task)` is assumed to have passed.
#'
#' @param learner,task,resampling ([mlr3::Learner], [mlr3::Task], [mlr3::Resampling])
#'
#' @return `logical(1)`: `TRUE` if the learner is trained and task and resampling are compatible
#' @keywords internal
#' @noRd
#'
is_pretrained = function(learner, task, resampling) {
	# Learner must be trained -> must have stored model
	learner_ok = !is.null(learner$model)
	# Early return if learner isn't trained anyway
	if (!learner_ok) {
		return(FALSE)
	}
	# Resampling must be instantiated and have 1 iteration / 1 test set
	resampling_ok = resampling$is_instantiated & (resampling$iters == 1)

	# Resampling test row IDs must be compatible with task row IDs
	# (not fool proof but would be an obvious issue)
	if (resampling_ok) {
		task_ok = length(setdiff(resampling$test_set(1), task$row_ids)) == 0
	}

	if (learner_ok & !resampling_ok) {
		cli::cli_abort(c(
			"Given {.code resampling} is not compatible with using a pre-trained {.cls Learner}",
			i = "If {.code learner} is pre-trained, {.code resampling} must be instantiated and have exactly 1 iteration"
		))
	}

	if (!task_ok) {
		cli::cli_abort(c(
			"Provided {.code task} has row_ids not compatible with provided {.code resampling}",
			i = "Make sure {.code resampling} was instantiated on the correct {.code task}"
		))
	}

	learner_ok & (resampling_ok & task_ok)
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
	if (is_pretrained(learner = learner, task = task, resampling = resampling)) {
		if (xplain_opt("debug")) {
			cli::cli_alert_info("Using pretrained learner")
		}
		pred = learner$predict(task, row_ids = resampling$test_set(1))

		# Clone learner: as_resample_result() clones internally but resets the model
		# on the object it receives, which would wipe the user's original via R6 reference
		resample_result = mlr3::as_resample_result(
			x = list(list(test = pred)),
			task = task,
			learners = list(learner$clone()),
			resampling = resampling
		)
	} else {
		if (xplain_opt("debug")) {
			cli::cli_alert_info("Using {.fun resample}")
		}
		resample_result = resample(
			task,
			learner,
			resampling,
			store_models = store_models,
			store_backends = store_backends
		)
	}
}

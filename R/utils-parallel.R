#' Parallel map function for xplainfi
#'
#' Unified parallelization interface supporting mirai, future, and sequential execution.
#' Follows the mlr3::future_map pattern for consistency.
#'
#' @param n Number of iterations (length of first varying argument)
#' @param .f Function to apply
#' @param ... Varying arguments to iterate over (passed to mapply)
#' @param .args Named list of fixed arguments passed to all iterations
#'
#' @return List of results from applying .f
#'
#' @details
#' Execution mode is determined automatically:
#' 1. If `getOption("xplainfi.sequential")` is TRUE, runs sequentially with mapply
#' 2. If mirai daemons are set, uses mirai_map for parallel execution
#' 3. If future plan is non-sequential, uses future.apply::future_mapply
#' 4. Otherwise, uses base mapply for sequential execution
#'
#' For mirai execution, required packages are loaded automatically based on context.
#'
#' @keywords internal
#' @noRd
xplainfi_map = function(n, .f, ..., .args = list()) {
	# Debug mode: sequential execution
	if (xplain_opt("sequential")) {
		return(mapply(.f, ..., MoreArgs = .args, SIMPLIFY = FALSE, USE.NAMES = TRUE))
	}

	# Mirai parallelization
	# Use same option as mlr3 for consistency (WVIM uses mlr3fselect which uses mlr3's parallelization)
	if (
		requireNamespace("mirai", quietly = TRUE) &&
			mirai::daemons_set(.compute = getOption("mlr3.mirai_parallelization", "mlr3_parallelization"))
	) {
		# Capture names from first varying argument
		varying_args = list(...)
		result_names = if (length(varying_args) > 0) names(varying_args[[1]]) else NULL

		# Create data.table from varying arguments
		# Ensure first column name matches first parameter of .f for named argument passing
		varying_dt = data.table::data.table(...)
		first_param = names(formals(.f))[1]
		data.table::setnames(varying_dt, 1, first_param)

		result = mirai::mirai_map(
			varying_dt,
			.f,
			.args = c(.args, list(is_sequential = FALSE)),
			.compute = getOption("mlr3.mirai_parallelization", "mlr3_parallelization")
		)

		collected = mirai::collect_mirai(result)

		# Check for errors in parallel execution
		has_errors = vapply(collected, inherits, logical(1), "miraiError")
		if (any(has_errors)) {
			error_msgs = collected[has_errors]
			error_indices = which(has_errors)
			cli::cli_abort(c(
				"Errors occurred in mirai worker processes:",
				"x" = "{length(error_indices)} task{?s} failed: {.val {error_indices}}",
				"i" = "First error: {as.character(error_msgs[[1]])}"
			))
		}

		# Restore names if they existed
		if (!is.null(result_names)) {
			names(collected) = result_names
		}

		return(collected)
	}

	# Future parallelization
	if (requireNamespace("future", quietly = TRUE)) {
		is_sequential = inherits(future::plan(), "sequential")

		if (!is_sequential) {
			# Get chunk size settings
			chunk_size = getOption("xplainfi.exec_chunk_size", 1)
			chunk_bins = getOption("xplainfi.exec_chunk_bins")
			if (!is.null(chunk_bins)) {
				chunk_size = ceiling(n / chunk_bins)
			}

			# Random scheduling for load balancing
			scheduling = if (isTRUE(getOption("xplainfi.exec_random", TRUE))) {
				structure(TRUE, ordering = "random")
			} else {
				TRUE
			}

			.args_future = c(.args, list(is_sequential = FALSE))

			return(future.apply::future_mapply(
				.f,
				...,
				MoreArgs = .args_future,
				SIMPLIFY = FALSE,
				USE.NAMES = FALSE,
				future.globals = FALSE,
				future.packages = character(0), # Packages loaded explicitly in worker
				future.seed = TRUE,
				future.scheduling = scheduling,
				future.chunk.size = chunk_size,
				future.stdout = TRUE
			))
		}
	}

	# Sequential fallback
	mapply(.f, ..., MoreArgs = .args, SIMPLIFY = FALSE, USE.NAMES = TRUE)
}

#' Worker preamble: package loads + optional ARF doParallel registration
#'
#' Centralizes setup that `xplainfi_map` workers need before running
#' user code. When `is_sequential` is TRUE this is a no-op (the
#' sequential fallback runs in the main process and inherits state).
#' When the sampler carries `parallel = TRUE`, registers a doParallel
#' cluster inside this worker and installs a cleanup handler on the
#' CALLER's `on.exit` so the cluster is stopped when the calling
#' worker function returns.
#'
#' @param learner_packages (`character`) Packages the learner needs.
#' @param sampler ([FeatureSampler] | `NULL`) Sampler; if it carries
#'   `parallel = TRUE`, register doParallel inside this worker.
#' @param arf_workers (`integer(1)`) Workers for the ARF doParallel
#'   cluster, resolved caller-side via `xplain_opt("arf_workers")`
#'   (mirai daemons do not inherit options).
#' @param is_sequential (`logical(1)`) If TRUE, no-op.
#' @return Invisibly NULL.
#' @keywords internal
#' @noRd
xplain_worker_preamble = function(learner_packages, sampler, arf_workers, is_sequential) {
	if (is_sequential) {
		return(invisible(NULL))
	}
	library("data.table")
	library("mlr3")
	library("xplainfi")
	for (pkg in learner_packages) {
		library(pkg, character.only = TRUE)
	}
	if (
		!is.null(sampler) &&
			isTRUE(sampler$param_set$values$parallel) &&
			arf_workers > 0L
	) {
		require_package("doParallel")
		doParallel::registerDoParallel(cores = arf_workers)
		do.call(
			"on.exit",
			list(quote(doParallel::stopImplicitCluster()), add = TRUE),
			envir = parent.frame()
		)
	}
	invisible(NULL)
}

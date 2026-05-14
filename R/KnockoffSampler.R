#' @title Knockoff Sampler
#'
#' @description Implements conditional sampling using Knockoffs.
#'
#' @details
#' The `KnockoffSampler` samples [Knockoffs][knockoff::knockoff] based on the task data.
#' This class allows arbitrary `knockoff_fun`, which also means that no input checking
#' against supported feature types can be done. Use [KnockoffGaussianSampler] for the
#' Gaussian knockoff sampler for numeric features.
#' Alternative knockoff samplers include `knockoff_seq()` from the `seqknockoff` package
#' available on GitHub: <https://github.com/kormama1/seqknockoff>.
#'
#' Knockoffs are related to the `ConditionalSampler` familty, with key differences:
#' They do not allow specifying a `conditioning_set`
#'
#' @examplesIf requireNamespace("knockoff", quietly = TRUE)
#' library(mlr3)
#' task = tgen("2dnormals")$generate(n = 100)
#' # Create sampler with default parameters
#' sampler = KnockoffSampler$new(task)
#' # Sample using row_ids from stored task
#' sampled_data = sampler$sample("x1")
#'
#' @references `r print_bib("watson_2021", "blesch_2023")`
#' @export
KnockoffSampler = R6Class(
	"KnockoffSampler",
	inherit = FeatureSampler,
	public = list(
		#' @field x_tilde Knockoff matrix with one (or `iters`) row(s) per original observation in `task`.
		x_tilde = NULL,
		#' @description
		#' Creates a new instance of the KnockoffSampler class.
		#' @param task ([mlr3::Task]) Task to sample from
		#' @param knockoff_fun (`function`) Function used to create knockoff matrix. Default are second-order Gaussian knockoffs (`knockoff::create.second_order()`)
		#' @param iters (`integer(1)`: `1`) Number of repetitions the `knockoff_fun` is applied to create multiple `x_tilde`
		#' instances per observation.
		initialize = function(
			task,
			knockoff_fun = function(x) knockoff::create.second_order(as.matrix(x)),
			iters = 1
		) {
			super$initialize(task)
			self$label = "Knockoff sampler"

			require_package("knockoff")

			# Override param_set to include Knockoff-specific parameters
			self$param_set = paradox::ps(
				# conditioning_set = paradox::p_uty(default = NULL),
				knockoff_fun = paradox::p_uty(
					default = function(x) knockoff::create.second_order(as.matrix(x)),
					custom_check = function(x) {
						if (is.function(x)) TRUE else "knockoff_fun must be a function."
					}
				),
				iters = paradox::p_int(lower = 1, default = 1)
			)

			# Set parameter values
			values_to_set = list()
			values_to_set$knockoff_fun = knockoff_fun
			values_to_set$iters = iters
			self$param_set$set_values(.values = values_to_set)

			# Create knockoff matrix, features only
			# No assertions here on feature types, the user has been warned in the doc
			if (iters == 1) {
				self$x_tilde = as.data.table(knockoff_fun(self$task$data(cols = self$task$feature_names)))
				self$x_tilde[, ..row_id := self$task$row_ids]
			} else {
				self$x_tilde = rbindlist(replicate(
					iters,
					{
						x_tilde = as.data.table(knockoff_fun(self$task$data(cols = self$task$feature_names)))
						x_tilde[, ..row_id := self$task$row_ids]
					},
					simplify = FALSE
				))
			}

			checkmate::assert_subset(colnames(self$x_tilde), c(self$task$feature_names, "..row_id"))
			checkmate::assert_true(nrow(self$x_tilde) == (iters * self$task$nrow))
		},

		#' @description
		#' Sample from stored task using knockoff values. Replaces specified feature(s) with
		#' their knockoff counterparts from the pre-generated knockoff matrix.
		#' @param feature (`character`) Feature(s) to sample.
		#' @param row_ids (`integer()` | `NULL`) Row IDs to use. If `NULL`, uses all rows.
		#' @param samples_per_row (`integer(1)`: `1L`) Number of independent samples per input row.
		#'   See [FeatureSampler]`$sample()` for output shape and ordering.
		#' @return Modified copy with knockoff feature(s).
		sample = function(
			feature,
			row_ids = NULL,
			samples_per_row = 1L
		) {
			checkmate::assert_count(samples_per_row, positive = TRUE)
			if (is.null(row_ids)) row_ids = self$task$row_ids
			iters = self$param_set$values$iters

			replace = samples_per_row > iters
			if (replace) {
				cli::cli_warn(c(
					"!" = "Some instances requested more often than they are present in generated knockoff matrix",
					i = "Will sample with replacement, so some knockoff values will be duplicated",
					i = "Create {.cls {class(self)[[1L]]}} with {.code iters = {samples_per_row}} or higher to prevent this"
				))
			}

			data_copy = private$.get_task_data_by_row_id(row_ids)

			out = data_copy[rep.int(seq_len(.N), times = samples_per_row)]

			# Pick a knockoff iter index per draw. Without replacement when possible
			# (samples_per_row <= iters); with replacement otherwise (warned above).
			iter_idx_per_draw = if (replace) {
				sample.int(iters, samples_per_row, replace = TRUE)
			} else {
				sample.int(iters, samples_per_row, replace = FALSE)
			}

			# Collect per-draw feature blocks then assign in bulk so column type
			# from x_tilde (e.g. numeric) replaces any incompatible task type
			# (e.g. integer) without triggering data.table truncation warnings.
			feat_blocks = lapply(seq_len(samples_per_row), function(d) {
				iter_i = iter_idx_per_draw[d]
				# x_tilde is `iters` knockoff matrices stacked iter-major: rows
				# ((i-1)*task$nrow + 1) .. (i*task$nrow) belong to iter i.
				iter_block = self$x_tilde[((iter_i - 1L) * self$task$nrow + 1L):(iter_i * self$task$nrow)]
				row_idx_in_block = match(row_ids, iter_block[["..row_id"]])
				iter_block[row_idx_in_block, .SD, .SDcols = feature]
			})
			feat_values = data.table::rbindlist(feat_blocks)
			out[, (feature) := feat_values]

			out = private$.ensure_feature_types(out)
			setcolorder(out, self$task$feature_names)
			out[, .SD, .SDcols = c(self$task$target_names, self$task$feature_names)]
		}
	)
)


#' @title Gaussian Knockoff Conditional Sampler
#'
#' @description
#' A [KnockoffSampler] defaulting to second-order Gaussian knockoffs
#' as created by [knockoff::create.second_order].
#'
#' @details
#' This is equivalent to [KnockoffSampler] using the default `knockoff_fun`.
#'
#' @examplesIf requireNamespace("knockoff", quietly = TRUE)
#' library(mlr3)
#' task = tgen("2dnormals")$generate(n = 100)
#' # Create sampler
#' sampler = KnockoffGaussianSampler$new(task)
#' # Sample using row_ids from stored task
#' sampled_data = sampler$sample("x1")
#' @references `r print_bib("watson_2021", "blesch_2023")`
#'
#' @export
KnockoffGaussianSampler = R6Class(
	"KnockoffGaussianSampler",
	inherit = KnockoffSampler,
	public = list(
		#' @field feature_types (`character()`) Feature types supported by the sampler.
		#'   Will be checked against the provided [mlr3::Task] to ensure compatibility.
		feature_types = c("numeric", "integer"),
		#' @field x_tilde Knockoff matrix
		x_tilde = NULL,

		#' @description
		#' Creates a new instance using Gaussian knockoffs via [knockoff::create.second_order].
		#' @param task ([mlr3::Task]) Task to sample from.
		#' @param iters (`integer(1)`: `1`) Number of repetitions the `knockoff_fun` is applied to create multiple `x_tilde`
		#' instances per observation.
		initialize = function(
			task,
			iters = 1
		) {
			require_package("knockoff")
			super$initialize(
				task = task,
				knockoff_fun = function(x) {
					knockoff::create.second_order(as.matrix(x))
				},
				iters = iters
			)
			self$label = "Gaussian Knockoff sampler"
		}
	)
)

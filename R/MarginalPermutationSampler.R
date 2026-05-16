#' @title Marginal Permutation Sampler
#'
#' @description Implements marginal permutation-based sampling for Permutation Feature Importance (PFI).
#' Each specified feature is randomly shuffled (permuted) independently, breaking the
#' relationship between the feature and the target as well as between rows.
#'
#' @details
#' The permutation sampler randomly shuffles feature values across observations:
#' - Each feature is permuted **independently** within its column
#' - The association between feature values and target values is broken
#' - The association between feature values **across rows** is broken
#' - The marginal distribution of each feature is preserved
#'
#' **Important distinction from SAGE's "marginal" approach:**
#' - `MarginalPermutationSampler`: Shuffles features independently, breaking row structure
#' - `MarginalSAGE`: Uses reference data but keeps rows intact (features in coalition stay together)
#'
#' This is the classic approach used in Permutation Feature Importance (PFI) and
#' assumes features are independent.
#'
#' @export
#' @examples
#' library(mlr3)
#' task = tgen("2dnormals")$generate(n = 10)
#' task$data()
#' sampler = MarginalPermutationSampler$new(task)
#'
#' # Sample using row_ids from stored task
#' sampler$sample("x1")
#'
#' # Or use external data
#' data = task$data()
#' sampler$sample_newdata("x1", newdata = data)
MarginalPermutationSampler = R6Class(
	"MarginalPermutationSampler",
	inherit = MarginalSampler,
	public = list(
		#' @description
		#' Creates a new instance of the MarginalPermutationSampler class.
		#' @param task ([mlr3::Task]) Task to sample from.
		initialize = function(task) {
			super$initialize(task)
			self$label = "Permutation sampler"
		}
	),

	private = list(
		# Implement marginal sampling via independent permutation.
		# CONTRACT: may mutate `data` by reference (the single-draw branch
		# permutes the feature column in place via `:=`). Callers MUST pass a
		# disposable table -- both entry points do: MarginalSampler$sample()
		# uses .get_task_data_by_row_id() -> task$data() (always a fresh
		# data.table) and $sample_newdata() takes data.table::copy(newdata). The in-place
		# branch is deliberate (avoids copying a potentially large table just
		# to shuffle one column); the draw-major branch must allocate anyway.
		.sample_marginal = function(data, feature, samples_per_row = 1L) {
			if (samples_per_row == 1L) {
				data[, (feature) := lapply(.SD, sample), .SDcols = feature]
				return(data[, .SD, .SDcols = c(self$task$target_names, self$task$feature_names)])
			}

			# Draw-major: stack `samples_per_row` independent permutations of the feature column(s).
			# Other columns (including the conditioning context / passthrough features) are
			# repeated positionally so that row alignment with `row_ids` is preserved.
			out = data[rep.int(seq_len(.N), times = samples_per_row)]

			# One `:=` assigns all target columns at once; the inner `lapply` builds one stacked
			# vector per feature (`samples_per_row` independent permutations concatenated).
			out[,
				(feature) := lapply(feature, function(feat) {
					unlist(
						lapply(seq_len(samples_per_row), function(d) sample(data[[feat]])),
						use.names = FALSE
					)
				})
			]

			out[, .SD, .SDcols = c(self$task$target_names, self$task$feature_names)]
		}
	)
)

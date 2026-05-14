#' @title (experimental) Conditional Inference Tree Conditional Sampler
#'
#' @description Implements conditional sampling using conditional inference trees (ctree).
#' Builds a tree predicting target features from conditioning features, then samples
#' from the terminal node corresponding to each test observation.
#'
#' @details
#' This sampler approximates the conditional distribution \eqn{P(X_B | X_A = x_A)} by:
#' 1. Building a conditional inference tree with \eqn{X_B} as response and \eqn{X_A} as predictors
#' 2. For each test observation, finding its terminal (leaf) node in the tree
#' 3. Sampling uniformly from training observations in that same terminal node
#'
#' Conditional inference trees (ctree) use permutation tests to determine splits,
#' which helps avoid overfitting and handles mixed feature types naturally. The tree
#' partitions the feature space based on the conditioning variables, creating local
#' neighborhoods that respect the conditional distribution structure.
#'
#' **Key advantages over other samplers:**
#' - Handles mixed feature types (continuous and categorical)
#' - Non-parametric (no distributional assumptions)
#' - Automatic feature selection (splits only on informative features)
#' - Can capture non-linear conditional relationships
#' - Statistically principled splitting criteria
#'
#' **Hyperparameters** control tree complexity:
#' - `mincriterion`: Significance level for splits (higher = fewer splits)
#' - `minsplit`: Minimum observations required for a split
#' - `minbucket`: Minimum observations in terminal nodes
#'
#' This implementation is inspired by shapr's ctree approach but simplified for our
#' use case (we build trees on-demand rather than pre-computing all subsets).
#'
#' **Advantages:**
#' - Works with any feature types
#' - Robust to outliers
#' - Interpretable tree structure
#' - Handles high-dimensional conditioning
#'
#' **Limitations:**
#' - Requires model fitting (slower than kNN)
#' - Can produce duplicates if terminal nodes are small
#' - Tree building time increases with data size
#'
#' @examples
#' \donttest{
#' library(mlr3)
#' task = tgen("friedman1")$generate(n = 100)
#'
#' # Create sampler with default parameters
#' sampler = ConditionalCtreeSampler$new(task)
#'
#' # Sample features conditioned on others
#' test_data = task$data(rows = 1:5)
#' sampled = sampler$sample_newdata(
#'   feature = c("important2", "important3"),
#'   newdata = test_data,
#'   conditioning_set = "important1"
#' )
#' }
#'
#' @references `r print_bib("hothorn_2006", "aas_2021")`
#'
#' @export
ConditionalCtreeSampler = R6Class(
	"ConditionalCtreeSampler",
	inherit = ConditionalSampler,
	public = list(
		#' @field feature_types (`character()`) Feature types supported by the sampler.
		#'   Will be checked against the provided [mlr3::Task] to ensure compatibility.
		feature_types = c("numeric", "integer", "factor", "ordered"),
		#' @field tree_cache (`environment`) Cache for fitted ctree models.
		tree_cache = NULL,
		#' @description
		#' Creates a new ConditionalCtreeSampler.
		#' @param task ([mlr3::Task]) Task to sample from.
		#' @param conditioning_set (`character` | `NULL`) Default conditioning set to use in `$sample()`.
		#' @param mincriterion (`numeric(1)`: `0.95`) Significance level threshold for splitting (1 - p-value).
		#'   Higher values result in fewer splits (simpler trees).
		#' @param minsplit (`integer(1)`: `20L`) Minimum number of observations required for a split.
		#' @param minbucket (`integer(1)`: `7L`) Minimum number of observations in terminal nodes.
		#' @param use_cache (`logical(1)`: `TRUE`) Whether to cache fitted trees.
		initialize = function(
			task,
			conditioning_set = NULL,
			mincriterion = 0.95,
			minsplit = 20L,
			minbucket = 7L,
			use_cache = TRUE
		) {
			require_package("partykit")
			super$initialize(task, conditioning_set = conditioning_set)

			# Initialize tree cache
			self$tree_cache = new.env(parent = emptyenv())

			# Extend param_set with ctree-specific parameters
			self$param_set = c(
				self$param_set,
				paradox::ps(
					mincriterion = paradox::p_dbl(lower = 0, upper = 1, default = 0.95),
					minsplit = paradox::p_int(lower = 1L, default = 20L),
					minbucket = paradox::p_int(lower = 1L, default = 7L),
					use_cache = paradox::p_lgl(default = TRUE)
				)
			)

			self$param_set$set_values(
				mincriterion = mincriterion,
				minsplit = minsplit,
				minbucket = minbucket,
				use_cache = use_cache
			)

			self$label = "Conditional Inference Tree Conditional Sampler"
		}
	),

	private = list(
		# Core ctree sampling logic implementing conditional inference tree sampling
		.sample_conditional = function(data, feature, conditioning_set, samples_per_row = 1L, ...) {
			training_data = self$task$data(cols = self$task$feature_names)

			# Marginal fallback (no conditioning): draw samples_per_row x nrow(data) values
			# per feature with replacement from the training column, organized draw-major.
			if (length(conditioning_set) == 0) {
				n = nrow(data)
				out = data[rep.int(seq_len(.N), times = samples_per_row)]
				for (feat in feature) {
					out[, (feat) := sample(training_data[[feat]], n * samples_per_row, replace = TRUE)]
				}
				return(out[, .SD, .SDcols = c(self$task$target_names, self$task$feature_names)])
			}

			tree = private$.get_or_build_tree(feature, conditioning_set)

			n = nrow(data)
			data[, ..eval_id := seq_len(.N)]
			node_ids = predict(tree, newdata = data, type = "node")
			data[, ..node_id := node_ids]

			train_nodes = predict(tree, newdata = training_data, type = "node")
			train_data_with_nodes = data.table::copy(training_data)
			train_data_with_nodes[, ..node_id := train_nodes]

			# For each draw, sample one training observation per evidence row from its terminal node.
			# Stack the draws in draw-major order so positional alignment with row_ids is preserved.
			out = data[rep.int(seq_len(.N), times = samples_per_row)]

			for (i in seq_len(n)) {
				node_obs = train_data_with_nodes[..node_id == node_ids[i]]
				if (nrow(node_obs) == 0) {
					cli::cli_warn("No training observations in terminal node, using marginal sample")
					# Marginal fallback per draw: draw samples_per_row values for this row from training_data
					for (d in seq_len(samples_per_row)) {
						out_row_idx = (d - 1L) * n + i
						for (feat in feature) {
							sampled_value = sample(training_data[[feat]], 1L)
							data.table::set(out, i = out_row_idx, j = feat, value = sampled_value)
						}
					}
				} else {
					picks = node_obs[sample.int(.N, samples_per_row, replace = TRUE)]
					for (d in seq_len(samples_per_row)) {
						out_row_idx = (d - 1L) * n + i
						for (feat in feature) {
							data.table::set(out, i = out_row_idx, j = feat, value = picks[[feat]][d])
						}
					}
				}
			}

			data[, ..eval_id := NULL]
			data[, ..node_id := NULL]
			out[, .SD, .SDcols = c(self$task$target_names, self$task$feature_names)]
		},

		# Get cached tree or build a new one
		.get_or_build_tree = function(feature, conditioning_set) {
			# Create cache key from feature names and conditioning set
			cache_key = paste(
				paste(sort(match(feature, self$task$feature_names)), collapse = "+"),
				"|",
				paste(sort(match(conditioning_set, self$task$feature_names)), collapse = "+")
			)

			use_cache = self$param_set$values$use_cache

			# Check cache
			if (use_cache && exists(cache_key, envir = self$tree_cache)) {
				return(get(cache_key, envir = self$tree_cache))
			}

			# Build new tree
			tree = private$.build_tree(feature, conditioning_set)

			# Cache if enabled
			if (use_cache) {
				assign(cache_key, tree, envir = self$tree_cache)
			}

			tree
		},

		# Build a ctree model
		.build_tree = function(feature, conditioning_set) {
			# Get training data from task
			training_data = self$task$data(cols = self$task$feature_names)

			# Create formula: features ~ conditioning_set
			# For multiple features, use cbind on LHS
			if (length(feature) == 1) {
				formula_str = sprintf(
					"`%s` ~ %s",
					feature,
					paste(sprintf("`%s`", conditioning_set), collapse = " + ")
				)
			} else {
				formula_str = sprintf(
					"cbind(%s) ~ %s",
					paste(sprintf("`%s`", feature), collapse = ", "),
					paste(sprintf("`%s`", conditioning_set), collapse = " + ")
				)
			}

			formula = as.formula(formula_str)

			# Extract control parameters
			mincriterion = self$param_set$values$mincriterion
			minsplit = self$param_set$values$minsplit
			minbucket = self$param_set$values$minbucket

			# Build tree with partykit::ctree
			tree = partykit::ctree(
				formula = formula,
				data = training_data,
				control = partykit::ctree_control(
					mincriterion = mincriterion,
					minsplit = minsplit,
					minbucket = minbucket
				)
			)

			tree
		}
	)
)

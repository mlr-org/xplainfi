#' @title k-Nearest Neighbors Conditional Sampler
#'
#' @description Implements conditional sampling using k-nearest neighbors (kNN).
#' For each observation, finds the `k` most similar observations based on conditioning
#' features, then samples the target features from these neighbors.
#'
#' @details
#' This sampler approximates the conditional distribution \eqn{P(X_B | X_A = x_A)} by:
#' 1. Finding the k nearest neighbors of \eqn{x_A} in the training data
#' 2. Sampling uniformly from the target feature values \eqn{X_B} of these k neighbors
#'
#' This is a simple, non-parametric approach that:
#' - Requires no distributional assumptions
#' - Handles mixed feature types (numeric, integer, factor, ordered, logical)
#' - Is computationally efficient (no model fitting required)
#' - Adapts locally to the data structure
#'
#' The method is related to hot-deck imputation and kNN imputation techniques used in
#' missing data problems. As \eqn{k \to \infty} and \eqn{k/n \to 0}, the kNN conditional
#' distribution converges to the true conditional distribution under mild regularity
#' conditions (Lipschitz continuity).
#'
#' **Distance Metrics:**
#'
#' The sampler supports two distance metrics:
#' - **Euclidean**: For numeric/integer features only.
#'   Standardizes features before computing distances and searches neighbors with a kd-tree via `FNN::get.knnx()`.
#' - **Gower**: For mixed feature types. Handles numeric, factor, ordered, and logical features.
#'   Numeric features are range-normalized, categorical features use exact matching (0/1).
#'   Neighbors are found with `gower::gower_topn()`.
#'
#' The `distance` parameter controls which metric to use:
#' - `"auto"` (default): Automatically selects Euclidean for all-numeric features, Gower otherwise
#' - `"euclidean"`: Forces Euclidean distance (errors if non-numeric features present)
#' - `"gower"`: Forces Gower distance (works with any feature types)
#'
#' **Advantages:**
#' - Very fast (no model training)
#' - Works with any feature types
#' - Automatic distance metric selection
#' - Naturally respects local data structure
#'
#' **Limitations:**
#' - Sensitive to choice of `k`
#' - The full task data is required for prediction
#' - Can produce duplicates if `k` is small
#' - May not extrapolate well to new regions
#'
#' @examples
#' library(mlr3)
#' task = tgen("friedman1")$generate(n = 100)
#' sampler = ConditionalKNNSampler$new(task, k = 5)
#'
#' # Sample features conditioned on others
#' test_data = task$data(rows = 1:5)
#' sampled = sampler$sample_newdata(
#'   feature = c("important2", "important3"),
#'   newdata = test_data,
#'   conditioning_set = "important1"
#' )
#'
#' @references `r print_bib("little_2019", "troyanskaya_2001")`
#'
#' @export
ConditionalKNNSampler = R6Class(
  "ConditionalKNNSampler",
  inherit = ConditionalSampler,
  public = list(
    #' @field feature_types (`character()`) Feature types supported by the sampler.
    feature_types = c("numeric", "integer", "factor", "ordered", "logical"),

    #' @description
    #' Creates a new ConditionalKNNSampler.
    #' @param task ([mlr3::Task]) Task to sample from.
    #' @param conditioning_set (`character` | `NULL`) Default conditioning set to use in `$sample()`.
    #' @param k (`integer(1)`: `5L`) Number of nearest neighbors to sample from.
    #' @param distance (`character(1)`: `"auto"`) Distance metric, one of `"auto"`, `"euclidean"`, or `"gower"`.
    #'   See the Distance Metrics section.
    initialize = function(task, conditioning_set = NULL, k = 5L, distance = c("auto", "euclidean", "gower")) {
      super$initialize(task, conditioning_set = conditioning_set)
      distance = match.arg(distance)

      self$param_set = c(
        self$param_set,
        paradox::ps(
          k = paradox::p_int(lower = 1L, default = 5L),
          distance = paradox::p_fct(levels = c("auto", "euclidean", "gower"), default = "auto")
        )
      )
      self$param_set$set_values(k = k, distance = distance)

      self$label = "k-Nearest Neighbors Conditional Sampler"
    },

    #' @description
    #' Sample features from their kNN-based conditional distribution.
    #'
    #' @param feature (`character()`) Feature name(s) to sample.
    #' @param row_ids (`integer()` | `NULL`) Row IDs from task to use as conditioning values.
    #' @param conditioning_set (`character()` | `NULL`) Features to condition on.
    #'   If `NULL`, samples from marginal distribution (random sampling from training data).
    #' @param samples_per_row (`integer(1)`: `1L`) Number of independent samples per input row.
    #'   See [FeatureSampler]`$sample()` for output shape and ordering.
    #' @param k (`integer(1)` | `NULL`) Number of neighbors. If `NULL`, uses stored parameter.
    #' @return Modified copy with sampled feature(s).
    sample = function(
      feature,
      row_ids = NULL,
      conditioning_set = NULL,
      samples_per_row = 1L,
      k = NULL
    ) {
      super$sample(feature, row_ids, conditioning_set, samples_per_row = samples_per_row, k = k)
    },

    #' @description
    #' Sample from external data conditionally.
    #'
    #' @param feature (`character()`) Feature(s) to sample.
    #' @param newdata ([`data.table`][data.table::data.table]) External data to use.
    #' @param conditioning_set (`character()` | `NULL`) Features to condition on.
    #' @param samples_per_row (`integer(1)`: `1L`) Number of independent samples per input row.
    #'   See [FeatureSampler]`$sample()` for output shape and ordering.
    #' @param k (`integer(1)` | `NULL`) Number of neighbors. If `NULL`, uses stored parameter.
    #' @return Modified copy with sampled feature(s).
    sample_newdata = function(
      feature,
      newdata,
      conditioning_set = NULL,
      samples_per_row = 1L,
      k = NULL
    ) {
      super$sample_newdata(
        feature,
        newdata,
        conditioning_set,
        samples_per_row = samples_per_row,
        k = k
      )
    }
  ),

  private = list(
    # Core kNN sampling logic implementing k-nearest neighbors conditional sampling
    .sample_conditional = function(
      data,
      feature,
      conditioning_set,
      samples_per_row = 1L,
      k = NULL,
      ...
    ) {
      k = resolve_param(k, self$param_set$values$k, 5L)
      training_data = self$task$data(cols = self$task$feature_names)

      # Marginal fallback (no conditioning): draw with replacement, draw-major
      if (length(conditioning_set) == 0) {
        n = nrow(data)
        out = data[rep.int(seq_len(.N), times = samples_per_row)]
        for (feat in feature) {
          out[, (feat) := sample(training_data[[feat]], n * samples_per_row, replace = TRUE)]
        }
        return(out[, .SD, .SDcols = c(self$task$target_names, self$task$feature_names)])
      }

      cond_types = self$task$feature_types[id %in% conditioning_set, type]
      all_numeric = all(cond_types %in% c("numeric", "integer"))
      distance = self$param_set$values$distance
      if (distance == "euclidean" && !all_numeric) {
        cli::cli_abort(c(
          x = "Euclidean distance requires numeric conditioning features.",
          i = "Non-numeric: {.val {conditioning_set[!cond_types %in% c('numeric', 'integer')]}}.",
          i = "Use {.code distance = \"gower\"} or {.code \"auto\"}."
        ))
      }
      use_gower = distance == "gower" || (distance == "auto" && !all_numeric)

      query_cond_dt = data[, .SD, .SDcols = conditioning_set]
      train_cond_dt = training_data[, .SD, .SDcols = conditioning_set]

      n = nrow(data)
      k_actual = min(k, nrow(training_data))

      # neighbors[, i] = the k training rows nearest to evidence row i. Both backends search
      # in C; exactly k are returned, ties at the k-th distance are not expanded.
      neighbors = if (use_gower) {
        require_package("gower")
        gower::gower_topn(query_cond_dt, train_cond_dt, n = k_actual)$index
      } else {
        require_package("FNN")
        train_cond = as.matrix(train_cond_dt)
        query_cond = as.matrix(query_cond_dt)
        # Standardize with training moments so no feature dominates the Euclidean distance
        means = colMeans(train_cond)
        sds = apply(train_cond, 2, stats::sd)
        sds[sds == 0] = 1
        train_cond = scale(train_cond, center = means, scale = sds)
        query_cond = scale(query_cond, center = means, scale = sds)
        t(FNN::get.knnx(train_cond, query_cond, k = k_actual)$nn.index)
      }

      # sampled_idx[d, i] = training row chosen for draw d, evidence row i
      sampled_idx = matrix(
        neighbors[cbind(
          sample.int(k_actual, n * samples_per_row, replace = TRUE),
          rep(seq_len(n), each = samples_per_row)
        )],
        nrow = samples_per_row
      )

      # Draw-major flatten: rows of `t(sampled_idx)` are draws, so as.vector(t(...))
      # gives [draw 1 across all n rows, draw 2 across all n rows, ...].
      flat_idx = as.vector(t(sampled_idx))

      out = data[rep.int(seq_len(.N), times = samples_per_row)]
      out[, (feature) := training_data[flat_idx, .SD, .SDcols = feature]]
      out[, .SD, .SDcols = c(self$task$target_names, self$task$feature_names)]
    }
  )
)

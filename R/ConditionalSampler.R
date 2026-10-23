#' @title Conditional Feature Sampler
#'
#' @description Base class for conditional sampling methods where features
#' are sampled conditionally on other features. This is an abstract class
#' that should be extended by concrete implementations.
#'
#' @export
ConditionalSampler = R6Class(
  "ConditionalSampler",
  inherit = FeatureSampler,
  public = list(
    #' @description
    #' Creates a new instance of the ConditionalSampler class
    #' @param task ([mlr3::Task]) Task to sample from
    #' @param conditioning_set (`character` | `NULL`) Default conditioning set to use in `$sample()`.
    initialize = function(task, conditioning_set = NULL) {
      super$initialize(task)
      self$label = "Conditional sampler"

      # Define param_set with conditioning_set parameter for all conditional samplers
      self$param_set = paradox::ps(
        conditioning_set = paradox::p_uty(default = NULL)
      )

      # Store conditioning_set if provided
      if (!is.null(conditioning_set)) {
        self$param_set$set_values(conditioning_set = conditioning_set)
      }
    },

    #' @description
    #' Sample from stored task conditionally on other features.
    #' @param feature (`character`) Feature(s) to sample.
    #' @param row_ids (`integer()` | `NULL`) Row IDs to use. If `NULL`, uses all rows.
    #' @param conditioning_set (`character` | `NULL`) Features to condition on.
    #' @param samples_per_row (`integer(1)`: `1L`) Number of independent samples per input row.
    #'   See [FeatureSampler]`$sample()` for output shape and ordering.
    #' @param ... Additional arguments passed to the sampler implementation.
    #' @return Modified copy with sampled feature(s).
    sample = function(feature, row_ids = NULL, conditioning_set = NULL, samples_per_row = 1L, ...) {
      checkmate::assert_count(samples_per_row, positive = TRUE)
      data_copy = private$.get_task_data_by_row_id(row_ids)

      # Determine conditioning set (note: NULL is different than character(0))
      # Priority:
      # 1) function argument,
      # 2) stored param_set value,
      # 3) default (all other features) (! important behavior expected by CFI implementation!)
      conditioning_set = resolve_param(
        conditioning_set,
        self$param_set$values$conditioning_set,
        setdiff(self$task$feature_names, feature)
      )

      if (xplain_opt("debug")) {
        cli::cli_alert_info(
          "Resolved conditioning_set: {.val {conditioning_set}}"
        )
      }

      private$.sample_conditional(
        data_copy,
        feature,
        conditioning_set,
        samples_per_row = samples_per_row,
        ...
      )
    },

    #' @description
    #' Sample from external data conditionally.
    #' @param feature (`character`) Feature(s) to sample.
    #' @param newdata ([`data.table`][data.table::data.table]) External data to use.
    #' @param conditioning_set (`character` | `NULL`) Features to condition on.
    #' @param samples_per_row (`integer(1)`: `1L`) Number of independent samples per input row.
    #'   See [FeatureSampler]`$sample()` for output shape and ordering.
    #' @param ... Additional arguments passed to the sampler implementation.
    #' @return Modified copy with sampled feature(s).
    sample_newdata = function(
      feature,
      newdata,
      conditioning_set = NULL,
      samples_per_row = 1L,
      ...
    ) {
      checkmate::assert_count(samples_per_row, positive = TRUE)
      # Create copy to avoid modifying original
      if (inherits(newdata, "data.table")) {
        data_copy = data.table::copy(newdata)
      } else {
        data_copy = as.data.table(newdata)
      }

      # Determine conditioning set (note: NULL is different than character(0))
      # Priority:
      # 1) function argument,
      # 2) stored param_set value,
      # 3) default (all other features) (! important behavior expected by CFI implementation!)
      conditioning_set = resolve_param(
        conditioning_set,
        self$param_set$values$conditioning_set,
        setdiff(self$task$feature_names, feature)
      )

      if (xplain_opt("debug")) {
        cli::cli_alert_info(
          "Resolved conditioning_set: {.val {conditioning_set}}"
        )
      }

      private$.sample_conditional(
        data_copy,
        feature,
        conditioning_set,
        samples_per_row = samples_per_row,
        ...
      )
    },

    #' @description
    #' Sample from external data with a row-specific conditioning set.
    #' Row `i` of `newdata` resamples the features whose `mask[i, ]` is `FALSE`,
    #' conditionally on the features whose `mask[i, ]` is `TRUE`.
    #' Features that are not columns of `mask` are neither resampled nor conditioned on.
    #' The default groups rows by their mask and calls `$sample_newdata()` once per group;
    #' subclasses may override it with a single batched call.
    #' @param newdata ([`data.table`][data.table::data.table]) External data to use.
    #' @param mask (`logical` matrix) One row per row of `newdata` and one named column per feature.
    #' @param samples_per_row (`integer(1)`: `1L`) Number of independent samples per input row.
    #'   See [FeatureSampler]`$sample()` for output shape and ordering.
    #' @return [data.table][data.table::data.table] with `samples_per_row * nrow(newdata)` rows in draw-major order.
    sample_masked = function(newdata, mask, samples_per_row = 1L) {
      newdata = as.data.table(newdata)
      checkmate::assert_matrix(mask, mode = "logical", any.missing = FALSE, nrows = nrow(newdata), col.names = "unique")
      checkmate::assert_subset(colnames(mask), self$task$feature_names)
      checkmate::assert_count(samples_per_row, positive = TRUE)

      n = nrow(newdata)
      features = colnames(mask)
      key = apply(mask, 1L, function(row) paste(as.integer(row), collapse = ""))
      groups = unique(key)
      out = vector("list", length(groups))
      positions = vector("list", length(groups))
      for (g in seq_along(groups)) {
        idx = which(key == groups[g])
        keep = mask[idx[1L], ]
        out[[g]] = if (all(keep)) {
          newdata[
            rep.int(idx, times = samples_per_row),
            .SD,
            .SDcols = c(self$task$target_names, self$task$feature_names)
          ]
        } else {
          self$sample_newdata(
            feature = features[!keep],
            newdata = newdata[idx],
            conditioning_set = features[keep],
            samples_per_row = samples_per_row
          )
        }
        # Group output is draw-major over its own rows; map back to draw-major over all rows.
        positions[[g]] = rep(idx, times = samples_per_row) +
          rep((seq_len(samples_per_row) - 1L) * n, each = length(idx))
      }
      out = rbindlist(out, use.names = TRUE)
      out[order(unlist(positions))]
    }
  ),

  private = list(
    # @description
    # Internal method for conditional sampling. Must be implemented by subclasses.
    # @param data (`data.table`) Data to sample from (copy that can be modified).
    # @param feature (`character`) Feature(s) to sample.
    # @param conditioning_set (`character` | `NULL`) Features to condition on.
    # @param samples_per_row (`integer(1)`) Number of draws per input row.
    # @param ... Additional sampler-specific arguments.
    .sample_conditional = function(data, feature, conditioning_set, samples_per_row = 1L, ...) {
      cli::cli_abort(c(
        "Abstract method",
        i = "Subclasses must implement the {.fn .sample_conditional} method"
      ))
    }
  )
)

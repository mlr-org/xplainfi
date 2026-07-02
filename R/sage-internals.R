#' Batch Predict for SAGE
#'
#' Performs batched prediction on combined data to manage memory usage.
#' Supports both classification (probability predictions) and regression.
#'
#' @param learner ([`Learner`][mlr3::Learner]) Trained mlr3 learner.
#' @param combined_data (`data.table`) Data with feature columns to predict on.
#' @param task ([`Task`][mlr3::Task]) mlr3 task object.
#' @param batch_size (`integer(1)` or `NULL`) Batch size for predictions. If `NULL` or if
#'   `total_rows <= batch_size`, processes all data at once.
#' @param task_type (`character(1)`) Task type, either `"classif"` or `"regr"`.
#'
#' @return For classification: `matrix` of class probabilities (n_rows x n_classes).
#'   For regression: `numeric` vector of predictions (length n_rows).
#'
#' @keywords internal
sage_batch_predict = function(learner, combined_data, task, batch_size, task_type) {
  total_rows = nrow(combined_data)

  if (!is.null(batch_size) && total_rows > batch_size) {
    # Batched prediction
    n_batches = ceiling(total_rows / batch_size)
    all_predictions = vector("list", n_batches)

    for (batch_idx in seq_len(n_batches)) {
      start_row = (batch_idx - 1) * batch_size + 1
      end_row = min(batch_idx * batch_size, total_rows)
      batch_data = combined_data[start_row:end_row]

      if (xplain_opt("debug")) {
        cli::cli_inform(
          "Predicting on {.val {nrow(batch_data)}} instances in batch {.val {batch_idx}/{n_batches}}"
        )
      }

      pred_result = if (is.function(learner$predict_newdata_fast)) {
        learner$predict_newdata_fast(newdata = batch_data, task = task)
      } else {
        learner$predict_newdata(newdata = batch_data, task = task)
      }

      all_predictions[[batch_idx]] = if (task_type == "classif") {
        pred_result$prob
      } else {
        pred_result$response
      }
    }

    # Combine predictions from all batches
    if (task_type == "classif") {
      do.call(rbind, all_predictions)
    } else {
      do.call(c, all_predictions)
    }
  } else {
    # Single prediction without batching
    if (xplain_opt("debug")) {
      cli::cli_inform("Predicting on {.val {nrow(combined_data)}} instances at once")
    }

    pred_result = if (is.function(learner$predict_newdata_fast)) {
      learner$predict_newdata_fast(newdata = combined_data, task = task)
    } else {
      learner$predict_newdata(newdata = combined_data, task = task)
    }

    if (task_type == "classif") {
      pred_result$prob
    } else {
      pred_result$response
    }
  }
}

#' Aggregate Predictions by Coalition and Test Instance
#'
#' Averages predictions across multiple samples (reference data or conditional samples)
#' for each unique combination of coalition and test instance.
#'
#' @param combined_data (`data.table`) Data with columns `.coalition_id`, `.test_instance_id`,
#'   and feature columns.
#' @param predictions (`matrix` or `numeric`) For classification: matrix of class probabilities.
#'   For regression: numeric vector of predictions.
#' @param task_type (`character(1)`) Task type, either `"classif"` or `"regr"`.
#' @param class_names (`character()` or `NULL`: `NULL`) Character vector of class names. Required
#'   for classification, ignored for regression.
#'
#' @return `data.table` with columns:
#'   - `.coalition_id`: Coalition identifier (integer)
#'   - `.test_instance_id`: Test instance identifier (integer)
#'   - For classification: One column per class with averaged probabilities (numeric)
#'   - For regression: `avg_pred` column with averaged predictions (numeric)
#'
#' @keywords internal
sage_aggregate_predictions = function(combined_data, predictions, task_type, class_names = NULL) {
  if (task_type == "classif") {
    # Add prediction columns to combined_data
    n_classes = ncol(predictions)
    for (j in seq_len(n_classes)) {
      combined_data[, paste0(".pred_class_", j) := predictions[, j]]
    }

    # Aggregate: calculate mean probability for each class, grouped by coalition and test instance
    agg_cols = paste0(".pred_class_", seq_len(n_classes))
    avg_preds = combined_data[,
      lapply(.SD, function(x) mean(x, na.rm = TRUE)),
      .SDcols = agg_cols,
      by = c(".coalition_id", ".test_instance_id")
    ]

    # Rename aggregated columns to original class names
    setnames(avg_preds, agg_cols, class_names)
    avg_preds
  } else if (task_type == "regr") {
    # Regression: add predictions and aggregate
    .prediction = NULL # the data.table NSE NOTE tax
    combined_data[, .prediction := predictions]

    combined_data[,
      list(avg_pred = mean(.prediction, na.rm = TRUE)),
      by = c(".coalition_id", ".test_instance_id")
    ]
  }
}

#' Build row-major growing-prefix coalitions for a permutation list
#'
#' For each permutation, emit its growing prefixes
#' (`perm[1]`, `perm[1:2]`, ..., `perm`). Row-major over
#' `(permutation, step)`. Pure; no evaluation, no RNG.
#'
#' @param perm_sublist (`list`) Feature-name permutations.
#' @return `list` of character vectors (coalitions).
#' @keywords internal
#' @noRd
sage_growing_coalitions = function(perm_sublist) {
  # Pre-allocate to the exact coalition count (sum of permutation lengths)
  coalitions = vector("list", sum(lengths(perm_sublist)))
  k = 1L
  for (i in seq_along(perm_sublist)) {
    perm = perm_sublist[[i]]
    for (j in seq_along(perm)) {
      coalitions[[k]] = perm[seq_len(j)]
      k = k + 1L
    }
  }
  coalitions
}

#' Accumulate SAGE marginal contributions from a loss vector
#'
#' Given growing-prefix losses laid out row-major over
#' `(permutation, step)` (optionally preceded by `offset` leading
#' slots, e.g. an empty-coalition entry), accumulate per-feature SAGE
#' value sums and squared sums. Every permutation is a full feature
#' permutation, so the loss index is closed-form: no search/map.
#' Pure; order-independent across permutations.
#'
#' @param perm_sublist (`list`) Feature-name permutations.
#' @param losses (`numeric`) Losses for `offset` leading slots then the
#'   row-major growing-prefix coalitions of `perm_sublist`.
#' @param baseline (`numeric(1)`) Empty-coalition loss anchor.
#' @param feature_names (`character`) Names for the output vectors.
#' @param offset (`integer(1)`: `0L`) Leading loss slots to skip.
#' @return `list(sv, sv_sq)` named numeric vectors over `feature_names`.
#' @keywords internal
#' @noRd
sage_marginal_contributions = function(perm_sublist, losses, baseline, feature_names, offset = 0L) {
  sv = numeric(length(feature_names))
  sv_sq = numeric(length(feature_names))
  names(sv) = feature_names
  names(sv_sq) = feature_names

  for (i in seq_along(perm_sublist)) {
    perm = perm_sublist[[i]]
    p = length(perm)
    prev_loss = baseline
    for (j in seq_len(p)) {
      feature = perm[j]
      current_loss = losses[offset + (i - 1L) * p + j]
      contribution = prev_loss - current_loss
      sv[feature] = sv[feature] + contribution
      sv_sq[feature] = sv_sq[feature] + contribution^2
      prev_loss = current_loss
    }
  }

  list(sv = sv, sv_sq = sv_sq)
}

#' Shapley-kernel coalition-size distribution
#'
#' Probability of drawing a coalition of size `k` under the Shapley kernel,
#' `P(size = k) proportional to 1 / (k * (m - k))` for `k` in `1..m-1`
#' (empty and full coalitions are excluded; they enter via the WLS constraints).
#'
#' This is the per-SIZE weight. The per-COALITION Shapley weight is
#' `(m - 1) / (choose(m, k) * k * (m - k))`; the `choose(m, k)` multiplicity
#' converts one into the other. The two coincide at `m = 3` but diverge for
#' larger `m`, so using the per-coalition weight here would silently bias the
#' estimator (Covert & Lee 2021 sample sizes proportional to `1 / (k * (m - k))`).
#'
#' @param m (`integer(1)`) Number of features. Must be `>= 2`.
#' @return `numeric(m - 1)` size probabilities for `k = 1, ..., m - 1`.
#' @keywords internal
#' @noRd
sage_kernel_size_probs = function(m) {
  k = seq_len(m - 1L)
  w = 1 / (k * (m - k))
  w / sum(w)
}

#' Exact Shapley-kernel design matrix `A = E[z z^T]`
#'
#' Closed form of the design matrix under the Shapley-kernel coalition
#' distribution (Covert & Lee 2021, "unbiased KernelSHAP", their `calculate_A`).
#' Using the exact `A` rather than a sampled estimate makes the estimator
#' unbiased and keeps `A` invertible for any coalition budget, so only `b`
#' needs Monte Carlo estimation.
#'
#' By symmetry the diagonal is `E[z_i] = 0.5`; the off-diagonal is the
#' co-occurrence probability `E[z_i z_j] = sum_k p(k) * k(k-1) / (m(m-1))`
#' with `p(k)` from `sage_kernel_size_probs()`. Paired sampling leaves `A`
#' unchanged, so this holds with or without it.
#'
#' @param m (`integer(1)`) Number of features. Must be `>= 2`.
#' @return `matrix` of dimension `m x m`.
#' @keywords internal
#' @noRd
sage_kernel_A = function(m) {
  k = seq_len(m - 1L)
  p = sage_kernel_size_probs(m)
  diag_val = sum(p * k / m) # = 0.5
  off_val = sum(p * k * (k - 1L) / (m * (m - 1L)))
  A = matrix(off_val, m, m)
  diag(A) = diag_val
  A
}

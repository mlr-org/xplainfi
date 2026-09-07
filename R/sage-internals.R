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

#' Default sampling budgets for the SAGE estimators
#'
#' Free functions rather than private methods to keep the R6 objects lean
#' (closures on R6 objects are serialized with them). Documented in the
#' `n_permutations` / `n_coalitions` param docs of [SAGE].
#'
#' The permutation default of 10 permutations is reduced on small feature sets so its cost
#' stays at or below half of exact enumeration (`2^m` coalition evaluations).
#'
#' The kernel default of `20 * m` draws (capped at 4096) is sized for the default `"original"`
#' variant: it gives the batch-means standard errors five variance blocks of `4 * m` draws, and
#' in our benchmarks (ten-feature `friedman1`, `regr.rpart`) it was several times more accurate
#' than the permutation default at roughly four times its evaluation cost. Like the permutation
#' default it is capped at half the cost of exact enumeration, so the default never triggers the
#' exact-cost message; below about nine features that cap binds, and the exact estimator is the
#' better tool.
#'
#' @param m (`integer(1)`) Number of features.
#' @return `integer(1)` default budget.
#' @keywords internal
#' @noRd
sage_default_n_permutations = function(m) {
  if (m >= 8L) 10L else max(2L, as.integer((2^m - 1L) %/% (2L * m)))
}

#' @rdname sage_default_n_permutations
#' @noRd
sage_default_n_coalitions = function(m) {
  half_enum = if (m <= 30L) 2^(m - 2L) else Inf # binds only for small m
  as.integer(max(2, min(4096, 20 * m, half_enum)))
}

#' Sampling budget requested for the configured estimator
#'
#' Reads the active estimator's budget from the param_set values, falling back to the
#' adaptive default when it is unset (e.g. cleared via `$param_set$values`). Shared by
#' `$compute()` and the `$budget` accessor so both report the same number.
#'
#' @param values (`list`) `$param_set$values` of a [SAGE] object.
#' @param m (`integer(1)`) Number of features.
#' @return `numeric(1)` budget in the estimator's own units (`2^m` coalitions for `"exact"`).
#' @keywords internal
#' @noRd
sage_requested_budget = function(values, m) {
  switch(
    values$estimator %||% "permutation",
    permutation = values$n_permutations %||% sage_default_n_permutations(m),
    kernel = values$n_coalitions %||% sage_default_n_coalitions(m),
    exact = 2^m
  )
}

#' Coalition evaluations implied by a sampling budget
#'
#' The estimators take budgets in different units (permutations, paired coalition draws,
#' enumerated coalitions), which makes them incomparable at face value. Coalition evaluations
#' are the shared currency: they are what the value function is actually called for, and the
#' cost the user pays. Reported by `$budget` and used to compare a sampling budget against
#' exact enumeration.
#'
#' @param estimator (`character(1)`) `"permutation"`, `"kernel"`, or `"exact"`.
#' @param m (`integer(1)`) Number of features.
#' @param budget (`integer(1)` | `NULL`) Budget in the estimator's own units; ignored for
#'   `"exact"`, which enumerates `2^m` coalitions regardless.
#' @return `numeric(1)` number of evaluated coalitions, or `NA_real_` for an unknown budget.
#' @keywords internal
#' @noRd
sage_n_evals = function(estimator, m, budget) {
  if (identical(estimator, "exact")) {
    return(2^m)
  }
  if (is.null(budget) || is.na(budget)) {
    return(NA_real_)
  }
  # One empty-coalition baseline plus m growing prefixes per permutation; two anchors
  # (empty and full) plus a coalition and its complement per paired kernel draw.
  if (identical(estimator, "permutation")) 1 + budget * m else 2 + 2 * budget
}

#' Unit a sampling budget is counted in
#'
#' Used for labelling `$budget`, convergence plots, and stopping messages.
#'
#' @param estimator (`character(1)`) `"permutation"`, `"kernel"`, or `"exact"`.
#' @return `character(1)`.
#' @keywords internal
#' @noRd
sage_budget_unit = function(estimator) {
  switch(estimator, permutation = "permutations", kernel = "coalition draws", exact = "coalitions")
}

#' Convergence ratio of a Monte Carlo Shapley estimate
#'
#' The stopping criterion of the reference Python `sage` package (`detect_convergence`):
#' the largest standard error relative to the spread of the importance values, which makes it
#' invariant to the scale of the loss. Convergence is declared when the ratio falls below the
#' threshold. Shared by both sampling estimators, but note that the standard errors feeding it
#' differ in provenance (running variance across permutations, closed form or batch means across
#' coalitions), so the same threshold does not buy the same budget across estimators or variants.
#'
#' Missing standard errors yield `NA` rather than being dropped: for the kernel estimator they
#' signal a design matrix that is not yet identifiable, which must not read as convergence.
#'
#' @param importance (`numeric`) Current importance estimates.
#' @param se (`numeric`) Their standard errors.
#' @return `numeric(1)` ratio, or `NA_real_` when it cannot be computed.
#' @keywords internal
#' @noRd
sage_convergence_ratio = function(importance, se) {
  if (anyNA(importance) || anyNA(se)) {
    return(NA_real_)
  }
  spread = max(importance) - min(importance)
  # A degenerate spread (single feature, or all features equal) leaves nothing to normalize
  # by, so the absolute standard error is used instead.
  ratio = if (spread > 0 && is.finite(spread)) max(se) / spread else max(se)
  if (is.finite(ratio)) ratio else NA_real_
}

#' Point out when a sampling budget reaches exact-enumeration cost
#'
#' Emits a message when the resolved sampling budget costs at least as many
#' coalition evaluations as exact enumeration, which removes the
#' coalition-sampling error at the same or lower cost. A message rather than a
#' warning: oversampling can be deliberate (e.g. ConditionalSAGE, where
#' repeated evaluations average the conditional sampler's noise). Emitted once
#' per `$compute()` call and silenced by `xplain_opt(verbose = FALSE)`. Gated to
#' `m >= 3`, where the adaptive defaults stay below enumeration and the
#' absolute waste can be nontrivial.
#'
#' @param estimator (`character(1)`) `"permutation"` or `"kernel"`.
#' @param m (`integer(1)`) Number of features.
#' @param budget (`integer(1)`) Resolved `n_permutations` or `n_coalitions`.
#' @param early_stopping (`logical(1)`) Whether the budget is a convergence-driven upper bound
#'   rather than a planned cost. Reaching enumeration cost is then a guard the computation is
#'   not expected to hit, so the message is suppressed; the exhaustion warning covers the case
#'   where it *is* hit.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
sage_inform_budget_vs_exact = function(estimator, m, budget, early_stopping = FALSE) {
  if (!xplain_opt("verbose") || early_stopping || m < 3L || m > 30L) {
    return(invisible(NULL))
  }
  n_exact = 2^m
  evals = sage_n_evals(estimator, m, budget)
  if (evals >= n_exact) {
    cli::cli_inform(
      c(
        "i" = "The {.val {estimator}} estimator will evaluate {evals} coalitions,
               at least as many as enumerating all {n_exact} (2^{m}) coalitions.",
        "i" = "{.code estimator = \"exact\"} computes SAGE values without coalition-sampling
               error at the same or lower cost (for {.cls ConditionalSAGE}, oversampling can
               still be deliberate; see the {.arg estimator} docs)."
      )
    )
  }
  invisible(NULL)
}

#' Abort when the exact estimator's enumeration exceeds the feature cap
#'
#' The exact estimator's cost grows as `2^m`, so it is capped. Checked at
#' construction and again in `$compute()`, since the estimator or cap may have
#' been changed via `$param_set$values` in between.
#'
#' @param m (`integer(1)`) Number of features.
#' @param max_features (`integer(1)`) Feature-count cap.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
sage_assert_exact_budget = function(m, max_features) {
  if (m > max_features) {
    cli::cli_abort(c(
      "The exact estimator would enumerate {.val {2^m}} coalitions of {m} features.",
      "i" = "This exceeds the {.arg max_features} cap ({max_features}); the cost grows as 2^n_features.",
      "i" = "Increase {.arg max_features} to override, or use {.code estimator = \"kernel\"} instead."
    ))
  }
  invisible(NULL)
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
#' The kernel SAGE estimator uses the *sampled* design matrix by default (original
#' KernelSHAP, far lower variance); this exact form is used only as a fallback when
#' the sampled matrix is not yet full rank, since it is invertible for any budget.
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

#' Constrained weighted least-squares Shapley solve
#'
#' The efficiency-constrained WLS solution shared by both kernel variants
#' (Covert & Lee 2021, Eqs. 7 and 9):
#' `phi = A^-1 b - A^-1 1 (1^T A^-1 b - total) / (1^T A^-1 1)`.
#'
#' @param A_inv (`matrix`) Inverse of the (sampled or exact) design matrix.
#' @param b (`numeric(m)`) Right-hand side.
#' @param total (`numeric(1)`) Value of the grand coalition, the sum constraint.
#' @return `numeric(m)` constrained Shapley estimate.
#' @keywords internal
#' @noRd
sage_kernel_solve_constrained = function(A_inv, b, total) {
  A_inv_b = as.numeric(A_inv %*% b)
  A_inv_1 = as.numeric(A_inv %*% rep(1, nrow(A_inv)))
  A_inv_b - A_inv_1 * ((sum(A_inv_b) - total) / sum(A_inv_1))
}

#' Unbiased kernel SAGE estimate and closed-form standard errors
#'
#' The "unbiased KernelSHAP" estimator (Covert & Lee 2021, Eq. 9, their Algorithm 3): the design
#' matrix `A` is the exact closed form and only `b` is estimated from the sampled coalitions. This
#' is the estimator implemented by the reference Python `sage` package. Its covariance is the
#' closed form of their Eqs. 12-13, `Cov(phi) = C Cov(b_mean) C^T` with
#' `C = A^-1 - A^-1 1 1^T A^-1 / (1^T A^-1 1)`.
#' Note the minus sign in `C`: the `sage` implementation adds this term (`calculate_result` in
#' `kernel_estimator.py`), which contradicts the paper's Eq. 13 and was verified by simulation to
#' misstate the variance; the paper's formula is used here.
#' `total = v(full)` is treated as a fixed constraint (its Monte Carlo error from the anchor
#' evaluations is ignored, matching `sage`).
#'
#' @param b_mean (`numeric(m)`) Running mean of the per-pair `b` vector.
#' @param cov_mean (`matrix` | `NULL`) Covariance of `b_mean` (per-pair covariance divided by
#'   the number of pairs). If `NULL`, standard errors are returned as `NA`.
#' @param total (`numeric(1)`) Value of the grand coalition, `v(full)`, used as the sum constraint.
#' @param A_inv (`matrix`) Precomputed `solve(sage_kernel_A(m))`; it depends only on `m`, so
#'   callers evaluating repeatedly (convergence checkpoints) pass it once.
#' @return `list(phi, se)`, each a `numeric(m)`.
#' @keywords internal
#' @noRd
sage_kernel_estimate_unbiased = function(b_mean, cov_mean, total, A_inv) {
  phi = sage_kernel_solve_constrained(A_inv, b_mean, total)

  se = rep(NA_real_, length(b_mean))
  if (!is.null(cov_mean)) {
    A_inv_1 = as.numeric(A_inv %*% rep(1, nrow(A_inv)))
    C = A_inv - outer(A_inv_1, A_inv_1) / sum(A_inv_1)
    se = sqrt(pmax(diag(C %*% cov_mean %*% t(C)), 0))
  }
  list(phi = phi, se = se)
}

#' Original kernel SAGE estimate and batch-means standard errors
#'
#' The "original KernelSHAP" estimator (Covert & Lee 2021, Eq. 7, their Algorithm 1): both the
#' design matrix `A` and `b` are estimated from the same sampled coalitions. Its variance has no
#' closed form (their Section 4.1), so the paper estimates it from independent intermediate
#' estimates computed on consecutive blocks of `block_size` draws (their Section 4.3):
#' `Cov(phi_n) ~ (block_size / n) Cov(block estimates)`, relying on the empirically observed
#' `O(1 / n)` rate. The standard errors thus quantify sampling variance only; the paper shows the
#' estimator's finite-sample bias to be negligible but does not bound it.
#' It falls back to the exact `A` only when the sampled one is singular at the end of the budget.
#'
#' @param A_mean (`matrix`) Running mean of the sampled design matrix.
#' @param b_mean (`numeric(m)`) Running mean of the per-pair `b` vector.
#' @param block_estimates (`list` of `numeric(m)`) Shapley estimates of the completed blocks.
#' @param block_size (`integer(1)`) Draws per block.
#' @param n_pairs (`integer(1)`) Total draws so far.
#' @param total (`numeric(1)`) Value of the grand coalition, `v(full)`, used as the sum constraint.
#' @param require_result (`logical(1)`) If `TRUE`, fall back to the exact `A` (with a warning)
#'   when the sampled `A` is singular so a finite point estimate is always returned.
#' @return `list(phi, se)`, each a `numeric(m)`. `se` is all-`NA` with fewer than two block
#'   estimates or when the sampled design matrix is not invertible.
#' @keywords internal
#' @noRd
sage_kernel_estimate_original = function(
  A_mean,
  b_mean,
  block_estimates,
  block_size,
  n_pairs,
  total,
  require_result = FALSE
) {
  m = length(b_mean)
  # Near-singular sampled A (early checkpoints at tiny budgets) is treated like a
  # hard singularity, so the convergence history reports NA instead of wild values.
  if (rcond(A_mean) <= 1e-10) {
    # Not yet identifiable from the sampled coalitions. Fall back to the exact A only
    # when a value is required (final estimate); otherwise degrade to NA gracefully.
    phi = if (require_result) {
      cli::cli_warn(c(
        "The sampled kernel design matrix is singular at the end of the coalition budget.",
        "i" = "Falling back to the exact design matrix for the point estimate,
               which is the {.val unbiased} kernel variant.",
        "i" = "Standard errors are unavailable; increase {.arg n_coalitions}."
      ))
      sage_kernel_solve_constrained(solve(sage_kernel_A(m)), b_mean, total)
    } else {
      rep(NA_real_, m)
    }
    return(list(phi = phi, se = rep(NA_real_, m)))
  }
  phi = sage_kernel_solve_constrained(solve(A_mean), b_mean, total)

  se = rep(NA_real_, m)
  if (length(block_estimates) >= 2L) {
    block_var = apply(do.call(rbind, block_estimates), 2L, stats::var)
    se = sqrt(block_size * block_var / n_pairs)
  }
  list(phi = phi, se = se)
}

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

#' Adaptive default sampling budgets for the SAGE estimators
#'
#' Free functions rather than private methods to keep the R6 objects lean
#' (closures on R6 objects are serialized with them). The defaults adapt to the
#' feature count so the default cost never exceeds half of exact enumeration
#' (`2^m` coalition evaluations). The kernel default of `5 * m` draws targets
#' the same evaluation budget as the permutation default (`2 + 10m` vs
#' `1 + 10m` evaluations); being the more sample-efficient estimator, it
#' typically comes out more accurate at that shared cost. Documented in the
#' `n_permutations` / `n_coalitions` param docs of [SAGE].
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
  half_enum = if (m <= 30L) as.integer(2^(m - 2L)) else 512L # binds only for small m
  max(2L, min(512L, half_enum, 5L * m))
}

#' Point out when a sampling budget reaches exact-enumeration cost
#'
#' Emits a message when the resolved sampling budget costs at least as many
#' coalition evaluations as exact enumeration, which removes the
#' coalition-sampling error at the same or lower cost. A message rather than a
#' warning: oversampling can be deliberate (e.g. ConditionalSAGE, where
#' repeated evaluations average the conditional sampler's noise). Gated to
#' `m >= 3`, where the adaptive defaults stay below enumeration and the
#' absolute waste can be nontrivial.
#'
#' @param estimator (`character(1)`) `"permutation"` or `"kernel"`.
#' @param m (`integer(1)`) Number of features.
#' @param budget (`integer(1)`) Resolved `n_permutations` or `n_coalitions`.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
sage_inform_budget_vs_exact = function(estimator, m, budget) {
  if (!xplain_opt("verbose") || m < 3L || m > 30L) {
    return(invisible(NULL))
  }
  n_exact = 2^m
  evals = if (estimator == "permutation") 1 + budget * m else 2 + 2 * budget
  if (evals >= n_exact) {
    cli::cli_inform(
      c(
        "i" = "The {.val {estimator}} estimator will evaluate {evals} coalitions,
               at least as many as enumerating all {n_exact} (2^{m}) coalitions.",
        "i" = "{.code estimator = \"exact\"} computes SAGE values without coalition-sampling
               error at the same or lower cost (for {.cls ConditionalSAGE}, oversampling can
               still be deliberate; see the {.arg estimator} docs)."
      ),
      .frequency = "once",
      .frequency_id = paste0("xplainfi_sage_budget_", estimator)
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

#' Reconstruct the sampled Shapley-kernel design matrix from its off-diagonals
#'
#' The sampled `A = E[z z^T]` has an *exactly* constant diagonal of `0.5`: with paired
#' sampling every feature appears in exactly one of each (coalition, complement) pair, so
#' each column of the stacked `z` matrix sums to `n` over `2n` rows and its mean is `0.5`.
#' Only the off-diagonals are random, so the delta-method moment vector carries just those;
#' this helper rebuilds the symmetric `m x m` matrix (diagonal fixed at `0.5`) from them.
#'
#' @param a (`numeric`) Off-diagonal entries in lower-triangular (row > col) order.
#' @param m (`integer(1)`) Number of features.
#' @param od (`matrix`) Two-column index matrix of the lower-triangular positions,
#'   i.e. `which(lower.tri(matrix(0, m, m)), arr.ind = TRUE)`, passed in to avoid recomputing.
#' @return Symmetric `m x m` matrix with diagonal `0.5`.
#' @keywords internal
#' @noRd
sage_kernel_A_from_offdiag = function(a, m, od) {
  A = matrix(0, m, m)
  diag(A) = 0.5
  A[od] = a
  A[od[, 2:1, drop = FALSE]] = a # mirror to the upper triangle
  A
}

#' Constrained weighted least-squares Shapley solve
#'
#' The efficiency-constrained WLS solution shared by both kernel variants and the
#' delta-method Jacobian:
#' `phi = A^-1 b - A^-1 1 (1^T A^-1 b - total) / (1^T A^-1 1)`.
#' Kept as the single implementation so the point estimate and the function being
#' differentiated for its standard errors can never drift apart.
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
#' The "unbiased KernelSHAP" estimator (Covert & Lee 2021, Eq. 9): the design matrix `A` is
#' the exact closed form and only `b` is estimated from the sampled coalitions. This is the
#' estimator implemented by the reference Python `sage` package, and it admits the closed-form
#' covariance of their Eqs. 12-13: `Cov(phi) = C Cov(b_mean) C^T` with
#' `C = A^-1 - A^-1 1 1^T A^-1 / (1^T A^-1 1)`.
#' Note the minus sign: the `sage` implementation itself adds this term
#' (`calculate_result` in `kernel_estimator.py`), which contradicts the paper's Eq. 13 and was
#' verified by simulation to misstate the variance; the paper's formula is used here.
#' `total = v(full)` is treated as a fixed constraint (its Monte Carlo error from the anchor
#' evaluations is ignored, matching `sage`).
#'
#' @param b_mean (`numeric(m)`) Running mean of the per-pair `b` vector.
#' @param cov_mean (`matrix` | `NULL`) Covariance of `b_mean` (per-pair covariance divided by
#'   the number of pairs). If `NULL`, standard errors are returned as `NA`.
#' @param m (`integer(1)`) Number of features.
#' @param total (`numeric(1)`) Value of the grand coalition, `v(full)`, used as the sum constraint.
#' @param A_inv (`matrix` | `NULL`) Precomputed `solve(sage_kernel_A(m))`; it depends only on
#'   `m`, so callers evaluating repeatedly (convergence checkpoints) should pass it once.
#' @return `list(phi, se)`, each a `numeric(m)`.
#' @keywords internal
#' @noRd
sage_kernel_estimate_unbiased = function(b_mean, cov_mean, m, total, A_inv = NULL) {
  A_inv = A_inv %||% solve(sage_kernel_A(m))
  phi = sage_kernel_solve_constrained(A_inv, b_mean, total)

  se = rep(NA_real_, m)
  if (!is.null(cov_mean)) {
    A_inv_1 = as.numeric(A_inv %*% rep(1, m))
    C = A_inv - outer(A_inv_1, A_inv_1) / sum(A_inv_1)
    se = sqrt(pmax(diag(C %*% cov_mean %*% t(C)), 0))
  }
  list(phi = phi, se = se)
}

#' Original kernel SAGE estimate and delta-method standard errors
#'
#' The "original KernelSHAP" estimator (Covert & Lee 2021, Eq. 7): both the design matrix `A`
#' and `b` are estimated from the same sampled coalitions. Sharing the samples couples their
#' errors, which cancel in the ratio `A^-1 b`, so this estimator converges much faster than the
#' unbiased variant on (near-)additive games, and the paper's Section 4.1 recommends it in
#' practice for this reason. It falls back to the exact `A` only when the sampled one is not
#' yet full rank.
#'
#' Given the running mean `w_mean` of the per-pair moment vector `w = (offdiag(A), b)` and
#' (optionally) the covariance `cov_mean` of that mean, compute the constrained weighted
#' least-squares Shapley values and their standard errors. Because the estimate
#' `phi = g(A, b)` is a smooth function of the sample-mean moments, its covariance follows from
#' the multivariate delta method, `Cov(phi) = J cov_mean J^T`, with `J` the Jacobian of the
#' constrained solve. For fixed `A` the solve is affine in `b`, so the `b` block of `J` is
#' exactly the projection matrix `C` of the unbiased variant; only the `A` off-diagonal block
#' needs central differences. This propagates the sampling variance of *both* `A` and `b`
#' (and their shared-sample covariance), extending the closed form of the unbiased variant.
#' `total = v(full)` is treated as a fixed constraint (its Monte Carlo error from the anchor
#' evaluations is ignored, matching `sage`).
#'
#' @param w_mean (`numeric`) Running mean of the moment vector, off-diagonals of `A` first
#'   (in `od` order) then the `m` entries of `b`.
#' @param cov_mean (`matrix` | `NULL`) Covariance of `w_mean` (i.e. per-pair covariance divided
#'   by the number of pairs). If `NULL`, standard errors are returned as `NA`.
#' @param m (`integer(1)`) Number of features.
#' @param total (`numeric(1)`) Value of the grand coalition, `v(full)`, used as the sum constraint.
#' @param od (`matrix`) Lower-triangular index matrix (see `sage_kernel_A_from_offdiag`).
#' @param noff (`integer(1)`) Number of off-diagonal entries, `m * (m - 1) / 2`.
#' @param require_result (`logical(1)`) If `TRUE`, fall back to the exact `A` (with a warning)
#'   when the sampled `A` is singular so a finite point estimate is always returned.
#' @return `list(phi, se)`, each a `numeric(m)`. `se` is all-`NA` when `cov_mean` is `NULL` or
#'   the (perturbed) design matrix is not invertible.
#' @keywords internal
#' @noRd
sage_kernel_estimate_original = function(w_mean, cov_mean, m, total, od, noff, require_result = FALSE) {
  b = w_mean[noff + seq_len(m)]

  A = sage_kernel_A_from_offdiag(w_mean[seq_len(noff)], m, od)
  # Near-singular sampled A (early checkpoints at tiny budgets) is treated like a
  # hard singularity, so the convergence history reports NA instead of wild values.
  A_inv = if (rcond(A) > 1e-10) tryCatch(solve(A), error = function(e) NULL) else NULL
  if (is.null(A_inv)) {
    # Not yet identifiable from the sampled coalitions. Fall back to the exact A only
    # when a value is required (final estimate); otherwise degrade to NA gracefully.
    phi = if (require_result) {
      cli::cli_warn(c(
        "The sampled kernel design matrix is singular at the end of the coalition budget.",
        "i" = "Falling back to the exact design matrix for the point estimate,
               which is the {.val unbiased} kernel variant.",
        "i" = "Standard errors are unavailable; increase {.arg n_coalitions}."
      ))
      sage_kernel_solve_constrained(solve(sage_kernel_A(m)), b, total)
    } else {
      rep(NA_real_, m)
    }
    return(list(phi = phi, se = rep(NA_real_, m)))
  }
  phi = sage_kernel_solve_constrained(A_inv, b, total)

  se = rep(NA_real_, m)
  if (!is.null(cov_mean)) {
    # b block of the Jacobian: the solve is affine in b for fixed A, so
    # dphi/db is exactly the projection matrix C (no differencing needed).
    A_inv_1 = as.numeric(A_inv %*% rep(1, m))
    C = A_inv - outer(A_inv_1, A_inv_1) / sum(A_inv_1)

    # A block: phi as a function of the off-diagonals, for central differences.
    phi_of_a = function(a) {
      Aw = sage_kernel_A_from_offdiag(a, m, od)
      if (rcond(Aw) <= 1e-10) {
        return(NULL)
      }
      Aw_inv = tryCatch(solve(Aw), error = function(e) NULL)
      if (is.null(Aw_inv)) {
        return(NULL)
      }
      sage_kernel_solve_constrained(Aw_inv, b, total)
    }
    J = matrix(0, m, noff + m)
    J[, noff + seq_len(m)] = C
    a_mean = w_mean[seq_len(noff)]
    h = 1e-6
    ok = TRUE
    for (j in seq_len(noff)) {
      ap = a_mean
      am = a_mean
      ap[j] = ap[j] + h
      am[j] = am[j] - h
      up = phi_of_a(ap)
      dn = phi_of_a(am)
      if (is.null(up) || is.null(dn)) {
        ok = FALSE
        break
      }
      J[, j] = (up - dn) / (2 * h)
    }
    if (ok) {
      phi_cov = J %*% cov_mean %*% t(J)
      se = sqrt(pmax(diag(phi_cov), 0))
    }
  }
  list(phi = phi, se = se)
}

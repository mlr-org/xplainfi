#' @title Shapley Additive Global Importance (SAGE) Base Class
#'
#' @description Base class for SAGE (Shapley Additive Global Importance)
#' feature importance based on Shapley values with marginalization.
#' This is an abstract class - use [MarginalSAGE] or [ConditionalSAGE].
#'
#' @details
#' SAGE uses Shapley values to fairly distribute the total prediction
#' performance among all features. Unlike perturbation-based methods,
#' SAGE marginalizes features by integrating over their distribution.
#' This is approximated by averaging predictions over a reference dataset.
#'
#' SAGE values are reductions in the measure's score relative to the empty coalition,
#' `score(empty) - score(S)`, so that positive values mean the feature improves performance.
#' For measures that are maximized (`measure$minimize = FALSE`, e.g. `classif.acc`) the scores are
#' negated internally, so the sign convention is the same for all measures.
#'
#' **Standard errors**: The standard errors reported in `$convergence_history` and `$convergence()` are
#' Monte Carlo standard errors of the Shapley *estimator*: how much the estimates would still move if
#' more permutations or coalitions were sampled, for the fixed trained model, the fixed test set, and (for
#' [MarginalSAGE]) the fixed reference subsample.
#' They are convergence diagnostics in the sense of Covert & Lee (2021, Section 4.3), not inference about
#' feature importance: they say nothing about variability across train/test splits or refits (see the
#' resampling-based `ci_method`s of `$importance()` for that), and a feature's SAGE value being several
#' SEs from zero only means the computation has converged, not that the feature matters.
#' For the permutation estimator the SE is the sample standard error of the per-permutation marginal
#' contributions.
#' For the kernel estimator it follows from the multivariate central limit theorem for the estimated
#' regression targets, `Cov(phi) = C Cov(b) C^T / n` (their Eqs. 10-13), so it covers the coalition draws
#' and the test observations they are paired with (both are sampled with replacement from the fixed test
#' set).
#' The exact estimator has no coalition-sampling error and reports no SE.
#'
#' **Estimators**: `estimator = "permutation"` (the default) is the permutation-sampling estimator of
#' Covert et al. (2020), budgeted by `n_permutations`.
#' `estimator = "kernel"` is the regression-based estimator of Covert & Lee (2021), budgeted by
#' `n_coalitions`: Shapley values are the solution of a weighted least squares problem, approximated from
#' sampled coalitions (see below).
#' `estimator = "exact"` enumerates all `2^n_features` coalitions and computes the Shapley values in closed
#' form, so it has no coalition-sampling error and serves as a ground-truth reference for the sampling
#' estimators on small feature sets (capped by `max_features`).
#' "Exact" refers to coalition sampling only: the marginalization error controlled by `n_samples` remains,
#' and for [ConditionalSAGE] the value function itself is a Monte Carlo estimate of the sampler.
#'
#' **Kernel estimator**: This implements the unbiased KernelSHAP estimator of Covert & Lee (2021, Eq. 9)
#' for the stochastic cooperative game of SAGE, mirroring the `KernelEstimator` of the reference Python
#' `sage` package.
#' Coalitions are drawn from the Shapley kernel (size `k` with probability proportional to `1 / (k (p - k))`,
#' uniform within size) together with their complement (paired sampling, their Section 4.2), and each
#' draw is paired with a single test observation drawn with replacement, whose observation-wise loss is
#' the value-function sample.
#' Consequently the kernel estimator requires a measure with an observation-wise loss
#' (`"obs_loss"` in `measure$properties`, e.g. `regr.mse` or `classif.logloss`, but not `classif.auc`), and
#' each coalition evaluation costs `n_samples` model rows rather than `n_test * n_samples` as for the other
#' estimators, which evaluate every coalition on the whole test set.
#' For such measures the kernel estimator targets the same SAGE values as the permutation and exact
#' estimators (the Shapley value is linear in the value function, and the test-set loss is the mean of the
#' observation-wise losses), so `estimator = "exact"` remains the reference; `$budget$n_rows` compares
#' their costs in model rows.
#' The kernel estimator is currently available for [MarginalSAGE] only.
#'
#' **Convergence and budget**: With `early_stopping = TRUE`, sampling stops once the largest SE,
#' relative to the spread of the SAGE values (`max(se) / (max(phi) - min(phi))`), falls below
#' `se_threshold`.
#' This is the criterion of the reference Python `sage` package.
#' This applies to both sampling estimators; the exact estimator has no criterion.
#' The budget argument (`n_permutations` or `n_coalitions`) then acts as an upper bound rather than a planned cost:
#' exhausting it without meeting the criterion returns the values with a warning.
#' `$budget` reports what was actually spent and whether the criterion was met, and
#' `$plot_convergence()` shows the trajectory that led there.
#' Under resampling, only the first iteration runs the criterion and the remaining iterations
#' reuse its budget, which keeps them comparable and avoids re-deriving the standard errors in
#' every iteration.
#'
#' @references
#' `r print_bib("lundberg_2020")`
#'
#' `r print_bib("covert_2021")`
#'
#' @seealso [MarginalSAGE] [ConditionalSAGE]
#'
#' @export
SAGE = R6Class(
  "SAGE",
  inherit = FeatureImportanceMethod,
  public = list(
    #' @field convergence_history ([`data.table`][data.table::data.table]) History of SAGE values during computation.
    #'   Columns `budget` (sampling effort in the estimator's own units), `n_evals` (the corresponding
    #'   number of evaluated coalitions), and `n_rows` (model rows predicted) index the checkpoints; see `$budget`.
    convergence_history = NULL,
    #' @field converged (`logical(1)`) Whether the convergence criterion was met (`early_stopping = TRUE`).
    #'   `NA` for the exact estimator, which enumerates all coalitions and has no criterion to meet.
    converged = FALSE,

    #' @description
    #' Creates a new instance of the SAGE class.
    #' @param task,learner,measure,resampling,features Passed to FeatureImportanceMethod.
    #' @param estimator (`character(1)`: `"permutation"`) Shapley-value estimator.
    #'   `"permutation"` is the permutation-sampling estimator of Covert et al. (2020), budgeted by
    #'   `n_permutations`; `"kernel"` is the regression-based estimator of Covert & Lee (2021), budgeted by
    #'   `n_coalitions`; `"exact"` enumerates all `2^n_features` coalitions (capped by `max_features`) and
    #'   takes no budget.
    #'   All approximate the same SAGE values; setting the budget argument of a different estimator is an
    #'   error.
    #'   Their costs are comparable through `$budget$n_rows`, the number of model rows predicted; see Details
    #'   for why the kernel estimator's coalition evaluations are cheaper than the others'.
    #'   `$compute()` points out in a message (silenced by `xplain_opt(verbose = FALSE)`) when the sampling
    #'   budget meets or exceeds the exact estimator's cost, since enumeration then removes the
    #'   coalition-sampling error at no extra cost.
    #' @param n_permutations (`integer(1)`: `NULL`) Number of permutations for `estimator = "permutation"`.
    #'   Each permutation evaluates one coalition per feature, so the cost is `1 + n_permutations * n_features`
    #'   evaluated coalitions.
    #'   If unset, defaults to `10L`.
    #' @param n_coalitions (`integer(1)`: `NULL`) Number of paired coalition draws for `estimator = "kernel"`.
    #'   Each draw evaluates a coalition and its complement on one test observation, so the cost is
    #'   `2 + 2 * n_coalitions` evaluated coalitions.
    #'   If unset, defaults to `2048L`.
    #'   Check whether the budget suffices with `$convergence()` or `$plot_convergence()`, or let
    #'   `early_stopping` decide.
    #' @param max_features (`integer(1)`: `12L`) Cap on the number of features for `estimator = "exact"`,
    #'   whose cost grows as `2^n_features`; construction aborts above it.
    #' @param batch_size (`integer(1)`: `5000L`) Maximum number of observations to process in a single prediction call.
    #' @param n_samples (`integer(1)`: `100L`) Number of samples to use for marginalizing out-of-coalition features.
    #'   For [MarginalSAGE], this is the number of marginal data samples ("background data" in other implementations).
    #'   For [ConditionalSAGE], this is the number of conditional samples per test instance retrieved from `sampler`.
    #' @param early_stopping (`logical(1)`: `FALSE`) Whether to stop once the convergence criterion is met,
    #'   rather than spending the full budget.
    #'   Applies to the permutation and kernel estimators; setting it for `estimator = "exact"` is a warning.
    #'   The budget then acts as an upper bound: if the criterion is not met within it, the values are
    #'   returned with a warning and `$budget` reports `converged = FALSE`.
    #' @param se_threshold (`numeric(1)`: `0.025`) Convergence threshold for relative standard error.
    #'   Convergence is detected when the maximum relative SE across all features falls below this threshold.
    #'   Relative SE is calculated as SE divided by the range of importance values (max - min),
    #'   making it scale-invariant across different loss metrics.
    #'   The default of `0.025` (convergence once the relative SE is below 2.5% of the importance range) is
    #'   the default of the Python `sage` package; the examples in Covert et al. (2020) and Covert & Lee (2021)
    #'   use `0.01` to `0.02`.
    #'   The same threshold buys different budgets across estimators, since their standard errors are
    #'   constructed differently (see Details).
    #' @param min_permutations (`integer(1)`: `10L`) Minimum permutations before checking for convergence.
    #'   Convergence is judged based on the standard errors of the estimated SAGE values,
    #'   which requires a sufficiently large number of samples (i.e., evaluated coalitions).
    #'   Permutation estimator only; the kernel estimator checks after every chunk of 512 draws.
    #' @param check_interval (`integer(1)`: `1L`) Check convergence every N permutations.
    #'   Permutation estimator only.
    initialize = function(
      task,
      learner,
      measure = NULL,
      resampling = NULL,
      features = NULL,
      estimator = c("permutation", "kernel", "exact"),
      n_permutations = NULL,
      n_coalitions = NULL,
      max_features = 12L,
      batch_size = 5000L,
      n_samples = 100L,
      early_stopping = FALSE,
      se_threshold = 0.025,
      min_permutations = 10L,
      check_interval = 1L
    ) {
      super$initialize(
        task = task,
        learner = learner,
        measure = measure,
        resampling = resampling,
        features = features,
        label = "Shapley Additive Global Importance"
      )

      estimator = match.arg(estimator)
      checkmate::assert_int(max_features, lower = 1L)
      checkmate::assert_flag(early_stopping)
      checkmate::assert_number(se_threshold, lower = 0, upper = 1)
      checkmate::assert_int(min_permutations, lower = 1L)
      checkmate::assert_int(check_interval, lower = 1L)

      # Each estimator takes its own budget argument; setting another estimator's budget
      # is a misunderstanding rather than a value to ignore.
      if (estimator == "permutation") {
        if (!is.null(n_coalitions)) {
          cli::cli_abort(c(
            "{.arg n_coalitions} is only valid for {.code estimator = \"kernel\"}.",
            "i" = "The permutation estimator is controlled by {.arg n_permutations}."
          ))
        }
        n_permutations = checkmate::assert_int(n_permutations %||% 10L, lower = 1L)
      } else if (estimator == "kernel") {
        if (!is.null(n_permutations)) {
          cli::cli_abort(c(
            "{.arg n_permutations} is only valid for {.code estimator = \"permutation\"}.",
            "i" = "The kernel estimator is controlled by {.arg n_coalitions}."
          ))
        }
        n_coalitions = checkmate::assert_int(n_coalitions %||% 2048L, lower = 1L)
        # The kernel estimator's value-function samples are observation-wise losses.
        if (!has_obs_loss(self$measure)) {
          cli::cli_abort(c(
            "The kernel estimator requires a measure with an observation-wise loss.",
            "i" = "Measure {.val {self$measure$id}} has no {.val obs_loss} property;
                   use e.g. {.code msr(\"regr.mse\")} or {.code msr(\"classif.logloss\")}."
          ))
        }
      } else {
        if (!is.null(n_permutations) || !is.null(n_coalitions)) {
          cli::cli_abort(c(
            "The exact estimator enumerates all coalitions and takes no sampling budget.",
            "i" = "Do not set {.arg n_permutations} or {.arg n_coalitions} with {.code estimator = \"exact\"}."
          ))
        }
        sage_assert_exact_budget(length(self$features), max_features)
      }
      # Convergence knobs do not change what is estimated, so a non-default value for an
      # estimator that ignores them is only a warning. early_stopping and se_threshold drive
      # both sampling estimators, the other two only the permutation estimator's checkpointing.
      non_default = c(
        early_stopping = early_stopping && estimator == "exact",
        se_threshold = !identical(se_threshold, 0.025) && estimator == "exact",
        min_permutations = !identical(as.integer(min_permutations), 10L) && estimator != "permutation",
        check_interval = !identical(as.integer(check_interval), 1L) && estimator != "permutation"
      )
      if (any(non_default)) {
        cli::cli_warn(
          "{.arg {names(non_default)[non_default]}} {?is/are} ignored by {.code estimator = \"{estimator}\"}."
        )
      }
      if (estimator != "exact" && !identical(as.integer(max_features), 12L)) {
        cli::cli_warn("{.arg max_features} only applies to {.code estimator = \"exact\"} and is ignored.")
      }

      # For classification tasks, require predict_type = "prob"
      if (self$task$task_type == "classif") {
        if (learner$predict_type != "prob") {
          cli::cli_abort(c(
            "Classification learners require probability predictions for SAGE.",
            "i" = "Please set {.code learner$configure(predict_type = \"prob\")} before using SAGE."
          ))
        }
      }

      # Set parameters
      ps = ps(
        estimator = paradox::p_fct(levels = c("permutation", "kernel", "exact"), default = "permutation"),
        n_permutations = paradox::p_int(lower = 1L, default = 10L),
        n_coalitions = paradox::p_int(lower = 1L, default = 2048L),
        max_features = paradox::p_int(lower = 1L, default = 12L),
        batch_size = paradox::p_int(lower = 1L, default = 5000L),
        n_samples = paradox::p_int(lower = 1L, default = 100L),
        early_stopping = paradox::p_lgl(default = FALSE),
        se_threshold = paradox::p_dbl(lower = 0, upper = 1, default = 0.025),
        min_permutations = paradox::p_int(lower = 1L, default = 10L),
        check_interval = paradox::p_int(lower = 1L, default = 1L)
      )
      # Only the knobs the estimator reads are stored, so $param_set$values documents
      # what actually applies.
      ps$values$estimator = estimator
      if (estimator == "permutation") {
        ps$values$n_permutations = n_permutations
        ps$values$early_stopping = early_stopping
        ps$values$se_threshold = se_threshold
        ps$values$min_permutations = min_permutations
        ps$values$check_interval = check_interval
      } else if (estimator == "kernel") {
        ps$values$n_coalitions = n_coalitions
        ps$values$early_stopping = early_stopping
        ps$values$se_threshold = se_threshold
      } else {
        ps$values$max_features = max_features
      }
      ps$values$batch_size = batch_size
      ps$values$n_samples = n_samples
      self$param_set = ps
      # The exact estimator has no criterion to meet, so `converged` is NA rather than FALSE.
      if (estimator == "exact") {
        self$converged = NA
      }
    },

    #' @description
    #' Compute SAGE values.
    #' @param store_backends (`logical(1)`) Whether to store data backends.
    #' @param batch_size (`integer(1)`: `5000L`) Maximum number of observations to process in a single prediction call.
    #' @param early_stopping (`logical(1)`: `FALSE`) Whether to check for convergence and stop early.
    #' @param se_threshold (`numeric(1)`: `0.025`) Convergence threshold for relative standard error.
    #'   SE is normalized by the range of importance values (max - min) to make convergence
    #'   detection scale-invariant. Default `0.025` means convergence when relative SE < 2.5%.
    #' @param min_permutations (`integer(1)`: `10L`) Minimum permutations before checking convergence.
    #' @param check_interval (`integer(1)`: `1L`) Check convergence every N permutations.
    #'   The convergence arguments only apply to `estimator = "permutation"`; passing them for another
    #'   estimator is a warning.
    compute = function(
      store_backends = TRUE,
      batch_size = NULL,
      early_stopping = NULL,
      se_threshold = NULL,
      min_permutations = NULL,
      check_interval = NULL
    ) {
      # Read from the param_set so post-construction edits via $param_set$values apply.
      estimator = self$param_set$values$estimator %||% "permutation"

      # Reset convergence tracking
      self$convergence_history = NULL
      self$converged = if (estimator == "exact") NA else FALSE
      private$.budget_used = NULL
      private$.n_test = NULL
      m = length(self$features)
      passed = c(
        early_stopping = !is.null(early_stopping) && estimator == "exact",
        se_threshold = !is.null(se_threshold) && estimator == "exact",
        min_permutations = !is.null(min_permutations) && estimator != "permutation",
        check_interval = !is.null(check_interval) && estimator != "permutation"
      )
      if (any(passed)) {
        cli::cli_warn(
          "{.arg {names(passed)[passed]}} {?is/are} ignored by {.code estimator = \"{estimator}\"}."
        )
      }
      if (estimator == "exact") {
        sage_assert_exact_budget(m, self$param_set$values$max_features %||% 12L)
      }

      # Resolve parameters using hierarchical resolution
      batch_size = resolve_param(batch_size, self$param_set$values$batch_size, 5000L)
      early_stopping = resolve_param(
        early_stopping,
        self$param_set$values$early_stopping,
        FALSE
      )
      se_threshold = resolve_param(
        se_threshold,
        self$param_set$values$se_threshold,
        0.025
      )
      min_permutations = resolve_param(
        min_permutations,
        self$param_set$values$min_permutations,
        10L
      )
      check_interval = resolve_param(check_interval, self$param_set$values$check_interval, 1L)

      # With early stopping the budget is an upper bound, so pointing at exact enumeration
      # only makes sense for a budget that is actually planned to be exhausted.
      if (estimator == "permutation") {
        sage_inform_budget_vs_exact(m, self$param_set$values$n_permutations, early_stopping)
      }

      # Initial resampling to get trained learners
      rr = assemble_rr(
        task = self$task,
        learner = self$learner,
        resampling = self$resampling,
        store_models = TRUE,
        store_backends = store_backends
      )
      # Store results
      self$resample_result = rr

      # For convergence tracking, we'll use the first resampling iteration
      # (convergence is about permutation count, not resampling)
      iter_for_convergence = 1L

      # Estimator dispatch. All estimators return the same list(scores, convergence_data)
      # shape, so the resampling aggregation below is estimator-agnostic. Only the first
      # iteration tracks convergence and may stop early; the remaining iterations reuse
      # the budget it actually spent, which keeps them comparable.
      score_iter = function(learner, test_dt, track_convergence) {
        if (estimator == "exact") {
          private$.compute_sage_scores_exact(learner = learner, test_dt = test_dt, batch_size = batch_size)
        } else if (estimator == "kernel") {
          private$.compute_sage_scores_kernel(
            learner = learner,
            test_dt = test_dt,
            n_coalitions = if (track_convergence) self$param_set$values$n_coalitions else private$.budget_used,
            batch_size = batch_size,
            early_stopping = track_convergence && early_stopping,
            se_threshold = se_threshold
          )
        } else {
          private$.compute_sage_scores(
            learner = learner,
            test_dt = test_dt,
            n_permutations = if (track_convergence) self$param_set$values$n_permutations else private$.budget_used,
            batch_size = batch_size,
            early_stopping = track_convergence && early_stopping,
            se_threshold = se_threshold,
            min_permutations = min_permutations,
            check_interval = check_interval
          )
        }
      }

      # Compute SAGE values for convergence tracking (first iteration)
      test_dt_first = self$task$data(rows = rr$resampling$test_set(iter_for_convergence))
      private$.n_test = nrow(test_dt_first)
      first_result = score_iter(
        learner = rr$learners[[iter_for_convergence]],
        test_dt = test_dt_first,
        track_convergence = TRUE
      )

      # Extract convergence data from first iteration
      # `convergence_data` exists even if early_stopping = FALSE
      self$convergence_history = first_result$convergence_data$convergence_history
      self$converged = first_result$convergence_data$converged
      private$.budget_used = first_result$convergence_data$budget_used

      # If we have multiple resampling iterations, compute the rest without convergence tracking
      if (self$resampling$iters > 1) {
        remaining_results = lapply(seq_len(self$resampling$iters)[-iter_for_convergence], \(iter) {
          score_iter(
            learner = rr$learners[[iter]],
            test_dt = self$task$data(rows = rr$resampling$test_set(iter)),
            track_convergence = FALSE
          )
        })

        # Extract scores from all results (always list format now)
        all_scores = c(list(first_result$scores), lapply(remaining_results, function(x) x$scores))
      } else {
        all_scores = list(first_result$scores)
      }

      # Combine results across resampling iterations
      scores = rbindlist(all_scores, idcol = "iter_rsmp")

      # iter_rsmp, feature, importance -- score_baseline or so don't apply here
      private$.scores = scores
    },

    #' @description
    #' Resets all stored fields populated by `$compute()`, including the convergence tracking
    #' (`$convergence_history`, `$converged`, `$budget`).
    reset = function() {
      super$reset()
      self$convergence_history = NULL
      self$converged = if (identical(self$param_set$values$estimator, "exact")) NA else FALSE
      private$.budget_used = NULL
      private$.n_test = NULL
    },

    #' @description
    #' Monte Carlo standard errors of the final SAGE estimates, i.e. the last checkpoint of
    #' `$convergence_history`, together with the convergence ratio `max(se) / (max(importance) - min(importance))`
    #' that `early_stopping` compares against `se_threshold`.
    #' These quantify how converged the computation is for the fixed model, not feature importance;
    #' see the *Standard errors* section in Details.
    #' @return A [`data.table`][data.table::data.table] with columns `feature`, `importance`, `se`, and `ratio`
    #'   (the same value in every row), or `NULL` before `$compute()` and for the exact estimator.
    convergence = function() {
      history = self$convergence_history
      if (is.null(history)) {
        return(NULL)
      }
      budget = importance = se = ratio = NULL # data.table NSE NOTE tax
      last = history[budget == max(budget), list(feature, importance, se)]
      last[, ratio := sage_convergence_ratio(importance, se)][]
    },

    #' @description
    #' Plot convergence history of SAGE values.
    #' @param features (`character` | `NULL`) Features to plot. If NULL, plots all features.
    #' @return A [ggplot2][ggplot2::ggplot] object
    plot_convergence = function(features = NULL) {
      require_package("ggplot2")

      if (identical(self$param_set$values$estimator, "exact")) {
        cli::cli_abort(c(
          "Convergence tracking is not applicable to the exact estimator.",
          "i" = "The exact estimator enumerates all coalitions, so there is no iterative convergence to plot."
        ))
      }
      if (is.null(self$convergence_history)) {
        cli::cli_abort("No convergence history available. Run $compute() first.")
      }

      # Create a copy to avoid modifying the original
      plot_data = copy(self$convergence_history)

      if (!is.null(features)) {
        plot_data = plot_data[feature %in% features]
      }

      # Not named `budget`: the x aesthetic below refers to the history column of that name.
      budget_row = self$budget
      has_se = !all(is.na(plot_data$se))

      p = ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = budget, y = importance, fill = feature, color = feature)
      )
      if (has_se) {
        p = p +
          ggplot2::geom_ribbon(
            ggplot2::aes(ymin = importance - se, ymax = importance + se),
            alpha = 1 / 3
          )
      }
      p = p +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_point(size = 2) +
        ggplot2::labs(
          title = "SAGE Value Convergence",
          subtitle = if (isTRUE(self$converged)) {
            sprintf(
              "Converged after %g %s (saved %g)",
              budget_row$used,
              budget_row$unit,
              budget_row$requested - budget_row$used
            )
          } else {
            sprintf("Completed all %g %s", budget_row$used, budget_row$unit)
          },
          x = if (identical(budget_row$estimator, "kernel")) "Number of Coalition Draws" else "Number of Permutations",
          y = "SAGE Value",
          color = "Feature",
          fill = "Feature"
        ) +
        ggplot2::theme_minimal(base_size = 14)

      if (isTRUE(self$converged)) {
        p = p +
          ggplot2::geom_vline(
            xintercept = budget_row$used,
            linetype = "dashed",
            color = "red",
            alpha = 0.5
          )
      }

      p
    }
  ),

  active = list(
    #' @field budget ([`data.table`][data.table::data.table]) Read-only one-row summary of the sampling
    #'   effort: the `estimator`, its `unit` of budget, the `requested` upper bound, the amount `used`
    #'   (below the request only with early stopping), the resulting number of coalition evaluations
    #'   `n_evals` (one empty-coalition baseline plus `n_features` per permutation; two anchors plus two per
    #'   coalition draw for the kernel estimator; `2^n_features` for the exact estimator), the number of model
    #'   rows predicted `n_rows`, and whether the computation `converged`.
    #'   `n_evals` counts coalition evaluations, which differ in cost between estimators (the kernel
    #'   estimator evaluates a coalition on one test observation, the others on the whole test set), so
    #'   `n_rows` is the unit in which estimators are comparable.
    #'   `used`, `n_evals`, and `n_rows` are `NA` before `$compute()`; `converged` is `NA` for the exact
    #'   estimator, which has no criterion to meet.
    #'   With multiple resampling iterations it describes the first iteration, whose budget the
    #'   remaining ones reuse (see `early_stopping`).
    budget = function(rhs) {
      if (!missing(rhs)) {
        cli::cli_abort("{.field $budget} is read-only; set the budget via {.code $param_set$values}.")
      }
      estimator = self$param_set$values$estimator %||% "permutation"
      values = self$param_set$values
      m = length(self$features)
      used = private$.budget_used
      data.table(
        estimator = estimator,
        unit = switch(estimator, permutation = "permutations", kernel = "coalition draws", exact = "coalitions"),
        requested = switch(
          estimator,
          permutation = as.numeric(values$n_permutations),
          kernel = as.numeric(values$n_coalitions),
          exact = 2^m
        ),
        used = as.numeric(used %||% NA_real_),
        n_evals = if (is.null(used)) NA_real_ else sage_n_evals(estimator, m, used),
        n_rows = if (is.null(used)) NA_real_ else sage_n_rows(estimator, m, used, private$.n_test, values$n_samples),
        converged = self$converged
      )
    },

    #' @field n_permutations_used Defunct.
    #'   Use `$budget` instead, which reports the effort spent alongside its unit and the implied
    #'   number of coalition evaluations.
    n_permutations_used = function(rhs) {
      cli::cli_abort(c(
        "The {.field n_permutations_used} field is defunct.",
        "i" = "Read the effort spent via {.code $budget} instead, which also reports its unit."
      ))
    },

    #' @field n_permutations (`integer(1)`) Deprecated.
    #'   The permutation budget lives in the param_set; use `$param_set$values$n_permutations` instead.
    #'   This alias is kept for backward compatibility with the field of the same name in
    #'   earlier releases and warns on every access.
    n_permutations = function(rhs) {
      if (missing(rhs)) {
        cli::cli_warn(c(
          "The {.field n_permutations} field is deprecated.",
          "i" = "Read it via {.code $param_set$values$n_permutations} instead."
        ))
        return(self$param_set$values$n_permutations)
      }
      cli::cli_warn(c(
        "The {.field n_permutations} field is deprecated.",
        "i" = "Set it via {.code $param_set$values$n_permutations} instead."
      ))
      if (!identical(self$param_set$values$estimator, "permutation")) {
        cli::cli_abort("{.arg n_permutations} is only valid for {.code estimator = \"permutation\"}.")
      }
      self$param_set$values$n_permutations = checkmate::assert_int(rhs, lower = 1L)
    }
  ),

  private = list(
    # Sampling effort spent by the first resampling iteration, in the estimator's own units.
    # Surfaced via $budget; also the budget the remaining iterations reuse after early stopping.
    .budget_used = NULL,
    # Test-set size of the first resampling iteration, for $budget$n_rows.
    .n_test = NULL,

    # This function computes the SAGE values for a single resampling iteration.
    # It iterates through permutations of features, evaluates coalitions, and calculates marginal contributions.
    .compute_sage_scores = function(
      learner,
      test_dt,
      n_permutations,
      batch_size = NULL,
      early_stopping = FALSE,
      se_threshold = 0.025,
      min_permutations = 10L,
      check_interval = 1L
    ) {
      # Initialize numeric vectors to store marginal contributions and their squares for variance calculation.
      # We track both sum and sum of squares to calculate running variance and standard errors.
      sage_values = numeric(length(self$features)) # Sum of marginal contributions
      sage_values_sq = numeric(length(self$features)) # Sum of squared marginal contributions
      names(sage_values) = self$features
      names(sage_values_sq) = self$features

      # Pre-generate `n_permutations` permutations upfront
      # Relevant for reproducibility, especially when using early stopping or parallel processing.
      # Example: if self$features = c("x1", "x2", "x3") and n_permutations = 2,
      # all_permutations might be list(c("x2", "x1", "x3"), c("x3", "x1", "x2"))
      all_permutations = replicate(n_permutations, sample(self$features), simplify = FALSE)

      # Initialize variables for iterative checkpoint-based computation.
      # This allows for early stopping based on convergence and provides progress updates.
      convergence_history = list() # Stores SAGE values at each checkpoint for convergence tracking
      n_completed = 0 # Number of permutations processed so far
      converged = FALSE # Flag to indicate if convergence has been detected
      baseline_loss = NULL # Loss of the empty coalition (model with no features / all features marginalized)

      # Calculate total checkpoints for progress tracking.
      # A checkpoint is a group of 'check_interval' permutations.
      total_checkpoints = ceiling(n_permutations / check_interval)
      current_checkpoint = 0

      # Start checkpoint-based progress bar if progress display is enabled.
      if (xplain_opt("progress")) {
        cli::cli_progress_bar(
          "Computing SAGE values",
          total = total_checkpoints
        )
      }

      # Main loop: Process permutations in checkpoints until all permutations are done or convergence is reached.
      while (n_completed < n_permutations && !converged) {
        # Determine the size of the current checkpoint.
        # This ensures that the last checkpoint processes only the remaining permutations.
        checkpoint_size = min(check_interval, n_permutations - n_completed)
        # Define the indices of permutations to be processed in this checkpoint.
        checkpoint_perms = (n_completed + 1):(n_completed + checkpoint_size)

        # Get the actual permutation sequences for this checkpoint from the pre-generated list.
        checkpoint_permutations = all_permutations[checkpoint_perms]

        # Build this checkpoint's growing-prefix coalitions. The
        # empty coalition is prepended only in the first checkpoint;
        # its loss is the baseline anchor for marginal contributions.
        # Same single-batch call/order as before, so the RNG-bearing
        # marginal sampling inside .evaluate_coalitions_batch is
        # byte-identical to the pre-refactor scheme.
        checkpoint_coalitions = sage_growing_coalitions(checkpoint_permutations)
        offset = 0L
        if (n_completed == 0) {
          checkpoint_coalitions = c(list(character(0)), checkpoint_coalitions)
          offset = 1L
        }

        # Progress: one tick per checkpoint (unchanged cadence).
        current_checkpoint = current_checkpoint + 1

        # Evaluate all coalitions collected in this checkpoint in a single batch.
        # This is a performance optimization to minimize prediction calls to the learner.
        checkpoint_losses = private$.evaluate_coalitions_batch(
          learner,
          test_dt,
          checkpoint_coalitions,
          batch_size
        )

        # Update progress bar.
        if (xplain_opt("progress")) {
          cli::cli_progress_update(inc = 1)
        }

        # Store the baseline loss (loss of the empty coalition) from the first checkpoint.
        # This is the model's performance when no features are available.
        if (n_completed == 0) {
          baseline_loss = checkpoint_losses[1] # The first element is always the empty coalition's loss
        }

        # Closed-form accumulation over the growing-prefix losses.
        # Every permutation is a full feature permutation, so each
        # coalition's loss index is computed directly; `offset` skips
        # the leading empty-coalition slot present in the first
        # checkpoint. Replaces the former O(n^2) which(sapply())
        # coalition-map lookup.
        acc = sage_marginal_contributions(
          checkpoint_permutations,
          checkpoint_losses,
          baseline_loss,
          self$features,
          offset = offset
        )
        # Name-aligned add (defensive: positional add is only valid if
        # orders match; reindex by name to be safe).
        sage_values = sage_values + acc$sv[names(sage_values)]
        sage_values_sq = sage_values_sq + acc$sv_sq[names(sage_values_sq)]

        # Update the count of completed permutations.
        n_completed = n_completed + checkpoint_size

        # Calculate the current average SAGE values and standard errors based on completed permutations.
        current_avg = sage_values / n_completed

        # Sample variance (Bessel-corrected) of the per-permutation marginal
        # contributions, SE = sqrt(Var / n). A single permutation carries no
        # variance information, so the SE is NA rather than a misleading 0.
        if (n_completed > 1L) {
          current_variance = (sage_values_sq - n_completed * current_avg^2) / (n_completed - 1L)
          # Ensure variance is non-negative (numerical precision issues)
          current_variance[current_variance < 0] = 0
          current_se = sqrt(current_variance / n_completed)
        } else {
          current_se = rep(NA_real_, length(current_avg))
          names(current_se) = names(current_avg)
        }

        if (xplain_opt("debug")) {
          cli::cli_alert_info("SAGE values after {.val {n_completed}} permutations")
          cli::cli_ol(c(
            "SAGE values: {.val {round(current_avg, 4)}}",
            "current SE: {.val {round(current_se, 3)}}",
            "Completed: {.val {n_completed}}"
          ))
        }

        # Store the current average SAGE values and standard errors in the convergence history.
        # Used for plotting, early stopping, and uncertainty quantification.
        checkpoint_history = data.table(
          budget = n_completed,
          n_evals = sage_n_evals("permutation", length(self$features), n_completed),
          n_rows = sage_n_rows(
            "permutation",
            length(self$features),
            n_completed,
            nrow(test_dt),
            self$param_set$values$n_samples
          ),
          feature = names(current_avg),
          importance = as.numeric(current_avg),
          se = as.numeric(current_se)
        )
        convergence_history[[length(convergence_history) + 1]] = checkpoint_history

        # Check for convergence if early stopping is enabled and enough permutations have
        # been processed (at least 2, since a single permutation has no SE).
        if (early_stopping && n_completed >= max(min_permutations, 2L)) {
          ratio = sage_convergence_ratio(current_avg, current_se)
          converged = !is.na(ratio) && ratio < se_threshold

          if (xplain_opt("verbose") && converged) {
            cli::cli_inform(c(
              "v" = "SAGE converged after {.val {n_completed}} permutations",
              "i" = "Maximum relative SE: {.val {round(ratio, 4)}} (threshold: {.val {se_threshold}})",
              "i" = "Saved {.val {n_permutations - n_completed}} permutations"
            ))
          }
        }
      }

      # Close the progress bar.
      if (xplain_opt("progress")) {
        cli::cli_progress_done()
      }

      # An exhausted budget under early stopping is a different outcome from a planned
      # run and must not pass silently.
      if (early_stopping && !converged) {
        cli::cli_warn(c(
          "SAGE did not converge within {.val {n_permutations}} permutations.",
          "i" = "Raise {.arg n_permutations} to allow more sampling, or relax {.arg se_threshold}."
        ))
      }

      # Calculate the final average SAGE values based on all completed permutations.
      final_sage_values = sage_values / n_completed

      # Return the computed scores and convergence data.
      list(
        scores = data.table(
          feature = names(final_sage_values),
          importance = as.numeric(final_sage_values)
        ),
        convergence_data = list(
          convergence_history = if (length(convergence_history) > 0) {
            rbindlist(convergence_history)
          } else {
            NULL
          },
          converged = converged,
          budget_used = n_completed
        )
      )
    },

    # Kernel estimator (Covert & Lee 2021, unbiased KernelSHAP, Eq. 9, for the stochastic
    # SAGE game): the exact design matrix A = E[z z^T] is known in closed form, so only
    # b = E[z V(z)] is estimated, from paired coalition draws each evaluated on one
    # test observation, as in the reference `sage.KernelEstimator`.
    .compute_sage_scores_kernel = function(
      learner,
      test_dt,
      n_coalitions,
      batch_size = NULL,
      early_stopping = FALSE,
      se_threshold = 0.025
    ) {
      features = self$features
      m = length(features)
      n_test = nrow(test_dt)
      n_samples = self$param_set$values$n_samples

      # Anchors on the whole test set: V(empty) = 0 and V(full) = total = null - loss(full).
      anchor_losses = private$.evaluate_coalitions_batch(learner, test_dt, list(character(0), features), batch_size)
      null_loss = anchor_losses[1L]
      total = null_loss - anchor_losses[2L]

      history_row = function(n_done, phi, se = NA_real_) {
        data.table(
          budget = n_done,
          n_evals = sage_n_evals("kernel", m, n_done),
          n_rows = sage_n_rows("kernel", m, n_done, n_test, n_samples),
          feature = features,
          importance = as.numeric(phi),
          se = as.numeric(se)
        )
      }

      # With a single feature the Shapley value is the total; the coalition-size
      # support 1..m-1 is empty, so there is nothing to sample.
      if (m == 1L) {
        return(list(
          scores = data.table(feature = features, importance = total),
          convergence_data = list(convergence_history = history_row(0L, total), converged = FALSE, budget_used = 0L)
        ))
      }

      size_probs = sage_kernel_size_probs(m)
      A_inv = solve(sage_kernel_A(m))
      # Covariance propagation from b to phi (their Eq. 13); the constraint term enters
      # with a minus sign, which the reference implementation gets wrong.
      A_inv_1 = as.numeric(A_inv %*% rep(1, m))
      C = A_inv - outer(A_inv_1, A_inv_1) / sum(A_inv_1)

      # Draws per chunk: bounds the rows materialized per prediction batch and sets the
      # granularity of the convergence history and early stopping (the reference
      # implementation's batch size).
      chunk = 512L
      # Running mean and sum of cross-deviations of the per-draw b samples (Welford, merged
      # chunk-wise), from which Cov(b) and hence the SEs follow (their Eqs. 10-12).
      b_mean = numeric(m)
      b_M2 = matrix(0, m, m)
      n_done = 0L
      converged = FALSE
      history = list()
      if (xplain_opt("progress")) {
        cli::cli_progress_bar("Computing SAGE values", total = ceiling(n_coalitions / chunk))
      }
      while (n_done < n_coalitions && !converged) {
        n_chunk = min(chunk, n_coalitions - n_done)
        zs = matrix(0L, nrow = n_chunk, ncol = m)
        for (i in seq_len(n_chunk)) {
          k = sample.int(m - 1L, size = 1L, prob = size_probs)
          zs[i, sample.int(m, size = k)] = 1L # uniform within size
        }
        rows = sample.int(n_test, size = n_chunk, replace = TRUE)

        # Paired sampling (their Section 4.2): a coalition and its complement on the same
        # observation, b = 0.5 (z V(z) + (1 - z) V(1 - z)) per draw.
        V = null_loss - private$.evaluate_pairs_batch(learner, test_dt[c(rows, rows)], rbind(zs, 1L - zs), batch_size)
        # Matrix-times-vector recycles column-wise, i.e. scales row i by V[i].
        b_chunk = 0.5 * (zs * V[seq_len(n_chunk)] + (1L - zs) * V[n_chunk + seq_len(n_chunk)])

        # Chan's parallel merge of the chunk's moments into the running Welford state.
        chunk_mean = colMeans(b_chunk)
        chunk_M2 = crossprod(sweep(b_chunk, 2L, chunk_mean))
        n_new = n_done + n_chunk
        delta = chunk_mean - b_mean
        b_mean = b_mean + delta * n_chunk / n_new
        b_M2 = b_M2 + chunk_M2 + outer(delta, delta) * (n_done * n_chunk / n_new)
        n_done = n_new

        phi = sage_kernel_solve_constrained(A_inv, b_mean, total)
        se = if (n_done > 1L) {
          cov_b = b_M2 / (n_done - 1)
          sqrt(pmax(diag(C %*% cov_b %*% t(C)), 0) / n_done)
        } else {
          rep(NA_real_, m)
        }
        history[[length(history) + 1L]] = history_row(n_done, phi, se)
        if (xplain_opt("progress")) {
          cli::cli_progress_update(inc = 1)
        }

        if (early_stopping) {
          ratio = sage_convergence_ratio(phi, se)
          converged = !is.na(ratio) && ratio < se_threshold
          if (xplain_opt("verbose") && converged) {
            cli::cli_inform(c(
              "v" = "SAGE converged after {.val {n_done}} coalition draws",
              "i" = "Maximum relative SE: {.val {round(ratio, 4)}} (threshold: {.val {se_threshold}})",
              "i" = "Saved {.val {n_coalitions - n_done}} coalition draws"
            ))
          }
        }
      }
      if (xplain_opt("progress")) {
        cli::cli_progress_done()
      }
      if (early_stopping && !converged) {
        cli::cli_warn(c(
          "SAGE did not converge within {.val {n_coalitions}} coalition draws.",
          "i" = "Raise {.arg n_coalitions} to allow more sampling, or relax {.arg se_threshold}."
        ))
      }

      list(
        scores = data.table(feature = features, importance = as.numeric(phi)),
        convergence_data = list(convergence_history = rbindlist(history), converged = converged, budget_used = n_done)
      )
    },

    # Observation-wise value-function samples for the kernel estimator: row i of `zs`
    # (0/1 coalition membership) is evaluated on row i of `rows_dt` only, and its
    # observation-wise loss is returned. Same prediction and aggregation pipeline as
    # `.evaluate_coalitions_batch`, but on (coalition, observation) pairs.
    .evaluate_pairs_batch = function(learner, rows_dt, zs, batch_size = NULL) {
      combined_data = private$.expand_pairs_data(rows_dt, zs)
      predictions = sage_batch_predict(learner, combined_data, self$task, batch_size, self$task$task_type)
      if (anyNA(predictions)) {
        cli::cli_warn("Encountered missing values in model prediction")
      }
      avg_preds = sage_aggregate_predictions(combined_data, predictions, self$task$task_type, self$task$class_names)
      setkey(avg_preds, .coalition_id)

      truth = rows_dt[[self$task$target_names]]
      pred = if (self$task$task_type == "classif") {
        PredictionClassif$new(
          row_ids = seq_len(nrow(zs)),
          truth = truth,
          prob = as.matrix(avg_preds[, .SD, .SDcols = self$task$class_names])
        )
      } else {
        PredictionRegr$new(row_ids = seq_len(nrow(zs)), truth = truth, response = avg_preds$avg_pred)
      }
      losses = pred$obs_loss(measures = self$measure)[[self$measure$id]]
      # Same sign convention as `.evaluate_coalitions_batch`.
      if (isFALSE(self$measure$minimize)) {
        losses = -losses
      }
      losses
    },

    # Abstract: one observation per coalition, see `.evaluate_pairs_batch`.
    .expand_pairs_data = function(rows_dt, zs) {
      cli::cli_abort(c(
        "The kernel estimator is not available for {.cls {class(self)[1]}}.",
        "i" = "Use {.code estimator = \"permutation\"} or {.code estimator = \"exact\"}."
      ))
    },

    # Exact estimator: enumerate all 2^p coalitions and compute the Shapley values in
    # closed form. Reuses the same value function as the permutation estimator, so it is
    # exact with respect to coalition sampling only.
    .compute_sage_scores_exact = function(learner, test_dt, batch_size = NULL) {
      features = self$features
      p = length(features)
      bit = bitwShiftL(1L, 0:(p - 1L)) # per-feature bit masks: 1, 2, 4, ...

      # Enumerate every coalition as a bitmask 0..2^p-1 (bit i set <=> feature i in S),
      # keeping list position k aligned with mask k-1 so losses index by mask.
      n_coal = bitwShiftL(1L, p) # 2^p, kept modest by the max_features guard
      masks = 0:(n_coal - 1L)
      coalitions = lapply(masks, function(mask) features[bitwAnd(mask, bit) > 0L])

      # Evaluate in chunks; input order is preserved, so losses align with masks. The
      # chunk bounds the materialized expansion (each coalition expands to
      # n_test * n_samples rows), while `batch_size` bounds the rows per prediction call.
      chunk = 256L
      losses = numeric(n_coal)
      start = 1L
      while (start <= n_coal) {
        idx = start:min(start + chunk - 1L, n_coal)
        losses[idx] = private$.evaluate_coalitions_batch(learner, test_dt, coalitions[idx], batch_size)
        start = start + chunk
      }

      # v(S) = baseline_loss - loss(S) in importance units; v(empty) = 0 (mask 0).
      v = losses[1L] - losses

      # phi_i = sum_{S not containing i} (v(S+i) - v(S)) / (p * C(p-1, |S|)).
      size = lengths(coalitions)
      weight = 1 / (p * choose(p - 1L, size))
      phi = numeric(p)
      for (i in seq_len(p)) {
        without = which(bitwAnd(masks, bit[i]) == 0L) # positions of masks lacking feature i
        with_i = without + bit[i] # position of the same mask plus feature i
        phi[i] = sum(weight[without] * (v[with_i] - v[without]))
      }
      names(phi) = features

      # No coalition-sampling error: nothing to converge, no history.
      list(
        scores = data.table(feature = features, importance = as.numeric(phi)),
        convergence_data = list(
          convergence_history = NULL,
          converged = NA,
          budget_used = n_coal
        )
      )
    },

    # Template method: Defines the complete prediction and aggregation pipeline
    # Subclasses only need to implement .expand_coalitions_data()
    .evaluate_coalitions_batch = function(learner, test_dt, all_coalitions, batch_size = NULL) {
      n_test = nrow(test_dt)

      if (xplain_opt("debug")) {
        cli::cli_inform("Evaluating {.val {length(all_coalitions)}} coalitions")
      }

      # STEP 1: Subclass-specific data expansion (abstract method)
      # combined data has rows `n_samples * nrow(test_dt) * length(all_coalitions)`
      # Full coalition -> return is just test_dt
      combined_data = private$.expand_coalitions_data(test_dt, all_coalitions)
      # STEPS 2-5: Shared processing pipeline using general utilities
      predictions = sage_batch_predict(
        learner,
        combined_data,
        self$task,
        batch_size,
        self$task$task_type
      )
      if (anyNA(predictions)) {
        cli::cli_warn("Encountered missing values in model prediction")
      }
      avg_preds = sage_aggregate_predictions(
        combined_data,
        predictions,
        self$task$task_type,
        self$task$class_names
      )

      # Private method (needs self$task and self$measure)
      coalition_losses = private$.calculate_coalition_losses(avg_preds, n_test, test_dt)

      # SAGE values are score reductions loss(empty) - loss(S). Negating the scores of a
      # measure that is maximized (e.g. classif.acc) keeps "positive = helps" for all measures.
      if (isFALSE(self$measure$minimize)) {
        coalition_losses = -coalition_losses
      }
      coalition_losses
    },

    # Abstract method - must be implemented by subclasses
    # Returns: data.table with all feature columns plus .coalition_id and .test_instance_id
    .expand_coalitions_data = function(test_dt, all_coalitions) {
      cli::cli_abort(c(
        "Abstract method not implemented",
        "i" = "Subclasses must implement {.fn .expand_coalitions_data}",
        "i" = "This method should return a data.table with:",
        "*" = "All feature columns (with marginalized features replaced/sampled)",
        "*" = "{.field .coalition_id}: integer identifying which coalition",
        "*" = "{.field .test_instance_id}: integer identifying original test instance"
      ))
    },

    # Private method - needs self$task and self$measure
    # Calculates losses from averaged predictions for each coalition.
    # Returns losses ordered by ascending `.coalition_id`, matching the
    # order coalitions were built (the closed-form accumulation indexes
    # this positionally).
    .calculate_coalition_losses = function(avg_preds, n_test, test_dt) {
      .coalition_id = .test_instance_id = NULL # data.table NSE NOTE tax

      coalition_ids = sort(unique(avg_preds$.coalition_id))
      truth = test_dt[[self$task$target_names]]

      # Key once and subset by key (binary search) per coalition,
      # rather than scanning `avg_preds[.coalition_id == i]` in a loop.
      # The key includes .test_instance_id so each block is
      # ordered to align with `truth` (test instance 1..n_test).
      setkey(avg_preds, .coalition_id, .test_instance_id)
      measure = self$measure
      is_classif = self$task$task_type == "classif"
      class_names = self$task$class_names

      # For pointwise (`obs_loss`) measures whose predict_type matches
      # the data we hold (regression response; classification prob), we
      # can call the measure's own mlr3measures function (`$fun`)
      # directly on (truth, response/prob), skipping the per-coalition
      # Prediction object. This reuses the mlr3 measure
      # implementation and does the
      # correct per-measure aggregation. Anything else (classification
      # response measures that need a prob->class step, measures needing
      # task/model context, non-decomposable measures like AUC) takes
      # the canonical Prediction$score() path, so correctness holds for
      # every measure.
      direct = !is.null(measure$fun) &&
        "obs_loss" %in% measure$properties &&
        ((!is_classif && measure$predict_type == "response") ||
          (is_classif && measure$predict_type == "prob"))

      score_block = if (direct && is_classif) {
        function(block) {
          measure$fun(truth = truth, prob = as.matrix(block[, .SD, .SDcols = class_names]))
        }
      } else if (direct) {
        function(block) measure$fun(truth = truth, response = block$avg_pred)
      } else if (is_classif) {
        function(block) {
          PredictionClassif$new(
            row_ids = seq_len(n_test),
            truth = truth,
            prob = as.matrix(block[, .SD, .SDcols = class_names])
          )$score(measure)
        }
      } else {
        function(block) {
          PredictionRegr$new(
            row_ids = seq_len(n_test),
            truth = truth,
            response = block$avg_pred
          )$score(measure)
        }
      }

      coalition_losses = numeric(length(coalition_ids))
      for (k in seq_along(coalition_ids)) {
        coalition_losses[k] = score_block(avg_preds[list(coalition_ids[k])])
      }

      coalition_losses
    }
  )
)

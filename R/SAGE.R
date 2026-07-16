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
#' The permutation estimator follows Covert et al. (2020); the kernel estimator
#' (`estimator = "kernel"`, Kernel SAGE) follows Covert & Lee (2021) and comes in two
#' variants selected by `kernel_variant`: `"original"` (their Eq. 7, the default, recommended
#' in their Section 4.1 for practical use) and `"unbiased"` (their Eq. 9, the variant
#' implemented by the reference Python `sage` package, useful for direct comparisons).
#'
#' **Standard errors**: The SEs reported in `$convergence_history` and used by
#' `$importance(ci_method = "montecarlo")` quantify the Monte Carlo error of the Shapley
#' estimates for a fixed trained model: how much the estimates would still move if more
#' permutations or coalitions were sampled.
#' They are convergence diagnostics, not importance inference (see `$importance()` for how
#' to read them), and do not capture variability across train/test splits or model refits.
#' For [MarginalSAGE] they are additionally conditional on the fixed reference dataset used
#' for marginalization: the variability from having drawn one particular reference subsample
#' (`n_samples` rows) is shared across all coalitions and is not part of the reported SE,
#' mirroring the Python `sage` implementation.
#' When features strongly affect predictions and `n_samples` is small, that unmodeled
#' component can exceed the coalition-sampling error.
#' Increase `n_samples` when the values themselves need to be precise, as the intervals will
#' not reflect this component.
#' For [ConditionalSAGE], by contrast, the sampler redraws at every evaluation, so its noise
#' enters the variation across permutations and is largely reflected in the reported SEs.
#' The exact estimator has no coalition-sampling error and reports no SE.
#'
#' Computationally, the permutation estimator uses the running SE across permutations, while
#' the kernel estimator propagates the coalition-sampling covariance through the constrained
#' least-squares solve (the delta method), which for `kernel_variant = "unbiased"` reduces to
#' the closed-form covariance of Covert & Lee (2021, Eqs. 12-13).
#' For `kernel_variant = "original"` this bookkeeping maintains a covariance matrix over all
#' pairwise feature co-occurrences, whose memory use grows with the fourth power of the number
#' of features (roughly 200 MB at 100 features), so with hundreds of features expect the SE
#' machinery rather than the model evaluations to become the bottleneck.
#'
#' **Relation to the reference implementation (Python `sage`)**:
#' Both implementations follow Covert & Lee (2021), but they estimate the value function in
#' different regimes, and several defaults differ as a consequence.
#'
#' * *Value function estimation*: `sage` estimates the stochastic cooperative game directly,
#'   evaluating each sampled coalition on a single randomly drawn observation, so individual
#'   evaluations are cheap but very noisy.
#'   xplainfi evaluates every coalition's loss on the complete test set, averaging predictions
#'   over `n_samples` reference draws, so a single evaluation is more expensive but nearly
#'   deterministic.
#' * *Default kernel variant*: in the noisy per-observation regime of `sage`, the two variants
#'   converge similarly, and the unbiased variant's closed-form variance powers the package's
#'   convergence detection.
#'   In xplainfi's batch-averaged regime, the original variant is far more sample-efficient at
#'   equal budgets (the coupled sampling errors of its design matrix and right-hand side
#'   largely cancel), matching the paper's own recommendation in Section 4.1.
#' * *Comparing point estimates*: for direct numerical comparisons with `sage`, use
#'   `kernel_variant = "unbiased"`.
#'   The two implementations then compute the same estimator and agree up to Monte Carlo error
#'   (coalition sampling, reference data, and test observations still differ between them).
#' * *Comparing uncertainties*: reported uncertainties are not directly comparable.
#'   The standard deviations in `sage` include the observation-sampling noise described above,
#'   whereas xplainfi conditions on the test set and quantifies coalition-sampling error only
#'   (test-set and model variability are instead addressed by the resampling-based
#'   `ci_method`s).
#'   In addition, `sage`'s uncertainty computation appears to deviate from the paper's Eq. 13
#'   in the sign of the covariance propagation's constraint-adjustment term.
#'   xplainfi implements Eqs. 12-13 as published, which matched the empirical variance in our
#'   simulations.
#' * *Convergence*: `sage` runs until its convergence criterion is met by default, while the
#'   kernel estimator in xplainfi evaluates a fixed budget of `n_coalitions` (early stopping
#'   applies to the permutation estimator only).
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
    convergence_history = NULL,
    #' @field converged (`logical(1)`) Whether convergence was detected (permutation estimator with early stopping).
    #'   The exact estimator sets `TRUE` trivially since it enumerates all coalitions; the kernel estimator never stops early and reports `FALSE`.
    converged = FALSE,
    #' @field n_permutations_used (`integer(1)`) Sampling effort actually spent, in estimator-specific units:
    #'   permutations (permutation estimator), paired coalition draws (kernel; each draw evaluates 2 coalitions),
    #'   or enumerated coalitions (exact).
    n_permutations_used = NULL,

    #' @description
    #' Creates a new instance of the SAGE class.
    #' @param task,learner,measure,resampling,features Passed to FeatureImportanceMethod.
    #' @param estimator (`character(1)`: `"permutation"`) Shapley-value estimator.
    #'   `"permutation"` uses the permutation-sampling estimator (Covert et al., 2020);
    #'   `"kernel"` uses the regression-based kernel estimator (Kernel SAGE, Covert & Lee, 2021);
    #'   `"exact"` enumerates all `2^n_features` coalitions and computes the exact Shapley values.
    #'   The three estimators approximate the same SAGE values but take different budget arguments:
    #'   `"permutation"` uses `n_permutations`, `"kernel"` uses `n_coalitions`, and `"exact"` takes no budget.
    #'   Setting the budget argument of a different estimator is an error.
    #'   The exact estimator has no coalition-sampling error, so it is primarily useful as a ground-truth
    #'   reference for verifying the sampling estimators on small feature sets.
    #'   Note that it is exact only with respect to coalition sampling: for [ConditionalSAGE] the value
    #'   function itself is still a Monte Carlo estimate (the sampler redraws per coalition), and for
    #'   [MarginalSAGE] the value function is defined by the fixed reference subsample, so the
    #'   marginalization error of that draw remains (see `n_samples`).
    #'   The estimators' costs are comparable via their total number of evaluated coalitions:
    #'   `1 + n_permutations * n_features` (permutation), `2 + 2 * n_coalitions` (kernel), and
    #'   `2^n_features` (exact).
    #'   When a sampling budget meets or exceeds the exact estimator's cost, `$compute()` points this out
    #'   in a message (once per session, if `xplain_opt("verbose")`), since exact enumeration then removes
    #'   the coalition-sampling error at no extra cost.
    #'   For [ConditionalSAGE] such oversampling can still be deliberate: repeated coalition evaluations
    #'   average out the conditional sampler's noise, which exact enumeration (one evaluation per
    #'   coalition) does not.
    #' @param n_permutations (`integer(1)`: `NULL`) Number of permutations to sample for SAGE value estimation.
    #'   Only valid for `estimator = "permutation"`.
    #'   Note that each permutation evaluates one coalition per feature, so the total number of evaluated
    #'   coalitions (the actual computational cost) is `1 (empty) + n_permutations * n_features`,
    #'   not `n_permutations`.
    #'   If unset, defaults to `10L`, reduced on small feature sets so the default budget stays at or below
    #'   half the cost of enumerating all `2^n_features` coalitions, where `estimator = "exact"` becomes
    #'   the better tool (see `estimator`).
    #'   To check whether a budget suffices, use `$importance(ci_method = "montecarlo")` or
    #'   `$plot_convergence()` after computation and increase the budget until the intervals are narrow
    #'   relative to the spread of the importance values.
    #' @param n_coalitions (`integer(1)`: `NULL`) Number of paired coalition draws for the kernel estimator.
    #'   Only valid for `estimator = "kernel"`.
    #'   Note that with paired sampling each draw evaluates a coalition and its complement, so the total
    #'   number of evaluated coalitions (the actual computational cost) is `2 (anchors) + 2 * n_coalitions`,
    #'   not `n_coalitions`.
    #'   If unset, defaults to `5 * n_features` draws, matching the evaluation budget of the permutation
    #'   estimator's default (`2 + 10 * n_features` vs `1 + 10 * n_features` evaluated coalitions); since
    #'   the kernel estimator is typically more sample-efficient, the default usually yields more accurate
    #'   values than the permutation default at the same cost.
    #'   The default is capped at `512L` and, on small feature sets, at `2^(n_features - 2)` so it stays
    #'   at or below half the cost of enumerating all `2^n_features` coalitions, where
    #'   `estimator = "exact"` becomes the better tool (see `estimator`).
    #'   To check whether a budget suffices, use `$importance(ci_method = "montecarlo")` or
    #'   `$plot_convergence()` after computation and increase the budget until the intervals are narrow
    #'   relative to the spread of the importance values.
    #' @param kernel_variant (`character(1)`: `"original"`) Variant of the kernel estimator.
    #'   Only valid for `estimator = "kernel"`.
    #'   `"original"` (default) estimates both the design matrix and its right-hand side from the same
    #'   sampled coalitions (original KernelSHAP, Covert & Lee 2021, Eq. 7).
    #'   Sharing the samples couples their errors, which largely cancel, so this variant converges much
    #'   faster on typical (near-additive) problems and is the paper's practical recommendation (their Section 4.1).
    #'   `"unbiased"` uses the exact closed-form design matrix and estimates only the right-hand side
    #'   (unbiased KernelSHAP, Eq. 9).
    #'   This is the estimator implemented in the reference Python `sage` package, so use it for direct
    #'   comparisons with `sage`.
    #'   At equal budgets its point estimates are substantially noisier, which its wider confidence
    #'   intervals reflect, so set `n_coalitions` well above the default (which is tuned for
    #'   `"original"`) when using it.
    #' @param max_features (`integer(1)`: `12L`) Feature-count cap for `estimator = "exact"`.
    #'   The exact estimator evaluates `2^n_features` coalitions, so it aborts when the number of
    #'   features exceeds this cap.
    #'   Increase it to override, keeping the combinatorial cost in mind.
    #'   Setting it for the other estimators is a warning, as they ignore it.
    #' @param batch_size (`integer(1)`: `5000L`) Maximum number of observations to process in a single prediction call.
    #' @param n_samples (`integer(1)`: `100L`) Number of samples to use for marginalizing out-of-coalition features.
    #'   For [MarginalSAGE], this is the number of marginal data samples ("background data" in other implementations).
    #'   Note that [MarginalSAGE] draws its reference subsample once at construction,
    #'   so changing `$param_set$values$n_samples` afterwards does not redraw it.
    #'   For [ConditionalSAGE], this is the number of conditional samples per test instance retrieved from `sampler`.
    #'   This is a second sampling budget on top of the coalition budget, and it affects *all* estimators:
    #'   even `estimator = "exact"` is exact only with respect to coalition sampling, while the
    #'   marginalization error from `n_samples` remains and shrinks roughly with `1 / sqrt(n_samples)`.
    #'   For [MarginalSAGE] this error stems from the single reference draw, is shared across all
    #'   coalitions, and is invisible to the Monte Carlo standard errors; in internal experiments its
    #'   magnitude at the default `n_samples` was comparable to or larger than the coalition-sampling
    #'   error at default budgets, so increase `n_samples` first when the values themselves need to be
    #'   precise, and gauge this component by recomputing with a different seed before construction.
    #'   For [ConditionalSAGE] the sampler redraws at every evaluation, so this noise is largely
    #'   included in the reported standard errors and is additionally averaged down by larger
    #'   coalition budgets.
    #' @param early_stopping (`logical(1)`: `FALSE`) Whether to enable early stopping based on convergence detection.
    #'   Only used by the permutation estimator.
    #' @param se_threshold (`numeric(1)`: `0.01`) Convergence threshold for relative standard error.
    #'   Convergence is detected when the maximum relative SE across all features falls below this threshold.
    #'   Relative SE is calculated as SE divided by the range of importance values (max - min),
    #'   making it scale-invariant across different loss metrics.
    #'   Default of `0.01` means convergence when relative SE is below 1% of the importance range.
    #'   Only used by the permutation estimator; setting a non-default value for another estimator is a warning.
    #' @param min_permutations (`integer(1)`: `10L`) Minimum permutations before checking for convergence.
    #'   Convergence is judged based on the standard errors of the estimated SAGE values,
    #'   which requires a sufficiently large number of samples (i.e., evaluated coalitions).
    #'   Only used by the permutation estimator; setting a non-default value for another estimator is a warning.
    #' @param check_interval (`integer(1)`: `1L`) Check convergence every N permutations.
    #'   Only used by the permutation estimator; setting a non-default value for another estimator is a warning.
    #'   The kernel estimator manages its own checkpointing for prediction batching and convergence history.
    initialize = function(
      task,
      learner,
      measure = NULL,
      resampling = NULL,
      features = NULL,
      estimator = c("permutation", "kernel", "exact"),
      n_permutations = NULL,
      n_coalitions = NULL,
      kernel_variant = NULL,
      max_features = 12L,
      batch_size = 5000L,
      n_samples = 100L,
      early_stopping = FALSE,
      se_threshold = 0.01,
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

      # Each estimator takes a different budget argument; setting another
      # estimator's budget is a hard error rather than a silently ignored value.
      # Budget defaults are NULL so we can distinguish "user set it" from "unset".
      if (estimator != "kernel" && !is.null(kernel_variant)) {
        cli::cli_abort(c(
          "{.arg kernel_variant} is only valid for {.code estimator = \"kernel\"}.",
          "i" = "The {.val {estimator}} estimator has no design-matrix variant."
        ))
      }
      if (estimator == "permutation") {
        if (!is.null(n_coalitions)) {
          cli::cli_abort(c(
            "{.arg n_coalitions} is only valid for {.code estimator = \"kernel\"}.",
            "i" = "The permutation estimator is controlled by {.arg n_permutations}."
          ))
        }
        n_permutations = checkmate::assert_int(
          n_permutations %||% sage_default_n_permutations(length(self$features)),
          lower = 1L
        )
      } else if (estimator == "kernel") {
        if (!is.null(n_permutations)) {
          cli::cli_abort(c(
            "{.arg n_permutations} is only valid for {.code estimator = \"permutation\"}.",
            "i" = "The kernel estimator is controlled by {.arg n_coalitions}."
          ))
        }
        n_coalitions = checkmate::assert_int(
          n_coalitions %||% sage_default_n_coalitions(length(self$features)),
          lower = 1L
        )
        kernel_variant = checkmate::assert_choice(
          kernel_variant %||% "original",
          choices = c("original", "unbiased")
        )
      } else {
        # exact: enumerates all coalitions, so no sampling budget applies.
        if (!is.null(n_permutations) || !is.null(n_coalitions)) {
          cli::cli_abort(c(
            "The exact estimator enumerates all coalitions and takes no sampling budget.",
            "i" = "Do not set {.arg n_permutations} or {.arg n_coalitions} with {.code estimator = \"exact\"}."
          ))
        }
      }

      # Estimator-specific tuning arguments are always validated; unlike the budgets
      # above, a value set for an estimator that ignores it is a warning, not an
      # error, since it does not change what is estimated.
      checkmate::assert_int(max_features, lower = 1L)
      checkmate::assert_flag(early_stopping)
      checkmate::assert_number(se_threshold, lower = 0, upper = 1)
      checkmate::assert_int(min_permutations, lower = 1L)
      checkmate::assert_int(check_interval, lower = 1L)
      if (estimator != "exact" && !identical(as.integer(max_features), 12L)) {
        cli::cli_warn(c(
          "{.arg max_features} only applies to {.code estimator = \"exact\"}.",
          "i" = "Ignored for the {.val {estimator}} estimator."
        ))
      }
      if (estimator != "permutation") {
        non_default = c(
          se_threshold = !identical(se_threshold, 0.01),
          min_permutations = !identical(as.integer(min_permutations), 10L),
          check_interval = !identical(as.integer(check_interval), 1L)
        )
        if (any(non_default)) {
          cli::cli_warn(c(
            "{.arg {names(non_default)[non_default]}} {?is/are} only used by {.code estimator = \"permutation\"}.",
            "i" = "Ignored for the {.val {estimator}} estimator."
          ))
        }
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

      # Set parameters. The param_set is the single source of truth for the
      # estimator configuration; read and edit it via $param_set$values.
      ps = ps(
        estimator = paradox::p_fct(c("permutation", "kernel", "exact"), default = "permutation"),
        n_permutations = paradox::p_int(lower = 1L, default = 10L),
        n_coalitions = paradox::p_int(lower = 1L, default = 512L),
        kernel_variant = paradox::p_fct(c("original", "unbiased"), default = "original"),
        max_features = paradox::p_int(lower = 1L, default = 12L),
        batch_size = paradox::p_int(lower = 1L, default = 5000L),
        n_samples = paradox::p_int(lower = 1L, default = 100L),
        early_stopping = paradox::p_lgl(default = FALSE),
        se_threshold = paradox::p_dbl(lower = 0, upper = 1, default = 0.01),
        min_permutations = paradox::p_int(lower = 1L, default = 10L),
        check_interval = paradox::p_int(lower = 1L, default = 1L)
      )
      ps$values$estimator = estimator
      # Only the active estimator's arguments are stored; the others stay unset
      # (NULL) so the param_set reflects what actually drives the computation.
      if (estimator == "permutation") {
        ps$values$n_permutations = n_permutations
        ps$values$early_stopping = early_stopping
        ps$values$se_threshold = se_threshold
        ps$values$min_permutations = min_permutations
        ps$values$check_interval = check_interval
      } else if (estimator == "kernel") {
        ps$values$n_coalitions = n_coalitions
        ps$values$kernel_variant = kernel_variant
      } else {
        ps$values$max_features = max_features
      }
      ps$values$batch_size = batch_size
      ps$values$n_samples = n_samples
      self$param_set = ps

      if (estimator == "exact") {
        sage_assert_exact_budget(length(self$features), max_features)
      }

      # SAGE reports Monte Carlo (coalition-sampling) standard errors, so it offers an
      # additional CI method on top of the resampling-based ones from the base class.
      private$.ci_methods = c(private$.ci_methods, "montecarlo")
    },

    #' @description
    #' Get aggregated importance scores with optional confidence intervals.
    #'
    #' Extends the base [FeatureImportanceMethod] method with `ci_method = "montecarlo"`, which
    #' builds Wald intervals from the Monte Carlo standard errors of the SAGE estimator
    #' (the uncertainty of the Shapley estimate for a fixed trained model due to finite coalition
    #' or permutation sampling).
    #' This differs from the resampling-based methods (`"raw"`, `"nadeau_bengio"`, `"quantile"`),
    #' which quantify variability across train/test splits.
    #' Monte Carlo standard errors are available for `estimator = "kernel"` and
    #' `estimator = "permutation"`, but not `"exact"` (which has no coalition-sampling error).
    #'
    #' **Interpreting `"montecarlo"` intervals**: they are convergence diagnostics, not importance
    #' inference.
    #' An importance of 1.5 with interval `[1.2, 1.8]` means: had all coalitions been enumerated
    #' for this fitted model on this test data (and, for [MarginalSAGE], this reference data),
    #' the result would lie in `[1.2, 1.8]`.
    #' The sampling budget has pinned the computation down to about `+/- 0.3`.
    #' It does *not* mean the feature's importance is nonzero in any generalizable sense: the
    #' interval excludes test-set, reference-data, and model-refit variability entirely.
    #' Because the estimation target is a fixed, generally nonzero number, a test against zero
    #' would reject for every feature given enough coalitions, so no `statistic` or `p.value` is
    #' reported and `p_adjust` is not accepted.
    #' Legitimate uses are checking whether the sampling budget suffices (small `se` relative to
    #' the importance range) and whether two features' estimates are distinguishable at the
    #' current budget (non-overlapping intervals).
    #' For inference about feature importance, use the resampling-based methods.
    #'
    #' @param relation (`character(1)` | `NULL`: `NULL`) Ignored for SAGE (importance is not a baseline/post relation).
    #' @param standardize (`logical(1)`: `FALSE`) If `TRUE`, importances (and their standard errors)
    #'   are standardized by the largest absolute importance.
    #' @param ci_method (`character(1)`: `"none"`) Variance estimation method.
    #'   In addition to the base methods (`"none"`, `"raw"`, `"nadeau_bengio"`, `"quantile"`),
    #'   SAGE supports `"montecarlo"` for coalition-sampling convergence intervals (see above).
    #' @param conf_level (`numeric(1)`: `0.95`) Confidence level for confidence intervals when `ci_method != "none"`.
    #' @param alternative (`character(1)`: `"two.sided"`) Type of alternative hypothesis.
    #'   For `"montecarlo"` this only selects between two-sided and lower-bounded intervals.
    #' @param p_adjust (`character(1)`: `"none"`) Method for p-value adjustment for multiple comparisons.
    #'   Not accepted for `ci_method = "montecarlo"`, which performs no hypothesis test.
    #' @param ... Passed to the base method for other CI methods.
    #' @return ([data.table][data.table::data.table]) Aggregated importance scores, with CI columns when requested.
    importance = function(
      relation = NULL,
      standardize = FALSE,
      ci_method = c("none", "raw", "nadeau_bengio", "quantile", "montecarlo"),
      conf_level = 0.95,
      alternative = c("two.sided", "greater"),
      p_adjust = "none",
      ...
    ) {
      if (length(ci_method) > 1) {
        ci_method = ci_method[1]
      }

      if (identical(ci_method, "montecarlo")) {
        # Mirror the base method's unknown-argument guard, which this early return bypasses:
        # a typo'd argument must error, not silently produce default intervals.
        if (...length() > 0) {
          dots = list(...)
          cli::cli_abort(c(
            "Unknown argument{?s}: {.arg {names(dots)}}.",
            i = "These arguments are not used by {.fun $importance} with {.code ci_method = \"montecarlo\"}."
          ))
        }
        if (is.null(private$.scores)) {
          cli::cli_inform(c(x = "No importances computed yet!"))
          return(invisible(NULL))
        }
        checkmate::assert_number(conf_level, lower = 0, upper = 1)
        # No hypothesis test is performed (see docs), so there are no p-values to adjust.
        if (!identical(p_adjust, "none")) {
          cli::cli_abort(c(
            "{.arg p_adjust} does not apply to {.code ci_method = \"montecarlo\"}.",
            "i" = "This method reports convergence intervals without test statistics or p-values.",
            "i" = "For importance inference with multiplicity adjustment, use a resampling-based method
                   such as {.code ci_method = \"nadeau_bengio\"}."
          ))
        }
        alternative = match.arg(alternative)

        scores = self$scores()
        if (!("se" %in% names(scores)) || all(is.na(scores$se))) {
          cli::cli_abort(c(
            "{.code ci_method = \"montecarlo\"} requires Monte Carlo standard errors, but none are available.",
            "i" = "The exact estimator has no coalition-sampling error, so it reports no SEs.",
            "i" = "SEs are also unavailable with a single feature of interest, with {.code n_permutations = 1},
                   or when the kernel sampling budget is too small to identify all features."
          ))
        }
        if (anyNA(scores$se)) {
          n_na = length(unique(scores$iter_rsmp[is.na(scores$se)]))
          cli::cli_warn(c(
            "Monte Carlo standard errors are missing for {n_na} resampling iteration{?s}.",
            "i" = "Affected features aggregate to {.val NA} intervals."
          ))
        }
        # The intervals quantify each iteration's coalition-sampling error only; pooling
        # across refitted models must not be mistaken for between-model uncertainty.
        if (self$resampling$iters > 1L) {
          cli::cli_warn(c(
            "Monte Carlo intervals are pooled across {self$resampling$iters} resampling iterations.",
            "i" = "They reflect only the coalition-sampling error of each iteration's computation,
                   not the variability between the refitted models.",
            "i" = "A single holdout split is the intended use for this {.arg ci_method}."
          ))
        }
        aggregator = self$measure$aggregator
        if (!is.null(aggregator) && !identical(aggregator, mean)) {
          cli::cli_warn(c(
            "{.code ci_method = \"montecarlo\"} always uses the mean across iterations as point estimate.",
            "i" = "The aggregator of measure {.val {self$measure$id}} is ignored here,
                   so point estimates can differ from other {.arg ci_method}s."
          ))
        }

        if (standardize) {
          importance = se = NULL # data.table NSE tax
          scale_factor = max(abs(scores$importance), na.rm = TRUE)
          scores[, c("importance", "se") := list(importance / scale_factor, se / scale_factor)]
        }

        agg = importance_sage_montecarlo(scores, conf_level, alternative)
        setkeyv(agg, "feature")
        return(agg[])
      }

      super$importance(
        relation = relation,
        standardize = standardize,
        ci_method = ci_method,
        conf_level = conf_level,
        alternative = alternative,
        p_adjust = p_adjust,
        ...
      )
    },

    #' @description
    #' Compute SAGE values.
    #' @param store_backends (`logical(1)`) Whether to store data backends.
    #' @param batch_size (`integer(1)`: `5000L`) Maximum number of observations to process in a single prediction call.
    #' @param early_stopping (`logical(1)`: `FALSE`) Whether to check for convergence and stop early.
    #'   Only used by the permutation estimator; passing it for another estimator is a warning.
    #' @param se_threshold (`numeric(1)`: `0.01`) Convergence threshold for relative standard error.
    #'   SE is normalized by the range of importance values (max - min) to make convergence
    #'   detection scale-invariant. Default `0.01` means convergence when relative SE < 1%.
    #'   Only used by the permutation estimator; passing it for another estimator is a warning.
    #' @param min_permutations (`integer(1)`: `10L`) Minimum permutations before checking convergence.
    #'   Only used by the permutation estimator; passing it for another estimator is a warning.
    #' @param check_interval (`integer(1)`: `1L`) Check convergence every N permutations.
    #'   Only used by the permutation estimator; passing it for another estimator is a warning.
    compute = function(
      store_backends = TRUE,
      batch_size = NULL,
      early_stopping = NULL,
      se_threshold = NULL,
      min_permutations = NULL,
      check_interval = NULL
    ) {
      # Reset convergence tracking
      self$convergence_history = NULL
      self$converged = FALSE
      self$n_permutations_used = NULL

      # The estimator configuration is read from the param_set (the public fields are
      # views of it), so post-construction edits via $param_set$values take effect here.
      estimator = self$param_set$values$estimator %||% "permutation"
      m = length(self$features)
      if (estimator == "exact") {
        sage_assert_exact_budget(m, self$param_set$values$max_features %||% 12L)
      } else {
        budget = if (estimator == "permutation") {
          self$param_set$values$n_permutations %||% sage_default_n_permutations(m)
        } else {
          self$param_set$values$n_coalitions %||% sage_default_n_coalitions(m)
        }
        sage_inform_budget_vs_exact(estimator, m, budget)
      }

      # Permutation-only convergence controls passed explicitly to $compute() are
      # ignored by the other estimators; warn instead of silently dropping them.
      if (estimator != "permutation") {
        passed = c(
          early_stopping = !is.null(early_stopping),
          se_threshold = !is.null(se_threshold),
          min_permutations = !is.null(min_permutations),
          check_interval = !is.null(check_interval)
        )
        if (any(passed)) {
          cli::cli_warn(c(
            "{.arg {names(passed)[passed]}} {?is/are} only used by {.code estimator = \"permutation\"}.",
            "i" = "Ignored for the {.val {estimator}} estimator."
          ))
        }
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
        0.01
      )
      min_permutations = resolve_param(
        min_permutations,
        self$param_set$values$min_permutations,
        10L
      )
      check_interval = resolve_param(check_interval, self$param_set$values$check_interval, 1L)

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

      # Estimator dispatch. All estimators return the same
      # list(scores, convergence_data) shape, so the resampling-aggregation
      # below is estimator-agnostic. The kernel and exact estimators ignore the
      # permutation-only convergence controls (early stopping etc.).
      score_iter = function(learner, test_dt, track_convergence) {
        if (estimator == "exact") {
          private$.compute_sage_scores_exact(
            learner = learner,
            test_dt = test_dt,
            batch_size = batch_size
          )
        } else if (estimator == "kernel") {
          # `check_interval` is a permutation-convergence knob (default 1); using it
          # here would batch coalition evaluations one draw at a time. The kernel path
          # only uses its chunk size for prediction batching and history granularity,
          # so it keeps its own larger default.
          private$.compute_sage_scores_kernel(
            learner = learner,
            test_dt = test_dt,
            n_coalitions = self$param_set$values$n_coalitions %||%
              sage_default_n_coalitions(length(self$features)),
            batch_size = batch_size,
            track_convergence = track_convergence
          )
        } else {
          private$.compute_sage_scores(
            learner = learner,
            test_dt = test_dt,
            # n_permutations_used is either same as n_permutations (if early_stopping = FALSE)
            # or a smaller value if early_stopping = TRUE and it stopped early.
            # Remaining iterations reuse the first iteration's stopped count.
            n_permutations = if (track_convergence) {
              self$param_set$values$n_permutations %||%
                sage_default_n_permutations(length(self$features))
            } else {
              self$n_permutations_used %||%
                self$param_set$values$n_permutations %||%
                sage_default_n_permutations(length(self$features))
            },
            batch_size = batch_size,
            # Only track convergence etc. for the first iteration
            early_stopping = if (track_convergence) early_stopping else FALSE,
            se_threshold = se_threshold,
            min_permutations = min_permutations,
            check_interval = check_interval
          )
        }
      }

      # Compute SAGE values for convergence tracking (first iteration)
      first_result = score_iter(
        learner = rr$learners[[iter_for_convergence]],
        test_dt = self$task$data(rows = rr$resampling$test_set(iter_for_convergence)),
        track_convergence = TRUE
      )

      # Extract convergence data from first iteration
      # `convergence_data` exists even if early_stopping = FALSE
      self$convergence_history = first_result$convergence_data$convergence_history
      self$converged = first_result$convergence_data$converged
      self$n_permutations_used = first_result$convergence_data$n_permutations_used

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
    #' Plot convergence history of SAGE values.
    #' @param features (`character` | `NULL`) Features to plot. If NULL, plots all features.
    #' @return A [ggplot2][ggplot2::ggplot] object
    plot_convergence = function(features = NULL) {
      require_package("ggplot2")

      estimator = self$param_set$values$estimator %||% "permutation"
      if (identical(estimator, "exact")) {
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

      # The kernel estimator tracks coalition draws rather than permutations; adapt
      # the axis labels accordingly. Both estimators populate SEs, so the ribbon is
      # drawn whenever they are available (skipped only if entirely missing).
      is_kernel = identical(estimator, "kernel")
      unit = if (is_kernel) "coalition draws" else "permutations"
      budget = if (is_kernel) self$param_set$values$n_coalitions else self$param_set$values$n_permutations
      x_label = if (is_kernel) "Number of Coalition Draws (2 evaluations each)" else "Number of Permutations"
      has_se = "se" %in% names(plot_data) && !all(is.na(plot_data$se))

      p = ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = n_permutations, y = importance, fill = feature, color = feature)
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
          subtitle = if (self$converged) {
            sprintf(
              "Converged after %d %s (saved %d)",
              self$n_permutations_used,
              unit,
              budget - self$n_permutations_used
            )
          } else {
            sprintf("Completed all %d %s", budget, unit)
          },
          x = x_label,
          y = "SAGE Value",
          color = "Feature",
          fill = "Feature"
        ) +
        ggplot2::theme_minimal(base_size = 14)

      if (self$converged) {
        p = p +
          ggplot2::geom_vline(
            xintercept = self$n_permutations_used,
            linetype = "dashed",
            color = "red",
            alpha = 0.5
          )
      }

      p
    }
  ),

  active = list(
    #' @field n_permutations (`integer(1)`) Deprecated.
    #'   The permutation budget lives in the param_set; use `$param_set$values$n_permutations` instead.
    #'   This alias is kept for backward compatibility with the field of the same name in
    #'   earlier releases and warns on access.
    n_permutations = function(rhs) {
      if (missing(rhs)) {
        cli::cli_warn(
          c(
            "The {.field n_permutations} field is deprecated.",
            "i" = "Read it via {.code $param_set$values$n_permutations} instead."
          ),
          .frequency = "once",
          .frequency_id = "xplainfi_sage_n_permutations_get"
        )
        return(self$param_set$values$n_permutations)
      }
      cli::cli_warn(
        c(
          "The {.field n_permutations} field is deprecated.",
          "i" = "Set it via {.code $param_set$values$n_permutations} instead."
        ),
        .frequency = "once",
        .frequency_id = "xplainfi_sage_n_permutations_set"
      )
      self$param_set$values$n_permutations = checkmate::assert_int(rhs, lower = 1L)
    }
  ),

  private = list(
    # This function computes the SAGE values for a single resampling iteration.
    # It iterates through permutations of features, evaluates coalitions, and calculates marginal contributions.
    .compute_sage_scores = function(
      learner,
      test_dt,
      n_permutations,
      batch_size = NULL,
      early_stopping = FALSE,
      se_threshold = 0.01,
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
          n_permutations = n_completed,
          feature = names(current_avg),
          importance = as.numeric(current_avg),
          se = as.numeric(current_se)
        )
        convergence_history[[length(convergence_history) + 1]] = checkpoint_history

        # Check for convergence if early stopping is enabled and enough permutations have
        # been processed (at least 2, since a single permutation has no SE).
        if (early_stopping && n_completed >= max(min_permutations, 2L) && length(convergence_history) > 1) {
          # Get SAGE values from the current checkpoint.
          curr_checkpoint = convergence_history[[length(convergence_history)]]

          # Ensure features are in the same order for comparison.
          curr_checkpoint_ordered = copy(curr_checkpoint)[order(feature)]
          curr_importance_values = curr_checkpoint_ordered$importance
          curr_se_values = curr_checkpoint_ordered$se

          # Calculate range of importance values across all features (matching fippy)
          importance_range = max(curr_importance_values, na.rm = TRUE) -
            min(curr_importance_values, na.rm = TRUE)

          # Normalize SE by range to get relative SE (matching fippy's formula)
          # fippy: ratio = SE / range, convergence if max(ratio) < threshold
          # https://github.com/gcskoenig/fippy/blob/a7a37aa5511f7074ead3289c89b1ae80036982cb/src/fippy/explainers/utils.py#L40-L42
          if (importance_range > 0 && is.finite(importance_range)) {
            relative_se_values = curr_se_values / importance_range
            max_relative_se = max(relative_se_values, na.rm = TRUE)
          } else {
            # If range is 0 or invalid, use absolute SE (fallback)
            max_relative_se = max(curr_se_values, na.rm = TRUE)
          }

          # Check convergence: max relative SE below threshold
          converged = is.finite(max_relative_se) && max_relative_se < se_threshold
          convergence_msg = c(
            "v" = "SAGE converged after {.val {n_completed}} permutations",
            "i" = "Maximum relative SE: {.val {round(max_relative_se, 4)}} (threshold: {.val {se_threshold}})",
            "i" = "Saved {.val {n_permutations - n_completed}} permutations"
          )

          if (xplain_opt("verbose") && converged) {
            cli::cli_inform(convergence_msg)
          }
        }
      }

      # Close the progress bar.
      if (xplain_opt("progress")) {
        cli::cli_progress_done()
      }

      # Calculate the final average SAGE values based on all completed permutations.
      final_sage_values = sage_values / n_completed

      # Final Monte Carlo SE per feature (from the last checkpoint's running variance),
      # surfaced so `$importance(ci_method = "montecarlo")` can build Wald CIs.
      final_se = current_se[names(final_sage_values)]

      # Return the computed scores and convergence data.
      list(
        scores = data.table(
          feature = names(final_sage_values),
          importance = as.numeric(final_sage_values),
          se = as.numeric(final_se)
        ),
        convergence_data = list(
          convergence_history = if (length(convergence_history) > 0) {
            rbindlist(convergence_history)
          } else {
            NULL
          },
          converged = converged,
          n_permutations_used = n_completed
        )
      )
    },

    # Kernel SAGE estimator (Covert & Lee 2021). Estimates Shapley values by
    # fitting an additive model to the value function v(S) = baseline_loss - loss(S)
    # via weighted least squares with the Shapley kernel. Reuses the exact same
    # value function (`.evaluate_coalitions_batch`) as the permutation estimator,
    # so both MarginalSAGE and ConditionalSAGE inherit it unchanged.
    #
    # Two variants (see `kernel_variant`), sharing the coalition sampling loop:
    # "original" (default, their Eq. 7) estimates BOTH the design matrix A = E[z z^T]
    # and b = E[z v(z)] from the same sampled coalitions. Sharing the samples couples
    # the errors in A and b, so the ratio A^{-1} b is far lower variance than plugging
    # in the exact A; Covert & Lee recommend it in practice for exactly this reason
    # (their Section 4.1). "unbiased" (their Eq. 9, the variant the Python `sage`
    # package implements) uses the exact closed-form A and estimates only b.
    # Coalitions are drawn with paired sampling (each draw plus its complement) for
    # variance reduction.
    #
    # Standard errors: the estimate is a smooth function of sample-mean moments, so
    # its covariance follows from the multivariate delta method. Per-pair moments are
    # accumulated with Welford's online algorithm (numerically stable running mean +
    # covariance). For "unbiased" the moment is just b and the delta method reduces to
    # the paper's closed form Cov(phi) = C Cov(b_mean) C^T (Eqs. 12-13); for
    # "original" the moment is w = (offdiag(A), b) and Cov(w_mean) is propagated
    # through the numerical Jacobian of the constrained solve, capturing the sampling
    # variance of both A and b (see sage_kernel_estimate_original).
    .compute_sage_scores_kernel = function(
      learner,
      test_dt,
      n_coalitions,
      batch_size = NULL,
      track_convergence = TRUE
    ) {
      features = self$features
      m = length(features)
      # Draws per checkpoint: bounds the rows materialized per prediction batch and
      # sets the granularity of the convergence history. Not user-facing (the
      # permutation estimator's check_interval knob deliberately does not map here).
      check_interval = 64L

      # Value-function anchors. V(S) = baseline_loss - loss(S), evaluated with the
      # same machinery the permutation estimator uses, so V(empty) = 0 and
      # V(full) = total = baseline_loss - loss(full).
      anchor_losses = private$.evaluate_coalitions_batch(
        learner,
        test_dt,
        list(character(0), features),
        batch_size
      )
      baseline_loss = anchor_losses[1L]
      total = baseline_loss - anchor_losses[2L]

      # With a single feature the Shapley value is exactly the total; the
      # coalition-size support (1..m-1) is empty, so there is nothing to sample.
      if (m == 1L) {
        phi = total
        names(phi) = features
        return(list(
          scores = data.table(feature = features, importance = as.numeric(phi), se = NA_real_),
          convergence_data = list(
            convergence_history = data.table(
              n_permutations = 0L,
              feature = features,
              importance = as.numeric(phi),
              se = NA_real_
            ),
            converged = FALSE,
            n_permutations_used = 0L
          )
        ))
      }

      size_probs = sage_kernel_size_probs(m)
      unbiased = identical(self$param_set$values$kernel_variant %||% "original", "unbiased")
      # The exact design matrix depends only on m; invert it once per iteration
      # rather than at every checkpoint estimate.
      A_inv_exact = if (unbiased) solve(sage_kernel_A(m)) else NULL

      # Moment vector layout. For "unbiased" only b is estimated (A is exact), so the
      # per-pair moment is just b. For "original" the sampled A has an exactly
      # constant diagonal (0.5), so only its lower-triangular off-diagonals are
      # random and enter w alongside b.
      od = which(lower.tri(matrix(0, m, m)), arr.ind = TRUE)
      noff = nrow(od)
      len_w = if (unbiased) m else noff + m

      # Welford running mean (w_mean) and sum of cross-deviations (w_M2) of the
      # per-pair moment vector, plus the pair count.
      w_mean = numeric(len_w)
      w_M2 = matrix(0, len_w, len_w)
      n_pairs = 0L

      # Point estimate + SE from the running moments, dispatched by variant.
      estimate = function(require_result = FALSE) {
        cov_mean = if (n_pairs > 1L) w_M2 / (n_pairs * (n_pairs - 1L)) else NULL
        if (unbiased) {
          sage_kernel_estimate_unbiased(w_mean, cov_mean, m, total, A_inv = A_inv_exact)
        } else {
          sage_kernel_estimate_original(w_mean, cov_mean, m, total, od, noff, require_result)
        }
      }

      convergence_history = list()
      n_completed = 0L

      if (xplain_opt("progress")) {
        cli::cli_progress_bar(
          "Computing kernel SAGE values",
          total = ceiling(n_coalitions / check_interval)
        )
      }

      # Draw coalitions in checkpoints so each checkpoint's losses are evaluated in
      # a single batched prediction call and the running estimate can be tracked.
      while (n_completed < n_coalitions) {
        this_chunk = min(check_interval, n_coalitions - n_completed)

        # Paired sampling: each draw contributes a coalition and its complement,
        # hence 2 rows per draw.
        zs = matrix(0L, nrow = this_chunk * 2L, ncol = m)
        coalitions = vector("list", this_chunk * 2L)
        for (i in seq_len(this_chunk)) {
          k = sample.int(m - 1L, size = 1L, prob = size_probs)
          sel = sample.int(m, size = k) # uniform within size
          comp = setdiff(seq_len(m), sel)
          row_a = 2L * i - 1L
          row_b = 2L * i
          zs[row_a, sel] = 1L
          coalitions[[row_a]] = features[sel]
          zs[row_b, comp] = 1L
          coalitions[[row_b]] = features[comp]
        }

        losses = private$.evaluate_coalitions_batch(learner, test_dt, coalitions, batch_size)
        V = baseline_loss - losses # value function in importance units

        # Chunk moment matrix W, one row per (coalition, complement) pair:
        # b_i = 0.5 (z V(z) + comp V(comp)) for both variants, prefixed by the
        # sampled-A off-diagonals 0.5 (z_k z_l + comp_k comp_l) for "original".
        # Matrix-times-vector recycles column-wise, i.e. scales row i by V[i].
        odd = seq(1L, 2L * this_chunk, by = 2L)
        za = zs[odd, , drop = FALSE]
        zb = zs[odd + 1L, , drop = FALSE]
        b_block = 0.5 * (za * V[odd] + zb * V[odd + 1L])
        W = if (unbiased) {
          b_block
        } else {
          a_block = 0.5 *
            (za[, od[, 1L], drop = FALSE] *
              za[, od[, 2L], drop = FALSE] +
              zb[, od[, 1L], drop = FALSE] * zb[, od[, 2L], drop = FALSE])
          cbind(a_block, b_block)
        }

        # Merge the chunk into the running moments with Chan's parallel Welford
        # update: one crossprod per chunk instead of a per-pair R-level loop with
        # O(len_w^2) allocations, same statistics.
        n_new = nrow(W)
        mean_new = colMeans(W)
        centered = sweep(W, 2L, mean_new)
        n_total = n_pairs + n_new
        delta = mean_new - w_mean
        w_M2 = w_M2 + crossprod(centered) + outer(delta, delta) * (n_pairs * n_new / n_total)
        w_mean = w_mean + delta * (n_new / n_total)
        n_pairs = n_total
        n_completed = n_completed + this_chunk

        if (xplain_opt("progress")) {
          cli::cli_progress_update(inc = 1)
        }

        # Running estimate + SEs for convergence tracking / history. Skipped when
        # the caller discards the history (resampling iterations after the first),
        # since the per-checkpoint delta-method SE is the expensive part.
        if (track_convergence) {
          est = estimate()
          convergence_history[[length(convergence_history) + 1L]] = data.table(
            n_permutations = n_completed, # coalition draws (shared column name for plotting)
            feature = features,
            importance = est$phi,
            se = est$se
          )
        }
      }

      if (xplain_opt("progress")) {
        cli::cli_progress_done()
      }

      est = estimate(require_result = TRUE)
      phi = est$phi
      names(phi) = features

      list(
        scores = data.table(
          feature = features,
          importance = as.numeric(phi),
          se = as.numeric(est$se)
        ),
        convergence_data = list(
          convergence_history = if (length(convergence_history) > 0) {
            rbindlist(convergence_history)
          } else {
            NULL
          },
          converged = FALSE,
          n_permutations_used = n_completed
        )
      )
    },

    # Exact SAGE estimator. Enumerates all 2^p coalitions, evaluates each once via
    # the shared value function, and computes the exact Shapley decomposition in
    # closed form. It has no coalition-sampling error (unlike the permutation and
    # kernel estimators), so it is a ground-truth reference on small feature sets.
    # The feature-count guard lives in $initialize(), so here p <= max_features.
    #
    # "Exact" is with respect to coalition sampling only: for ConditionalSAGE the
    # value function v(S) is itself a Monte Carlo estimate (the sampler redraws per
    # coalition), so exactness there removes only the coalition-sampling error.
    .compute_sage_scores_exact = function(learner, test_dt, batch_size = NULL) {
      features = self$features
      p = length(features)
      bit = bitwShiftL(1L, 0:(p - 1L)) # per-feature bit masks: 1, 2, 4, ...

      # Enumerate every coalition as a bitmask 0..2^p-1 (bit i set <=> feature i in S),
      # keeping list position k aligned with mask k-1 so losses index by mask.
      n_coal = bitwShiftL(1L, p) # 2^p, kept modest by the max_features guard
      masks = 0:(n_coal - 1L)
      coalitions = lapply(masks, function(mask) features[bitwAnd(mask, bit) > 0L])

      # Evaluate all coalitions in chunks; input order is preserved, so losses align
      # with masks. The chunk size bounds the materialized expansion (each coalition
      # expands to n_test * n_samples rows before prediction), while `batch_size`
      # separately bounds the rows per prediction call within a chunk.
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

      # Exact Shapley from the enumerated value function:
      # phi_i = sum_{S not containing i} (v(S+i) - v(S)) / (p * C(p-1, |S|)).
      size = lengths(coalitions) # |S| per coalition (list position k holds mask k-1)
      weight = 1 / (p * choose(p - 1L, size)) # by coalition, via its size |S|
      phi = numeric(p)
      for (i in seq_len(p)) {
        without = which(bitwAnd(masks, bit[i]) == 0L) # positions of masks lacking feature i
        with_i = without + bit[i] # position of the same mask plus feature i
        phi[i] = sum(weight[without] * (v[with_i] - v[without]))
      }
      names(phi) = features

      # The exact estimator has no coalition-sampling error, so there is no Monte Carlo
      # SE to report (se = NA); `ci_method = "montecarlo"` is therefore not applicable.
      list(
        scores = data.table(feature = features, importance = as.numeric(phi), se = NA_real_),
        convergence_data = list(
          convergence_history = NULL,
          converged = TRUE,
          n_permutations_used = n_coal
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

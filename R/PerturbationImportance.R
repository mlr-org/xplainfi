#' @title Perturbation Feature Importance Base Class
#'
#' @description Abstract base class for perturbation-based importance methods PFI, CFI, and RFI
#'
#' @export
PerturbationImportance = R6Class(
  "PerturbationImportance",
  inherit = FeatureImportanceMethod, # Inherit from existing base class
  public = list(
    #' @field sampler ([FeatureSampler]) Sampler object for feature perturbation
    sampler = NULL,
    #' @field weight_fun (`function(data, feature)` | `NULL`) Observation weight function, see `initialize()`.
    weight_fun = NULL,

    #' @description
    #' Creates a new instance of the PerturbationImportance class
    #' @param task,learner,measure,resampling,features,groups Passed to [FeatureImportanceMethod].
    #' @param sampler ([FeatureSampler]) Sampler to use for feature perturbation.
    #' @param relation (`character(1)`: `"difference"`) How to relate perturbed and baseline scores. Can also be `"ratio"`.
    #' @param n_repeats (`integer(1)`: `30L`) Number of permutation/conditional sampling iterations. Can also be overridden in `$compute()`.
    #' @param batch_size (`integer(1)` | `NULL`: `NULL`) Maximum number of rows to predict at once. When `NULL`, predicts all `test_size * n_repeats` rows in one call. Use smaller values to reduce memory usage at the cost of more prediction calls. Can be overridden in `$compute()`.
    #' @param weight_fun (`function(data, feature)` | `NULL`: `NULL`) Optional observation weight function.
    #'   Called once per feature (or group) and resampling iteration with `data`, a [data.table][data.table::data.table]
    #'   of the perturbed feature values (all task features, `test_size * n_repeats` rows), and `feature`,
    #'   the perturbed feature(s).
    #'   Must return a non-negative `numeric(nrow(data))`.
    #'   Weights are self-normalized and passed to the measure via [mlr3::Prediction]`$weights`,
    #'   so `measure` must have the `"weights"` property.
    #'   The baseline score stays unweighted.
    #'   See [weights_arf()] for importance sampling weights that turn marginal perturbation into a [CFI] estimator.
    #'   When set, `$scores()` gains a column `ess` with the effective sample size \eqn{(\sum w)^2 / \sum w^2}
    #'   of the weights, and `$obs_loss()` a column `weight` (scaled to mean 1 within its normalization group).
    #' @param normalize (`character(1)`: `"global"`) How weights from `weight_fun` are normalized.
    #'   `"global"`: across observations within each repeat (self-normalized importance sampling of the mean loss).
    #'   `"per_observation"`: across the `n_repeats` draws of each observation, so that
    #'   \eqn{\hat\mu_i = \frac{1}{R}\sum_r \tilde w_{ir} L_{ir}} is a per-observation conditional expectation;
    #'   this is the mode paired-test inference (`ci_method = "cpi"`) needs.
    #'   It requires a measure that aggregates as the mean of observation-wise losses
    #'   (e.g. `regr.mse`, `regr.mae`, `classif.ce`, `classif.logloss`; `regr.rmse` is not),
    #'   and `ess` then reports the mean per-observation effective sample size, which is at most `n_repeats`.
    #'   Observations whose weights all underflow to zero fall back to uniform weights, with a warning.
    #'   Ignored when `weight_fun` is `NULL`.
    initialize = function(
      task,
      learner,
      measure = NULL,
      resampling = NULL,
      features = NULL,
      groups = NULL,
      sampler = NULL,
      relation = "difference",
      n_repeats = 30L,
      batch_size = NULL,
      weight_fun = NULL,
      normalize = "global"
    ) {
      super$initialize(
        task = task,
        learner = learner,
        measure = measure,
        resampling = resampling,
        features = features,
        groups = groups,
        label = "Feature Importance (Abstract Class)"
      )

      # If no sampler is provided, create a default one (implementation dependent)
      self$sampler = sampler

      private$.assert_weight_fun(weight_fun)
      self$weight_fun = weight_fun

      # Set up common parameters for all perturbation-based methods
      ps = paradox::ps(
        relation = paradox::p_fct(c("difference", "ratio"), default = "difference"),
        n_repeats = paradox::p_int(lower = 1, default = 1),
        batch_size = paradox::p_int(lower = 1, special_vals = list(NULL), default = NULL),
        normalize = paradox::p_fct(c("global", "per_observation"), default = "global")
      )

      ps$values$relation = relation
      ps$values$n_repeats = n_repeats
      ps$values$batch_size = batch_size
      ps$values$normalize = checkmate::assert_choice(normalize, c("global", "per_observation"))
      self$param_set = ps

      # Add CPI to variance methods registry
      private$.ci_methods = c(private$.ci_methods, "cpi")
    },

    #' @description
    #' Get aggregated importance scores.
    #' Extends the base `$importance()` method to support `ci_method = "cpi"`.
    #' For details, see [CFI], which is the only sub-method for which it is known to be valid.
    #' @param relation (`character(1)`) How to relate perturbed scores to originals ("difference" or "ratio"). If `NULL`, uses stored parameter value.
    #' @param standardize (`logical(1)`: `FALSE`) If `TRUE`, importances are standardized by the highest score so all scores fall in `[-1, 1]`.
    #' @param ci_method (`character(1)`: `"none"`) Variance estimation method. In addition to base methods (`"none"`, `"raw"`, `"nadeau_bengio"`, `"quantile"`),
    #'   perturbation methods support `"cpi"` (Conditional Predictive Impact).
    #'   CPI is specifically designed for [CFI] with knockoff samplers and uses one-sided hypothesis tests.
    #' @param conf_level (`numeric(1)`: `0.95`) Confidence level for confidence intervals when `ci_method != "none"`.
    #' @param alternative (`character(1)`: `"two.sided"`) Type of alternative hypothesis for statistical tests.
    #'   `"greater"` tests H0: importance <= 0 vs H1: importance > 0 (one-sided).
    #'   `"two.sided"` tests H0: importance = 0 vs H1: importance != 0.
    #' @param test (`character(1)`: `"t"`) Test to use for CPI. One of `"t"`, `"wilcoxon"`, `"fisher"`, or `"binomial"`. Only used when `ci_method = "cpi"`.
    #' @param B (`integer(1)`: `1999`) Number of replications for Fisher test. Only used when `ci_method = "cpi"` and `test = "fisher"`.
    #' @param p_adjust (`character(1)`: `"none"`) Method for p-value adjustment for multiple comparisons.
    #'   Accepts any method supported by [stats::p.adjust.methods], e.g. `"holm"`, `"bonferroni"`, `"BH"`, `"none"`.
    #'   When `"bonferroni"`, confidence intervals are also adjusted (alpha/k).
    #'   For other correction methods (e.g. `"holm"`, `"BH"`), only p-values are adjusted;
    #'   confidence intervals remain at the nominal `conf_level` because these sequential/adaptive
    #'   procedures do not have a clean per-comparison alpha for CI construction.
    #' @param ... Additional arguments passed to the base method.
    #' @return ([data.table][data.table::data.table]) Aggregated importance scores.
    importance = function(
      relation = NULL,
      standardize = FALSE,
      ci_method = c("none", "raw", "nadeau_bengio", "quantile", "cpi"),
      conf_level = 0.95,
      alternative = c("two.sided", "greater"),
      test = c("t", "wilcoxon", "fisher", "binomial"),
      B = 1999,
      p_adjust = "none",
      ...
    ) {
      # Handle CPI separately, delegate rest to parent
      if (length(ci_method) > 1) {
        ci_method = ci_method[1]
      }
      alternative = match.arg(alternative)

      if (ci_method == "cpi") {
        # CPI requires special handling
        if (is.null(private$.scores)) {
          cli::cli_inform(c(
            x = "No importances computed yet!"
          ))
          return(invisible(NULL))
        }

        checkmate::assert_number(conf_level, lower = 0, upper = 1)

        # CPI does not support standardization - it uses obs-wise losses for inference
        if (standardize) {
          cli::cli_warn(c(
            "!" = "Standardization is not supported for CPI.",
            "i" = "CPI uses observation-wise losses for statistical inference.",
            "i" = "Ignoring {.code standardize = TRUE}."
          ))
        }

        # Call CPI function
        test = match.arg(test)
        agg_importance = importance_cpi(
          conf_level = conf_level,
          alternative = alternative,
          test = test,
          p_adjust = p_adjust,
          B = B,
          method_obj = self
        )

        setkeyv(agg_importance, "feature")
        return(agg_importance[])
      } else {
        # Delegate to parent for other methods
        super$importance(
          relation = relation,
          standardize = standardize,
          ci_method = ci_method,
          conf_level = conf_level,
          alternative = alternative,
          p_adjust = p_adjust,
          ...
        )
      }
    },

    #' @description
    #' Re-score the stored predictions with a different weight function, without perturbing or predicting again.
    #' The perturbed feature values from `$compute()` are kept, so any weight function can be applied afterwards.
    #' Since [weights_arf()] assumes the stored perturbation is a marginal permutation,
    #' use this on [PFI] or on [CFI] / [RFI] with `estimator = "weighting"`; it lets one prediction pass serve
    #' PFI, CFI, and any RFI conditioning set:
    #' ```r
    #' pfi$compute()
    #' pfi$importance()                                  # PFI
    #' pfi$reweight(weights_arf(sampler))$importance()   # CFI
    #' pfi$reweight(weights_arf(sampler, "x2"))$importance() # RFI given x2
    #' pfi$reweight(NULL)$importance()                   # back to PFI
    #' ```
    #' @param weight_fun (`function(data, feature)` | `NULL`: `NULL`) New weight function, see `initialize()`.
    #'   `NULL` removes weights.
    #' @param normalize (`character(1)` | `NULL`: `NULL`) Weight normalization, see `initialize()`.
    #'   `NULL` keeps the stored value.
    #' @return `invisible(self)`, with `$scores()` and `$obs_loss()` updated.
    reweight = function(weight_fun = NULL, normalize = NULL) {
      if (is.null(self$predictions)) {
        cli::cli_abort(c(
          x = "No stored predictions to reweight.",
          i = "Run {.fun $compute} first."
        ))
      }
      private$.assert_weight_fun(weight_fun)
      if (!is.null(normalize)) {
        self$param_set$values$normalize = checkmate::assert_choice(normalize, c("global", "per_observation"))
      }
      self$weight_fun = weight_fun
      private$.score_predictions(weight_fun, self$param_set$values$normalize)
      invisible(self)
    },

    #' @description
    #' Resets all stored fields populated by `$compute`, including the perturbed feature values used by `$reweight()`.
    reset = function() {
      super$reset()
      private$.perturbed = NULL
    }
  ),

  private = list(
    .assert_weight_fun = function(weight_fun) {
      checkmate::assert_function(weight_fun, args = c("data", "feature"), null.ok = TRUE)
      if (!is.null(weight_fun) && !("weights" %in% self$measure$properties)) {
        cli::cli_abort(c(
          x = "{.cls Measure} {.val {self$measure$id}} does not support observation weights.",
          i = "{.code weight_fun} requires a measure with the {.val weights} property."
        ))
      }
    },

    .compute_baseline = function(store_backends = TRUE) {
      self$resample_result = assemble_rr(
        task = self$task,
        learner = self$learner,
        resampling = self$resampling,
        store_models = TRUE,
        store_backends = store_backends
      )
      private$.baseline_scores()
    },

    .baseline_scores = function() {
      scores_baseline = self$resample_result$score(self$measure)[,
        .SD,
        .SDcols = c("iteration", self$measure$id)
      ]
      setnames(scores_baseline, old = self$measure$id, "score_baseline")
      setnames(scores_baseline, old = "iteration", "iter_rsmp")
      scores_baseline[]
    },

    # Common computation method for all perturbation-based methods
    .compute_perturbation_importance = function(
      n_repeats = NULL,
      batch_size = NULL,
      store_models = TRUE,
      store_backends = TRUE,
      sampler = NULL
    ) {
      # Use provided sampler or default to self$sampler
      sampler = sampler %||% self$sampler

      n_repeats = resolve_param(n_repeats, self$param_set$values$n_repeats, 1L)
      batch_size = resolve_param(batch_size, self$param_set$values$batch_size, NULL)

      private$.compute_baseline(store_backends = store_backends)

      # Get predictions for each resampling iter, permutation iter, feature
      # Create progress bar that tracks resampling_iter * feature/group combinations
      # if (xplain_opt("progress")) {
      # n_features_or_groups = length(self$groups %||% self$features)
      # total_iterations = self$resampling$iters * n_features_or_groups
      # progress_bar_id = cli::cli_progress_bar(
      # 	"Computing importances",
      # 	total = total_iterations
      # )
      # }

      all_preds = lapply(seq_len(self$resampling$iters), \(iter) {
        # Extract the learner here once because apparently reassembly is expensive
        this_learner = self$resample_result$learners[[iter]]
        test_row_ids = self$resampling$test_set(iter)
        test_size = length(test_row_ids)

        if (is.null(self$groups)) {
          iteration_proxy = self$features
          # name so lapply returns named list, used as idcol in rbindlist()
          names(iteration_proxy) = iteration_proxy
        } else {
          iteration_proxy = self$groups
        }

        # Use unified parallelization helper
        pred_per_feature = xplainfi_map(
          length(iteration_proxy),
          \(
            foi,
            task,
            learner,
            sampler,
            test_row_ids,
            n_repeats,
            batch_size,
            learner_packages,
            is_sequential = TRUE
          ) {
            # Load required packages in parallel workers
            if (!is_sequential) {
              library("data.table")
              library("mlr3")
              library("xplainfi")
              for (pkg in learner_packages) {
                library(pkg, character.only = TRUE)
              }
              # Force sequential forging inside the daemon. The outer mirai layer
              # already parallelizes across features, so letting the sampler's
              # stored `parallel` flag drive nested `arf::forge()` parallelism
              # here would only oversubscribe cores. That flag reflects fit-time
              # config (`adversarial_rf`/`forde`, done once in the caller) and
              # must not leak into sample-time forging. Set it on the daemon-local
              # sampler copy, samplers without a `parallel` param (all non-ARF
              # samplers) are unaffected.
              if ("parallel" %in% sampler$param_set$ids()) {
                sampler$param_set$set_values(parallel = FALSE)
              }
            }

            # Sampler produces `samples_per_row * test_size` rows in draw-major order;
            # see `FeatureSampler$sample()` for the contract.
            test_size = length(test_row_ids)
            perturbed_data = sampler$sample(
              foi,
              row_ids = test_row_ids,
              samples_per_row = n_repeats
            )

            # Split into n_repeats groups, each of test_size rows in test_row_ids order.
            perturbed_data_list = split(
              perturbed_data,
              rep(seq_len(n_repeats), each = test_size)
            )

            # Use batched prediction helper
            preds = predict_batched(
              learner = learner,
              data_list = perturbed_data_list,
              task = task,
              test_row_ids = test_row_ids,
              batch_size = batch_size
            )

            # Store predictions in data.table list column
            pred_per_perm = lapply(preds, \(pred) data.table::data.table(prediction = list(pred)))

            # Append iteration id for within-resampling permutations
            preds_dt = data.table::rbindlist(pred_per_perm, idcol = "iter_repeat")
            # Only the perturbed columns are kept (all repeats, draw-major), so that
            # `$reweight()` can rebuild the perturbed data without the sampler.
            list(
              preds = preds_dt,
              perturbed = perturbed_data[, .SD, .SDcols = foi]
            )
          },
          iteration_proxy, # Varying argument
          .args = list(
            task = self$task,
            learner = this_learner,
            sampler = sampler,
            test_row_ids = test_row_ids,
            n_repeats = n_repeats,
            batch_size = batch_size,
            learner_packages = this_learner$packages
          )
        )

        # When groups are defined, "feature" is the group name
        # mild misnomer for convenience because if-else'ing the column name is annoying
        list(
          preds = rbindlist(lapply(pred_per_feature, `[[`, "preds"), idcol = "feature"),
          perturbed = data.table::data.table(
            feature = names(iteration_proxy),
            perturbed = lapply(pred_per_feature, `[[`, "perturbed")
          )
        )
      })
      # Append iteration id for resampling
      self$predictions = rbindlist(lapply(all_preds, `[[`, "preds"), idcol = "iter_rsmp")
      private$.perturbed = rbindlist(lapply(all_preds, `[[`, "perturbed"), idcol = "iter_rsmp")

      # Close progress bar
      # if (xplain_opt("progress")) {
      # 	cli::cli_progress_done(id = progress_bar_id)
      # }

      private$.score_predictions(self$weight_fun, self$param_set$values$normalize)
    },

    # Score stored predictions, optionally weighted. Separated from perturbation
    # and prediction so that `$reweight()` can re-run it alone.
    # Abort unless the measure aggregates as the mean of its observation-wise losses,
    # which per-observation normalization relies on when it bypasses `pred$score()`.
    .assert_mean_decomposable = function() {
      measure = self$measure
      if (!has_obs_loss(measure)) {
        cli::cli_abort(c(
          x = "{.code normalize = \"per_observation\"} requires observation-wise losses.",
          i = "{.cls Measure} {.val {measure$id}} has no {.fun $obs_loss}."
        ))
      }
      pred = self$resample_result$predictions()[[1]]
      score = pred$score(measures = measure)[[measure$id]]
      if (abs(score - mean(pred$obs_loss(measures = measure)[[measure$id]])) > 1e-8) {
        cli::cli_abort(c(
          x = "{.cls Measure} {.val {measure$id}} does not aggregate as the mean of its observation-wise losses.",
          i = "{.code normalize = \"per_observation\"} needs a mean-decomposable measure such as
               {.val regr.mse}, {.val regr.mae}, {.val classif.ce}, or {.val classif.logloss}."
        ))
      }
    },

    .score_predictions = function(weight_fun = NULL, normalize = "global") {
      all_preds = self$predictions
      scores_baseline = private$.baseline_scores()
      per_obs = !is.null(weight_fun) && normalize == "per_observation"
      if (per_obs) {
        private$.assert_mean_decomposable()
      }
      # Per-observation ESS is a property of a (feature, iter_rsmp) block, not of a repeat.
      ess_block = data.table(feature = character(), iter_rsmp = integer(), ess = numeric())

      if (is.null(weight_fun)) {
        for (pred in all_preds$prediction) {
          pred$data$weights = NULL
        }
      } else {
        feature_names = self$task$feature_names
        for (i in seq_len(nrow(private$.perturbed))) {
          feat = private$.perturbed$feature[[i]]
          iter = private$.perturbed$iter_rsmp[[i]]
          perturbed = private$.perturbed$perturbed[[i]]
          foi = if (is.null(self$groups)) feat else self$groups[[feat]]

          # Rebuild the full perturbed feature data: test rows replicated draw-major,
          # perturbed columns swapped in.
          test_row_ids = self$resampling$test_set(iter)
          test_size = length(test_row_ids)
          n_repeats = nrow(perturbed) %/% test_size
          data = self$task$data(rows = test_row_ids, cols = feature_names)[rep(seq_len(test_size), times = n_repeats)]
          data[, (foi) := perturbed]

          weights = weight_fun(data, foi)
          checkmate::assert_numeric(weights, len = nrow(data), lower = 0, any.missing = FALSE)
          # Draw-major vector -> observations in rows, repeats in columns
          w = matrix(weights, nrow = test_size, ncol = n_repeats)
          if (per_obs) {
            row_mean = rowMeans(w)
            # Weights are exp(log_w - max) over the whole block, so rows far from the max can underflow entirely.
            zero = row_mean == 0
            if (any(zero)) {
              cli::cli_warn(c(
                "!" = "{sum(zero)} observation{?s} of feature {.val {feat}} had all-zero weights and fall back to uniform weights."
              ))
              w[zero, ] = 1
              row_mean[zero] = 1
            }
            ess_block = rbind(
              ess_block,
              data.table(feature = feat, iter_rsmp = iter, ess = mean(rowSums(w)^2 / rowSums(w^2)))
            )
            w = w / row_mean
          } else {
            w = sweep(w, 2L, colMeans(w), "/")
          }
          preds = all_preds[feature == feat & iter_rsmp == iter][order(iter_repeat)]
          for (r in seq_len(n_repeats)) {
            preds$prediction[[r]]$data$weights = w[, r]
          }
        }
      }

      score_fun = if (per_obs) {
        # Weighted mean with weights normalized across repeats; `pred$score()` would re-normalize across observations.
        \(p) mean(p$weights * p$obs_loss(measures = self$measure)[[self$measure$id]])
      } else {
        \(p) p$score(measures = self$measure)[[self$measure$id]]
      }
      scores = data.table::copy(all_preds)[, score_post := vapply(prediction, score_fun, FUN.VALUE = numeric(1))]
      vars_to_keep = c("feature", "iter_rsmp", "iter_repeat", "score_baseline", "score_post")
      if (per_obs) {
        scores = ess_block[scores, on = c("feature", "iter_rsmp")]
        vars_to_keep = c(vars_to_keep, "ess")
      } else if (!is.null(weight_fun)) {
        # Effective sample size as diagnostic for how concentrated the weights are
        scores[, ess := vapply(prediction, \(p) sum(p$weights)^2 / sum(p$weights^2), FUN.VALUE = numeric(1))]
        vars_to_keep = c(vars_to_keep, "ess")
      }
      scores = scores[scores_baseline, on = c("iter_rsmp")]
      private$.scores = scores[, .SD, .SDcols = vars_to_keep]

      # for obs_loss:
      # Not all losses are decomposable so this is optional and depends on the provided measure
      if (has_obs_loss(self$measure)) {
        grouping_vars = c("feature", "iter_rsmp", "iter_repeat")

        obs_loss_all = all_preds[,
          {
            pred = prediction[[1]]

            # Get only vector of obs losses, Prediction$obs_loss() returns full table
            obs_loss_vals = pred$obs_loss(measures = self$measure)[[self$measure$id]]

            c(
              list(row_ids = pred$row_ids, loss_post = obs_loss_vals),
              if (!is.null(pred$weights)) list(weight = pred$weights)
            )
          },
          by = grouping_vars
        ]

        private$.obs_losses = obs_loss_all
      }
    },

    # Perturbed feature columns per feature/group and resampling iteration, see `.score_predictions()`.
    .perturbed = NULL
  )
)


#' @title Permutation Feature Importance
#'
#' @description
#' Implementation of Permutation Feature Importance (PFI) using modular sampling approach.
#' PFI measures the importance of a feature by calculating the increase in model error
#' when the feature's values are randomly permuted, breaking the relationship between
#' the feature and the target variable.
#'
#' @details
#' Permutation Feature Importance was originally introduced by Breiman (2001) as part of
#' the Random Forest algorithm. The method works by:
#' 1. Computing baseline model performance on the original dataset
#' 2. For each feature, randomly permuting its values while keeping other features unchanged
#' 3. Computing model performance on the permuted dataset
#' 4. Calculating importance as the difference (or ratio) between permuted and original performance
#'
#' @references
#' `r print_bib("breiman_2001")`
#' `r print_bib("fisher_2019")`
#' `r print_bib("strobl_2008")`
#'
#' @examples
#' library(mlr3)
#'
#' task <- sim_dgp_correlated(n = 500)
#'
#' pfi <- PFI$new(
#'   task = task,
#'   learner = lrn("regr.rpart"),
#'   measure = msr("regr.mse"),
#' n_repeats = 5
#' )
#' pfi$compute()
#' pfi$importance()
#' @export
PFI = R6Class(
  "PFI",
  inherit = PerturbationImportance,
  public = list(
    #' @description
    #' Creates a new instance of the PFI class
    #' @param task,learner,measure,resampling,features,groups,relation,n_repeats,batch_size,weight_fun,normalize Passed to [PerturbationImportance]
    initialize = function(
      task,
      learner,
      measure = NULL,
      resampling = NULL,
      features = NULL,
      groups = NULL,
      relation = "difference",
      n_repeats = 30L,
      batch_size = NULL,
      weight_fun = NULL,
      normalize = "global"
    ) {
      super$initialize(
        task = task,
        learner = learner,
        measure = measure,
        resampling = resampling,
        features = features,
        groups = groups,
        sampler = MarginalPermutationSampler$new(task),
        relation = relation,
        n_repeats = n_repeats,
        batch_size = batch_size,
        weight_fun = weight_fun,
        normalize = normalize
      )

      self$label = "Permutation Feature Importance"
    },

    #' @description
    #' Compute PFI scores
    #' @param n_repeats (`integer(1)`; `NULL`) Number of permutation iterations. If `NULL`, uses stored value.
    #' @param batch_size (`integer(1)` | `NULL`: `NULL`) Maximum number of rows to predict at once. If `NULL`, uses stored value.
    #' @param store_models,store_backends (`logical(1)`: `TRUE`) Whether to store fitted models / data backends, passed to [mlr3::resample] internally
    #' for the initial fit of the learner.
    #' This may be required for certain measures and is recommended to leave enabled unless really necessary.
    compute = function(
      n_repeats = NULL,
      batch_size = NULL,
      store_models = TRUE,
      store_backends = TRUE
    ) {
      # PFI uses the MarginalPermutationSampler directly
      private$.compute_perturbation_importance(
        n_repeats = n_repeats,
        batch_size = batch_size,
        store_models = store_models,
        store_backends = store_backends,
        sampler = self$sampler
      )
    }
  )
)

#' @title Conditional Feature Importance
#'
#' @description Implementation of CFI using modular sampling approach
#'
#' @details
#'
#' CFI replaces feature values with conditional samples from the distribution of
#' the feature given the other features. Any [ConditionalSampler] or [KnockoffSampler] can be used.
#'
#' ## Estimators
#'
#' - `estimator = "sampling"` (default): draw \eqn{x_j' \sim p(x_j \mid x_{-j})} with `sampler`.
#' - `estimator = "weighting"`: permute \eqn{x_j} marginally as in [PFI] and weight each perturbed observation
#'   by the density ratio \eqn{p(x_j' \mid x_{-j}) / p(x_j')}, estimated by the ARF in `sampler`
#'   (see [weights_arf()]).
#'   This is an importance sampling estimator of the same quantity that needs only density evaluations
#'   ([arf::lik]) instead of conditional draws ([arf::forge]).
#'   It uses the same perturbed data and predictions as [PFI], and its runtime is comparable to `"sampling"`
#'   (`forge` amortizes over `n_repeats`, `lik` does not).
#'   Requires a [ConditionalARFSampler] (preferably fitted with `finite_bounds = "local"`, see [weights_arf()])
#'   and a measure with the `"weights"` property.
#'   The effective sample size of the weights is reported as `ess` in `$scores()`;
#'   small values indicate that few observations dominate and `"sampling"` should be preferred.
#'   `ci_method = "cpi"` is available only with `normalize = "per_observation"` and is not yet validated for it.
#'
#' ## Statistical Inference
#'
#' Two approaches for statistical inference are primarily supported via
#' `$importance(ci_method = "cpi")`:
#'
#' - **CPI** (Watson & Wright, 2021): The original Conditional Predictive Impact method,
#'   designed for use with knockoff samplers ([KnockoffGaussianSampler]).
#'
#' - **cARFi** (Blesch et al., 2025): CFI with ARF-based conditional sampling
#'   ([ConditionalARFSampler]), using the same CPI inference framework.
#'
#' Both require a decomposable measure (e.g., MSE) and out-of-sample evaluation.
#' CPI inference is guaranteed to be valid with holdout (a single train/test split).
#' With cross-validation, test observations are i.i.d. but models are fit on
#' overlapping training data, which may affect inference coverage. With bootstrap
#' or subsampling, both non-i.i.d. test observations and overlapping training data
#' can be an issue. See `vignette("inference", package = "xplainfi")` for details.
#'
#' Available tests: `"t"` (t-test), `"wilcoxon"` (signed-rank), `"fisher"` (permutation),
#' `"binomial"` (sign test). The Fisher test is recommended.
#'
#' Method-agnostic inference methods (`"raw"`, `"nadeau_bengio"`, `"quantile"`) are also
#' available; see [FeatureImportanceMethod] for details.
#'
#' For a comprehensive overview of inference methods including usage examples,
#' see `vignette("inference", package = "xplainfi")`.
#'
#' @references `r print_bib("watson_2021", "blesch_2025")`
#'
#' @examples
#' library(mlr3)
#'
#' task <- sim_dgp_correlated(n = 200)
#'
#' # Using default ConditionalARFSampler
#' cfi <- CFI$new(
#'   task = task,
#'   learner = lrn("regr.rpart"),
#'   measure = msr("regr.mse"),
#'   sampler = ConditionalGaussianSampler$new(task),
#'   n_repeats = 5
#' )
#' cfi$compute()
#' cfi$importance()
#' @export
CFI = R6Class(
  "CFI",
  inherit = PerturbationImportance,
  public = list(
    #' @description
    #' Creates a new instance of the CFI class
    #' @param task,learner,measure,resampling,features,groups,relation,n_repeats,batch_size Passed to [PerturbationImportance].
    #' @param sampler ([ConditionalSampler]) Optional custom sampler. Defaults to instantiating `ConditionalARFSampler` internally with default parameters.
    #' @param estimator (`character(1)`: `"sampling"`) `"sampling"` for conditional sampling with `sampler`,
    #'   or `"weighting"` for marginal permutation with ARF-based importance sampling weights. See Details.
    #' @param normalize (`character(1)`: `"global"`) Weight normalization for `estimator = "weighting"`,
    #'   passed to [PerturbationImportance].
    initialize = function(
      task,
      learner,
      measure = NULL,
      resampling = NULL,
      features = NULL,
      groups = NULL,
      relation = "difference",
      n_repeats = 30L,
      batch_size = NULL,
      sampler = NULL,
      estimator = "sampling",
      normalize = "global"
    ) {
      estimator = checkmate::assert_choice(estimator, c("sampling", "weighting"))
      # Use ConditionalARFSampler by default for CFI
      if (is.null(sampler)) {
        sampler = ConditionalARFSampler$new(task)
        if (xplain_opt("verbose")) {
          cli::cli_alert_info(
            "No {.code sampler} provided, using {.cls ConditionalARFSampler} with default settings."
          )
        }
      }
      # checkmate::assert_class would expect sampler to inherit from all clases, but
      # the two are mutually exclusive (for now?)
      if (!inherits(sampler, c("ConditionalSampler", "KnockoffSampler"))) {
        cli::cli_abort(c(
          x = "Provided sampler is of class {.cls {class(sampler)[[1]]}}.",
          "!" = "Either a {.cls ConditionalSampler} or a {.cls KnockoffSampler} is needed for {.cls CFI}.",
          i = "Choose a supported {.cls FeatureSampler}, such as {.cls ConditionalARFSampler} or {.class KnockoffGaussianSampler}."
        ))
      }
      if (
        inherits(sampler, "ConditionalSampler") &&
          !is.null(sampler$param_set$values$conditioning_set)
      ) {
        cli::cli_warn(c(
          "!" = "Provided sampler has a pre-configured {.code conditioning_set}.",
          i = "To calculate {.cls CFI} correctly, {.code conditioning_set} will be reset such that sampling is performed conditionally on all remaining features."
        ))
        sampler$param_set$values$conditioning_set = NULL
      }

      weight_fun = NULL
      if (estimator == "weighting") {
        if (!inherits(sampler, "ConditionalARFSampler")) {
          cli::cli_abort(c(
            x = "Provided sampler is of class {.cls {class(sampler)[[1]]}}.",
            "!" = "{.code estimator = \"weighting\"} requires a {.cls ConditionalARFSampler} to evaluate densities."
          ))
        }
        # Perturbation is marginal, the ARF only supplies the weights.
        weight_fun = weights_arf(sampler)
        sampler = MarginalPermutationSampler$new(task)
      }

      super$initialize(
        task = task,
        learner = learner,
        measure = measure,
        resampling = resampling,
        features = features,
        groups = groups,
        sampler = sampler,
        relation = relation,
        n_repeats = n_repeats,
        batch_size = batch_size,
        weight_fun = weight_fun,
        normalize = normalize
      )

      cfi_ps = paradox::ps(
        estimator = paradox::p_fct(c("sampling", "weighting"), default = "sampling")
      )
      cfi_ps$values$estimator = estimator
      self$param_set = c(self$param_set, cfi_ps)

      self$label = "Conditional Feature Importance"
    },

    #' @description
    #' Compute CFI scores
    #' @param n_repeats (`integer(1)`) Number of permutation iterations. If `NULL`, uses stored value.
    #' @param batch_size (`integer(1)` | `NULL`: `NULL`) Maximum number of rows to predict at once. If `NULL`, uses stored value.
    #' @param store_models,store_backends (`logical(1)`: `TRUE`) Whether to store fitted models / data backends, passed to [mlr3::resample] internally
    #' for the initial fit of the learner.
    #' This may be required for certain measures and is recommended to leave enabled unless really necessary.
    compute = function(
      n_repeats = NULL,
      batch_size = NULL,
      store_models = TRUE,
      store_backends = TRUE
    ) {
      # CFI expects sampler configured to condition on all other features for each feature
      # Default for ConditionalARFSampler
      private$.compute_perturbation_importance(
        n_repeats = n_repeats,
        batch_size = batch_size,
        store_models = store_models,
        store_backends = store_backends,
        sampler = self$sampler
      )
    }
  )
)

#' @title Relative Feature Importance
#'
#' @description RFI generalizes CFI and PFI with arbitrary conditioning sets and samplers.
#'
#' @references `r print_bib("konig_2021")`
#'
#' @examples
#' library(mlr3)
#' task = tgen("friedman1")$generate(n = 200)
#' rfi = RFI$new(
#'   task = task,
#'   learner = lrn("regr.rpart"),
#'   measure = msr("regr.mse"),
#'   conditioning_set = c("important1"),
#'   sampler = ConditionalGaussianSampler$new(task),
#'   n_repeats = 5
#' )
#' rfi$compute()
#' rfi$importance()
#' @export
RFI = R6Class(
  "RFI",
  inherit = PerturbationImportance,
  public = list(
    #' @description
    #' Creates a new instance of the RFI class
    #' @param task,learner,measure,resampling,features,groups,relation,n_repeats,batch_size Passed to [PerturbationImportance].
    #' @param conditioning_set ([character()]) Set of features to condition on. Can be overridden in `$compute()`.
    #'   Default (`character(0)`) is equivalent to `PFI`. In `CFI`, this would be set to all features except that of interest.
    #' @param sampler ([ConditionalSampler]) Optional custom sampler. Defaults to `ConditionalARFSampler`.
    #' @param weight_fun (`function(data, feature)` | `NULL`: `NULL`) Passed to [PerturbationImportance].
    #'   Ignored when `estimator = "weighting"`.
    #' @param estimator (`character(1)`: `"sampling"`) `"sampling"` for conditional sampling with `sampler`,
    #'   or `"weighting"` for marginal permutation with ARF-based importance sampling weights
    #'   \eqn{p(x_j' \mid x_S) / p(x_j')} for the conditioning set \eqn{S}. See [CFI] and [weights_arf()].
    #' @param normalize (`character(1)`: `"global"`) Weight normalization, passed to [PerturbationImportance].
    initialize = function(
      task,
      learner,
      measure = NULL,
      resampling = NULL,
      features = NULL,
      groups = NULL,
      conditioning_set = NULL,
      relation = "difference",
      n_repeats = 30L,
      batch_size = NULL,
      sampler = NULL,
      weight_fun = NULL,
      estimator = "sampling",
      normalize = "global"
    ) {
      estimator = checkmate::assert_choice(estimator, c("sampling", "weighting"))
      # Use ConditionalARFSampler by default for RFI
      if (is.null(sampler)) {
        sampler = ConditionalARFSampler$new(task)
        if (xplain_opt("verbose")) {
          cli::cli_alert_info(
            "No {.cls ConditionalSampler} provided, using {.cls ConditionalARFSampler} with default settings."
          )
        }
      } else {
        checkmate::assert_class(sampler, "ConditionalSampler")
      }

      if (estimator == "weighting") {
        if (!inherits(sampler, "ConditionalARFSampler")) {
          cli::cli_abort(c(
            x = "Provided sampler is of class {.cls {class(sampler)[[1]]}}.",
            "!" = "{.code estimator = \"weighting\"} requires a {.cls ConditionalARFSampler} to evaluate densities."
          ))
        }
        # Keep the ARF for rebuilding weights when `$compute()` overrides the conditioning set.
        private$.arf_sampler = sampler
        sampler = MarginalPermutationSampler$new(task)
      }

      if (!is.null(conditioning_set)) {
        conditioning_set = checkmate::assert_subset(conditioning_set, task$feature_names)
      } else {
        # Default to empty set (equivalent(ish) to PFI)
        cli::cli_warn(c(
          "Using empty conditioning set",
          i = "Set {.code conditioning_set} to condition on features."
        ))
        conditioning_set = character(0)
      }

      # Configure the sampler (or the weights) with the conditioning_set
      if (estimator == "weighting") {
        weight_fun = weights_arf(private$.arf_sampler, conditioning_set)
      } else {
        sampler$param_set$values$conditioning_set = conditioning_set
      }

      super$initialize(
        task = task,
        learner = learner,
        measure = measure,
        resampling = resampling,
        features = features,
        groups = groups,
        sampler = sampler,
        relation = relation,
        n_repeats = n_repeats,
        batch_size = batch_size,
        weight_fun = weight_fun,
        normalize = normalize
      )

      # Create extended param_set for RFI with conditioning_set parameter
      rfi_ps = paradox::ps(
        conditioning_set = paradox::p_uty(default = character(0)),
        estimator = paradox::p_fct(c("sampling", "weighting"), default = "sampling")
      )
      rfi_ps$values$conditioning_set = conditioning_set
      rfi_ps$values$estimator = estimator
      self$param_set = c(self$param_set, rfi_ps)

      self$label = "Relative Feature Importance"
    },

    #' @description
    #' Compute RFI scores
    #' @param conditioning_set (`character()`) Set of features to condition on. If `NULL`, uses the stored parameter value.
    #' @param n_repeats (`integer(1)`) Number of permutation iterations. If `NULL`, uses stored value.
    #' @param batch_size (`integer(1)` | `NULL`: `NULL`) Maximum number of rows to predict at once. If `NULL`, uses stored value.
    #' @param store_models,store_backends (`logical(1)`: `TRUE`) Whether to store fitted models / data backends, passed to [mlr3::resample] internally
    #' for the initial fit of the learner.
    #' This may be required for certain measures and is recommended to leave enabled unless really necessary.
    compute = function(
      conditioning_set = NULL,
      n_repeats = NULL,
      batch_size = NULL,
      store_models = TRUE,
      store_backends = TRUE
    ) {
      # Handle conditioning_set parameter override
      if (!is.null(conditioning_set)) {
        # Validate the provided conditioning_set
        conditioning_set = checkmate::assert_subset(conditioning_set, self$task$feature_names)

        # Clear cache and temporarily modify sampler's conditioning_set (or the weights)
        private$.scores = NULL
        if (self$param_set$values$estimator == "weighting") {
          old_weight_fun = self$weight_fun
          self$weight_fun = weights_arf(private$.arf_sampler, conditioning_set)
          on.exit(self$weight_fun <- old_weight_fun)
        } else {
          old_conditioning_set = self$sampler$param_set$values$conditioning_set
          self$sampler$param_set$values$conditioning_set = conditioning_set
          on.exit(self$sampler$param_set$values$conditioning_set <- old_conditioning_set)
        }
      }

      # Use the (potentially modified) sampler
      private$.compute_perturbation_importance(
        n_repeats = n_repeats,
        batch_size = batch_size,
        store_models = store_models,
        store_backends = store_backends,
        sampler = self$sampler
      )
    }
  ),
  private = list(
    .arf_sampler = NULL
  )
)

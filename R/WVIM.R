#' @title Williamson's Variable Importance Measure (WVIM)
#'
#' @description
#' Base class generalizing refit-based variable importance measures.
#' Default corresponds to leaving out each feature `n_repeats` times, which
#' corresponds to LOCO (Leave One Covariate Out).
#'
#' @examplesIf requireNamespace("ranger", quietly = TRUE) && requireNamespace("mlr3learners", quietly = TRUE)
#' library(mlr3)
#' library(mlr3learners)
#'
#' task <- sim_dgp_correlated(n = 500)
#'
#' # Group correlated features together, independent features separately
#' groups <- list(
#'   correlated = c("x1", "x2"),
#'   independent = c("x3", "x4")
#' )
#'
#' wvim <- WVIM$new(
#'   task = task,
#'   learner = lrn("regr.ranger", num.trees = 10),
#'   groups = groups
#' )
#' wvim$compute()
#' wvim$importance()
#' @export
#' @keywords internal
WVIM = R6Class(
	"WVIM",
	inherit = FeatureImportanceMethod,
	public = list(
		#' @field direction (`character(1)`) Either "leave-out" or "leave-in".
		direction = NULL,
		#' @field design (`logical()`) Feature selection design matrix where `TRUE` equals "left in" and `FALSE` "left out".
		#' 	Columns correspond to `task$feature_names` and the number of rows corresponds to `length(features) * n_repeats`.
		#'  The base matrix is created by [wvim_design_matrix] and then replicated `n_repeats` times before.
		design = NULL,
		#' @field instance (`FSelectInstanceBatchSingleCrit`) The `mlr3fselect` feature selection instance containing
		#'   also the archive of all evaluations, possible useful for future use. Only stored if `store_instance` is `TRUE`.
		instance = NULL,
		#' @description
		#' Creates a new instance of this [R6][R6::R6Class] class.
		#' @param task,learner,measure,resampling,features,groups Passed to `FeatureImportanceMethod` for construction.
		#' @param direction (`character(1)`) Either "leave-out" or "leave-in".
		#' @param label (`character(1)`) Method label.
		#' @param n_repeats (`integer(1)`: `30L`) Number of refit iterations per resampling iteration.
		initialize = function(
			task,
			learner,
			measure = NULL,
			resampling = NULL,
			features = NULL,
			groups = NULL,
			direction = c("leave-out", "leave-in"),
			label = "Williamson's Variable Importance Measure (WVIM)",
			n_repeats = 30L
		) {
			require_package("mlr3fselect")

			# Should this go in the param_set?
			direction = match.arg(direction)
			self$direction = direction
			checkmate::assert_int(n_repeats, lower = 1L)

			# params
			ps = ps(
				relation = paradox::p_fct(c("difference", "ratio"), default = "difference"),
				n_repeats = paradox::p_int(lower = 1L, default = 1L)
			)
			ps$values = list(
				relation = "difference",
				n_repeats = n_repeats
			)

			# If learner is already trained, warn that the model will be ignored
			# Note that resample() clones the learner anyway later.
			if (!is.null(learner$model)) {
				cli::cli_warn(c(
					"{.cls Learner} is already trained, which is not compatible with refit-based importance methods",
					"!" = "Given {.cls Learner} will be cloned.",
					"i" = "Provide an untrained {.cls Learner} to avoid this warning."
				))
			}

			super$initialize(
				task = task,
				learner = learner,
				measure = measure,
				resampling = resampling,
				features = features,
				groups = groups,
				param_set = ps,
				label = label
			)

			# Add Lei et al. (2018) inference to CI methods registry
			private$.ci_methods = c(private$.ci_methods, "lei")
		},

		#' @description
		#' Get aggregated importance scores.
		#' Extends the base `$importance()` method to support `ci_method = "lei"`.
		#'
		#' This implements distribution-free inference based on Lei et al. (2018),
		#' testing observation-wise loss differences using the Wilcoxon signed-rank test by default.
		#'
		#' Lei et al. (2018) proposed this method specifically for LOCO with L1 (absolute) loss,
		#' median aggregation, and a single train/test split (holdout). The inference is
		#' conditional on the training data, requiring i.i.d. test observations from a single
		#' split. While the aggregation function, statistical test, and resampling strategy are
		#' parameterizable, deviating from these defaults may invalidate the theoretical guarantees.
		#'
		#' For a comprehensive overview of inference methods, see
		#' `vignette("inference", package = "xplainfi")`.
		#'
		#' @param relation (`character(1)`) How to relate perturbed scores to originals ("difference" or "ratio"). If `NULL`, uses stored parameter value.
		#' @param standardize (`logical(1)`: `FALSE`) If `TRUE`, importances are standardized by the highest score so all scores fall in `[-1, 1]`.
		#' @param ci_method (`character(1)`: `"none"`) Variance estimation method. In addition to base methods (`"none"`, `"raw"`, `"nadeau_bengio"`, `"quantile"`),
		#'   WVIM methods support `"lei"` for distribution-free inference (Lei et al., 2018).
		#' @param conf_level (`numeric(1)`: `0.95`) Confidence level to use for confidence interval construction when `ci_method != "none"`.
		#' @param alternative (`character(1)`: `"greater"`) Type of alternative hypothesis for statistical tests.
		#'   `"greater"` tests H0: importance <= 0 vs H1: importance > 0 (one-sided).
		#'   `"two.sided"` tests H0: importance = 0 vs H1: importance != 0.
		#' @param test (`character(1)`: `"wilcoxon"`) Test to use for Lei et al. inference. One of `"wilcoxon"`, `"t"`, `"fisher"`, or `"binomial"`.
		#'   Only used when `ci_method = "lei"`.
		#' @param B (`integer(1)`: `1999`) Number of replications for Fisher test. Only used when `ci_method = "lei"` and `test = "fisher"`.
		#' @param aggregator (`function`: `stats::median`) Aggregation function for computing the point estimate from observation-wise
		#'   importance values. Defaults to `stats::median` as proposed by Lei et al. (2018). Only used when `ci_method = "lei"`.
		#' @param p_adjust (`character(1)`: `"none"`) Method for p-value adjustment for multiple comparisons.
		#'   Accepts any method supported by [stats::p.adjust.methods], e.g. `"holm"`, `"bonferroni"`, `"BH"`, `"none"`.
		#'   When `"bonferroni"`, confidence intervals are also adjusted (alpha/k).
		#'   For other correction methods (e.g. `"holm"`, `"BH"`), only p-values are adjusted;
		#'   confidence intervals remain at the nominal `conf_level` because these sequential/adaptive
		#'   procedures do not have a clean per-comparison alpha for CI construction.
		#' @param ... Additional arguments passed to the base method.
		#' @return ([data.table][data.table::data.table]) Aggregated importance scores.
		#' @references `r print_bib("lei_2018")`
		importance = function(
			relation = NULL,
			standardize = FALSE,
			ci_method = c("none", "raw", "nadeau_bengio", "quantile", "lei"),
			conf_level = 0.95,
			alternative = c("greater", "two.sided"),
			test = c("wilcoxon", "t", "fisher", "binomial"),
			B = 1999,
			aggregator = NULL,
			p_adjust = "none",
			...
		) {
			if (length(ci_method) > 1) {
				ci_method = ci_method[1]
			}
			alternative = match.arg(alternative)

			if (ci_method == "lei") {
				if (is.null(private$.scores)) {
					cli::cli_inform(c(x = "No importances computed yet!"))
					return(invisible(NULL))
				}

				# Default aggregator for Lei et al. is median
				aggregator = aggregator %||% stats::median

				test = match.arg(test)
				agg_importance = importance_loco(
					conf_level = conf_level,
					alternative = alternative,
					test = test,
					aggregator = aggregator,
					p_adjust = p_adjust,
					B = B,
					method_obj = self
				)

				if (standardize) {
					agg_importance[, importance := importance / max(abs(importance), na.rm = TRUE)]
				}

				setkeyv(agg_importance, "feature")
				return(agg_importance[])
			}

			# Default to parent method for other ci_methods
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
		#' Computes leave-out or leave-in feature importance.
		# @param design (`data.table`) A design matrix indicating which features are to be left in or out
		# at each step. Can be created with [wvim_design_matrix].
		#' `wvim_design_matrix(task$feature_names, "leave-out")` corresponds to LOCO.
		#' @param store_models,store_backends (`logical(1)`: `TRUE`) Whether to store fitted models / data backends, passed to [mlr3::resample] internally
		#'   backends in resample result.
		#'   Required for some measures, but may increase memory footprint.
		#' @param store_instance (`logical(1)`: `FALSE`) Whether to store the [mlr3fselect] instance in `$instance`.
		compute = function(store_models = TRUE, store_backends = TRUE, store_instance = FALSE) {
			if (is.null(self$groups)) {
				feature_or_groups = self$features
			} else {
				feature_or_groups = self$groups
			}

			design = wvim_design_matrix(
				all_features = self$task$feature_names,
				feature_names = feature_or_groups,
				direction = self$direction
			)

			design = data.table::rbindlist(replicate(
				n = self$param_set$values$n_repeats,
				design,
				simplify = FALSE
			))
			private$.compute_wvim(
				design = design,
				store_models = store_models,
				store_backends = store_backends,
				store_instance = store_instance
			)
		}
	),

	private = list(
		# Compute baseline scores for WVIM, which depend on the direction
		# "leave-out" -> baseline is the "full" model with all features in the task used
		# "leave-in" -> baseline is the "empty" model, so we swap in the featureless learner
		.compute_baseline = function(store_models = TRUE, store_backends = TRUE) {
			# Correct featureless learner depends on task type, there's no "surv.featureless" though
			learner = switch(
				self$direction,
				"leave-out" = self$learner,
				"leave-in" = lrn(paste(self$task$task_type, "featureless", sep = "."))
			)
			# Initial resampling
			self$resample_result = resample(
				self$task,
				learner,
				self$resampling,
				store_models = store_models,
				store_backends = store_backends
			)
			# Prepare baseline scores
			scores_baseline = self$resample_result$score(self$measure)[,
				.SD,
				.SDcols = c("iteration", self$measure$id)
			]
			setnames(scores_baseline, old = self$measure$id, "score_baseline")
			setnames(scores_baseline, old = "iteration", "iter_rsmp")
			scores_baseline[]
		},

		.compute_wvim = function(
			design,
			store_models = TRUE,
			store_backends = TRUE,
			store_instance = FALSE
		) {
			scores_baseline = private$.compute_baseline(store_backends = store_backends)

			# Fselect section
			instance = mlr3fselect::fselect(
				fselector = mlr3fselect::fs("design_points", design = design),
				task = self$task,
				learner = self$learner,
				resampling = self$resampling,
				measures = self$measure,
				store_models = store_models
			)

			archive_dt = as.data.table(instance$archive)
			archive_base = copy(archive_dt)
			# Match features in fselect instance to features left in or out, and
			# assign group names to "feature" var for consistency
			archive_base[, feature := private$.foi_chr(features)]
			archive_base = archive_base[, .(batch_nr, feature)]

			# Only keep instance if requested, otherwise would just increase memory footprint
			if (store_instance) {
				self$instance = instance
			}

			# Scores and predictions ---
			scores = instance$archive$benchmark_result$score(self$measure)
			setnames(scores, self$measure$id, "score_post")
			setnames(scores, "iteration", "iter_rsmp")
			setnames(scores, "nr", "batch_nr")

			# merge baseline scores and post-modification scores
			scores = scores[scores_baseline, on = "iter_rsmp"]
			# join with batch_nr to identify the foi for eatch iteration
			scores = archive_base[scores, on = "batch_nr"]
			# Regain n_repeats (hacky but kind of works I guess)
			scores[, iter_repeat := seq_along(batch_nr), by = c("iter_rsmp", "feature")]

			# Sanity check for iter refit
			#scores[, .(iter_rsmp, batch_nr, iter_repeat, feature)][feature == "important1"]
			private$.scores = scores[, .(feature, iter_rsmp, iter_repeat, score_baseline, score_post)]

			# Extract prediction for storage
			# self$predictions needs: iter_rsmp, iter_repeat/refit, feature, prediction
			predictions = copy(scores)[, .(iter_rsmp, iter_repeat, feature, prediction_test)]
			setnames(predictions, "prediction_test", "prediction")
			setkeyv(predictions, cols = c("feature", "iter_rsmp"))
			self$predictions = predictions[, .(iter_rsmp, iter_repeat, feature, prediction)]

			# obs losses ----
			if (has_obs_loss(self$measure)) {
				obs_loss_vals = instance$archive$benchmark_result$obs_loss(self$measure)
				setnames(obs_loss_vals, "resample_result", "batch_nr")
				# add iter_repeat to keep track
				obs_loss_vals[, iter_repeat := (batch_nr %% (self$param_set$values$n_repeats) + 1)]

				obs_loss_vals = archive_base[obs_loss_vals, on = "batch_nr"]

				setnames(obs_loss_vals, "iteration", "iter_rsmp")
				setnames(obs_loss_vals, self$measure$id, "loss_post")

				private$.obs_losses = obs_loss_vals[, .(
					feature,
					iter_rsmp,
					iter_repeat,
					row_ids,
					loss_post
				)]
			}
		},

		# This is hackier than I'd like but oh well, it gets the job done.
		# @param feature (list()) List-column of feature names in fselect instance
		# Goal is to create a column for the feature of interest OR group name
		# for the features of interest. Input is always list of features "left in",
		# so we need to flip (setdiff) for the other direction.
		# And for grouped features we do extra shenanigans to get the group names
		.foi_chr = function(features) {
			if (is.null(self$groups)) {
				switch(
					self$direction,
					"leave-in" = unlist(features),
					"leave-out" = vapply(
						features,
						\(x) {
							setdiff(self$task$feature_names, x)
						},
						character(1)
					)
				)
			} else {
				groups_tbl = private$.groups_tbl()
				vapply(
					features,
					\(x) {
						if (self$direction == "leave-out") {
							x = setdiff(self$task$feature_names, x)
						}
						feature_chr = paste0(x, collapse = ";")
						groups_tbl[feature_chr == groups_tbl$features, group]
					},
					character(1)
				)
			}
		}
	)
)

#' @title Leave-One-Covariate-Out (LOCO)
#'
#' @description
#' Calculates Leave-One-Covariate-Out (LOCO) scores.
#'
#' @details
#' LOCO measures feature importance by comparing model performance with and without
#' each feature. For each feature, the model is retrained without that feature and
#' the performance difference (reduced_model_loss - full_model_loss) indicates the
#' feature's importance. Higher values indicate more important features.
#'
#' @examplesIf requireNamespace("ranger", quietly = TRUE) && requireNamespace("mlr3learners", quietly = TRUE)
#' library(mlr3)
#' library(mlr3learners)
#'
#' task <- sim_dgp_correlated(n = 500)
#'
#' loco <- LOCO$new(
#'   task = task,
#'   learner = lrn("regr.ranger", num.trees = 10),
#'   measure = msr("regr.mse")
#' )
#' loco$compute()
#' loco$importance()
#' @export
#'
#' @references `r print_bib("lei_2018")`
LOCO = R6Class(
	"LOCO",
	inherit = WVIM,
	public = list(
		#' @description
		#' Creates a new instance of this [R6][R6::R6Class] class.
		#' @param task ([mlr3::Task]) Task to compute importance for.
		#' @param learner ([mlr3::Learner]) Learner to use for prediction.
		#' @param measure ([mlr3::Measure]: `NULL`) Measure to use for scoring. Defaults to
		#'   `classif.ce` for classification and `regr.mse` for regression.
		#' @param resampling ([mlr3::Resampling]) Resampling strategy. Defaults to holdout.
		#' @param features (`character()`) Features to compute importance for. Defaults to all features.
		#' @param n_repeats (`integer(1)`: `30L`) Number of refit iterations per resampling iteration.
		initialize = function(
			task,
			learner,
			measure = NULL,
			resampling = NULL,
			features = NULL,
			n_repeats = 30L
		) {
			if (!is.null(features)) {
				# LOCO specifically does not "allow" grouped features
				checkmate::assert_character(features)
				checkmate::assert_subset(features, choices = task$feature_names)
			}

			super$initialize(
				task = task,
				learner = learner,
				measure = measure,
				resampling = resampling,
				features = features,
				direction = "leave-out",
				label = "Leave-One-Covariate-Out (LOCO)",
				n_repeats = n_repeats
			)
		},

		#' @description
		#' Compute LOCO importances.
		#'
		#' @param store_models,store_backends (`logical(1)`: `TRUE`) Whether to store fitted models / data backends, passed to [mlr3::resample] internally
		compute = function(store_models = TRUE, store_backends = TRUE) {
			design = wvim_design_matrix(
				all_features = self$task$feature_names,
				feature_names = self$features,
				direction = self$direction
			)

			design = data.table::rbindlist(replicate(
				n = self$param_set$values$n_repeats,
				design,
				simplify = FALSE
			))

			private$.compute_wvim(design, store_models = store_models, store_backends = store_backends)
		}
	)
)

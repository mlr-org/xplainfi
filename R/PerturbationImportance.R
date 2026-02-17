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

		#' @description
		#' Creates a new instance of the PerturbationImportance class
		#' @param task,learner,measure,resampling,features,groups Passed to [FeatureImportanceMethod].
		#' @param sampler ([FeatureSampler]) Sampler to use for feature perturbation.
		#' @param relation (`character(1)`: `"difference"`) How to relate perturbed and baseline scores. Can also be `"ratio"`.
		#' @param n_repeats (`integer(1)`: `30L`) Number of permutation/conditional sampling iterations. Can also be overridden in `$compute()`.
		#' @param batch_size (`integer(1)` | `NULL`: `NULL`) Maximum number of rows to predict at once. When `NULL`, predicts all `test_size * n_repeats` rows in one call. Use smaller values to reduce memory usage at the cost of more prediction calls. Can be overridden in `$compute()`.
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
			batch_size = NULL
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

			# Knockoffs only generate one x_tilde, hence n_repeats > 1 is meaningless
			if (inherits(sampler, "KnockoffSampler") && n_repeats > sampler$param_set$values$iters) {
				cli::cli_inform(c(
					"Requested {.code n_repeats = {n_repeats}} permutations with {.cls {class(sampler)[[1]]}}",
					"!" = "A {.cls KnockoffSampler} was constructed with {.val {sampler$param_set$values$iters}} iterations",
					i = "Proceeding with {.code n_repeats = {sampler$param_set$values$iters}}",
					i = "Reconstruct {.cls {class(sampler)[[1]]}} with {.code iters >= {n_repeats}} or use {.cls ConditionalARFSampler} if repeated sampling is required."
				))
				n_repeats = sampler$param_set$values$iters
			}

			# Set up common parameters for all perturbation-based methods
			ps = paradox::ps(
				relation = paradox::p_fct(c("difference", "ratio"), default = "difference"),
				n_repeats = paradox::p_int(lower = 1, default = 1),
				batch_size = paradox::p_int(lower = 1, special_vals = list(NULL), default = NULL)
			)

			ps$values$relation = relation
			ps$values$n_repeats = n_repeats
			ps$values$batch_size = batch_size
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
		#' @param alternative (`character(1)`: `"greater"`) Type of alternative hypothesis for statistical tests.
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
			alternative = c("greater", "two.sided"),
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
		}
	),

	private = list(
		.compute_baseline = function(store_models = TRUE, store_backends = TRUE) {
			self$resample_result = assemble_rr(
				task = self$task,
				learner = self$learner,
				resampling = self$resampling,
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

			scores_baseline = private$.compute_baseline(store_backends = store_backends)

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
						}

						# Sample feature - sampler handles conditioning appropriately
						test_row_ids_replicated = rep.int(test_row_ids, times = n_repeats)
						perturbed_data = sampler$sample(foi, row_ids = test_row_ids_replicated)

						# Split perturbed data into repeats (n_repeats elements, each with test_size rows)
						test_size = length(test_row_ids)
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
						data.table::rbindlist(pred_per_perm, idcol = "iter_repeat")
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
				rbindlist(pred_per_feature, idcol = "feature")
			})
			# Append iteration id for resampling
			all_preds = rbindlist(all_preds, idcol = "iter_rsmp")
			# setkeyv(all_preds, cols = c("feature", "iter_rsmp"))

			# store predictions for future reference maybe?
			self$predictions = all_preds

			# Close progress bar
			# if (xplain_opt("progress")) {
			# 	cli::cli_progress_done(id = progress_bar_id)
			# }

			scores = data.table::copy(all_preds)[,
				score_post := vapply(
					prediction,
					\(p) p$score(measures = self$measure)[[self$measure$id]],
					FUN.VALUE = numeric(1)
				)
			]
			vars_to_keep = c("feature", "iter_rsmp", "iter_repeat", "score_baseline", "score_post")
			scores = scores[scores_baseline, on = c("iter_rsmp")]
			private$.scores = scores[, .SD, .SDcols = vars_to_keep]

			# for obs_loss:
			# Not all losses are decomposable so this is optional and depends on the provided measure
			if (has_obs_loss(self$measure)) {
				grouping_vars = c("feature", "iter_rsmp", "iter_repeat")

				obs_loss_all <- all_preds[,
					{
						pred <- prediction[[1]]

						# Get only vector of obs losses, Prediction$obs_loss() returns full table
						obs_loss_vals <- pred$obs_loss()[[self$measure$id]]

						list(
							row_ids = pred$row_ids,
							loss_post = obs_loss_vals
						)
					},
					by = grouping_vars
				]

				private$.obs_losses = obs_loss_all
			}
		}
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
#' @examplesIf requireNamespace("ranger", quietly = TRUE) && requireNamespace("mlr3learners", quietly = TRUE)
#' library(mlr3)
#' library(mlr3learners)
#'
#' task <- sim_dgp_correlated(n = 500)
#'
#' pfi <- PFI$new(
#'   task = task,
#'   learner = lrn("regr.ranger", num.trees = 10),
#'   measure = msr("regr.mse")
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
		#' @param task,learner,measure,resampling,features,groups,relation,n_repeats,batch_size Passed to [PerturbationImportance]
		initialize = function(
			task,
			learner,
			measure = NULL,
			resampling = NULL,
			features = NULL,
			groups = NULL,
			relation = "difference",
			n_repeats = 30L,
			batch_size = NULL
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
				batch_size = batch_size
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
#' Both require a decomposable measure (e.g., MSE) and holdout resampling
#' so each observation appears at most once in the test set.
#'
#' Available tests: `"t"` (t-test), `"wilcoxon"` (signed-rank), `"fisher"` (permutation),
#' `"binomial"` (sign test). The Fisher test is recommended.
#'
#' Method-agnostic inference methods (`"raw"`, `"nadeau_bengio"`, `"quantile"`) are also
#' available; see [FeatureImportanceMethod] for details.
#'
#' @references `r print_bib("watson_2021", "blesch_2025")`
#'
#' @examplesIf requireNamespace("ranger", quietly = TRUE) && requireNamespace("mlr3learners", quietly = TRUE) && requireNamespace("arf", quietly = TRUE)
#' library(mlr3)
#' library(mlr3learners)
#'
#' task <- sim_dgp_correlated(n = 500)
#'
#' # Using default ConditionalARFSampler
#' cfi <- CFI$new(
#'   task = task,
#'   learner = lrn("regr.ranger", num.trees = 10),
#'   measure = msr("regr.mse")
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
			sampler = NULL
		) {
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

			super$initialize(
				task = task,
				learner = learner,
				measure = measure,
				resampling = resampling,
				features = features,
				groups = groups,
				sampler = sampler,
				n_repeats = n_repeats,
				batch_size = batch_size
			)

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
#' @examplesIf requireNamespace("ranger", quietly = TRUE) && requireNamespace("mlr3learners", quietly = TRUE) && requireNamespace("arf", quietly = TRUE)
#' library(mlr3)
#' task = tgen("friedman1")$generate(n = 200)
#' rfi = RFI$new(
#'   task = task,
#'   learner = lrn("regr.ranger", num.trees = 50),
#'   measure = msr("regr.mse"),
#'   conditioning_set = c("important1")
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
			sampler = NULL
		) {
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
				batch_size = batch_size
			)

			# Validate and set up conditioning set after task is available
			if (!is.null(conditioning_set)) {
				conditioning_set = checkmate::assert_subset(conditioning_set, self$task$feature_names)
			} else {
				# Default to empty set (equivalent(ish) to PFI)
				cli::cli_warn(c(
					"Using empty conditioning set",
					i = "Set {.code conditioning_set} to condition on features."
				))
				conditioning_set = character(0)
			}

			# Configure the sampler with the conditioning_set
			self$sampler$param_set$values$conditioning_set = conditioning_set

			# Create extended param_set for RFI with conditioning_set parameter
			rfi_ps = paradox::ps(
				conditioning_set = paradox::p_uty(default = character(0))
			)
			rfi_ps$values$conditioning_set = conditioning_set
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

				# Clear cache and temporarily modify sampler's conditioning_set
				self$scores = NULL
				old_conditioning_set = self$sampler$param_set$values$conditioning_set
				self$sampler$param_set$values$conditioning_set = conditioning_set
				on.exit(self$sampler$param_set$values$conditioning_set <- old_conditioning_set)
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
	)
)

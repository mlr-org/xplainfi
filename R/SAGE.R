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
#' **Standard Error Calculation**: The standard errors (SE) reported in
#' `$convergence_history` reflect the uncertainty in Shapley value estimation
#' across different random permutations within a single resampling iteration.
#' These SEs quantify the Monte Carlo sampling error for a fixed trained model
#' and are only valid for inference about the importance of features for that
#' specific model. They do not capture broader uncertainty from model variability
#' across different train/test splits or resampling iterations.
#'
#' **Parallelization**: SAGE participates in xplainfi's parallel
#' backend layer (set a [future::plan()] or [mirai::daemons()]).
#' Permutations are distributed across workers. The permutation set is
#' reproducible (generated on the caller under the active seed); sampler
#' draws inside workers are statistically equivalent to sequential
#' execution but not bitwise identical, matching the contract of PFI/CFI.
#' Early stopping (`early_stopping = TRUE`) requires the convergence
#' iteration to run sequentially: with a single resampling iteration
#' this means no parallel speedup for that run. Choose `early_stopping`
#' (work economy at small permutation counts) or parallel execution
#' (wall-time economy at large counts); under early stopping, a larger
#' `check_interval` widens each sequential batch.
#'
#' @references
#' `r print_bib("lundberg_2020")`
#'
#' @seealso [MarginalSAGE] [ConditionalSAGE]
#'
#' @export
SAGE = R6Class(
	"SAGE",
	inherit = FeatureImportanceMethod,
	public = list(
		#' @field n_permutations (`integer(1)`) Number of permutations to sample.
		n_permutations = NULL,
		#' @field convergence_history ([`data.table`][data.table::data.table]) History of SAGE values during computation.
		convergence_history = NULL,
		#' @field converged (`logical(1)`) Whether convergence was detected.
		converged = FALSE,
		#' @field n_permutations_used (`integer(1)`) Actual number of permutations used.
		n_permutations_used = NULL,

		#' @description
		#' Creates a new instance of the SAGE class.
		#' @param task,learner,measure,resampling,features Passed to FeatureImportanceMethod.
		#' @param n_permutations (`integer(1)`: `10L`) Number of permutations to sample for SAGE value estimation.
		#'   The total number of evaluated coalitions is `1 (empty) + n_permutations * n_features`.
		#' @param batch_size (`integer(1)`: `5000L`) Maximum number of observations to process in a single prediction call.
		#' @param n_samples (`integer(1)`: `100L`) Number of samples to use for marginalizing out-of-coalition features.
		#'   For [MarginalSAGE], this is the number of marginal data samples ("background data" in other implementations).
		#'   For [ConditionalSAGE], this is the number of conditional samples per test instance retrieved from `sampler`.
		#' @param early_stopping (`logical(1)`: `TRUE`) Whether to enable early stopping based on convergence detection.
		#' @param se_threshold (`numeric(1)`: `0.01`) Convergence threshold for relative standard error.
		#'   Convergence is detected when the maximum relative SE across all features falls below this threshold.
		#'   Relative SE is calculated as SE divided by the range of importance values (max - min),
		#'   making it scale-invariant across different loss metrics.
		#'   Default of `0.01` means convergence when relative SE is below 1% of the importance range.
		#' @param min_permutations (`integer(1)`: `10L`) Minimum permutations before checking for convergence. Convergence is judged based on the standard errors of the estimated SAGE values,
		#' which requires a sufficiently large number of samples (i.e., evaluated coalitions).
		#' @param check_interval (`integer(1)`: `1L`) Check convergence every N permutations.
		initialize = function(
			task,
			learner,
			measure = NULL,
			resampling = NULL,
			features = NULL,
			n_permutations = 10L,
			batch_size = 5000L,
			n_samples = 100L,
			early_stopping = TRUE,
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

			self$n_permutations = checkmate::assert_int(n_permutations, lower = 1L)

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
				n_permutations = paradox::p_int(lower = 1L, default = 10L),
				batch_size = paradox::p_int(lower = 1L, default = 5000L),
				n_samples = paradox::p_int(lower = 1L, default = 100L),
				early_stopping = paradox::p_lgl(default = TRUE),
				se_threshold = paradox::p_dbl(lower = 0, upper = 1, default = 0.01),
				min_permutations = paradox::p_int(lower = 1L, default = 3L),
				check_interval = paradox::p_int(lower = 1L, default = 1L)
			)
			ps$values$n_permutations = n_permutations
			ps$values$batch_size = batch_size
			ps$values$n_samples = n_samples
			ps$values$early_stopping = early_stopping
			ps$values$se_threshold = se_threshold
			ps$values$min_permutations = min_permutations
			ps$values$check_interval = check_interval
			self$param_set = ps
		},

		#' @description
		#' Compute SAGE values.
		#' @param store_backends (`logical(1)`) Whether to store data backends.
		#' @param batch_size (`integer(1)`: `5000L`) Maximum number of observations to process in a single prediction call.
		#' @param early_stopping (`logical(1)`: `TRUE`) Whether to check for convergence and stop early.
		#' @param se_threshold (`numeric(1)`: `0.01`) Convergence threshold for relative standard error.
		#'   SE is normalized by the range of importance values (max - min) to make convergence
		#'   detection scale-invariant. Default `0.01` means convergence when relative SE < 1%.
		#' @param min_permutations (`integer(1)`: `10L`) Minimum permutations before checking convergence.
		#' @param check_interval (`integer(1)`: `1L`) Check convergence every N permutations.
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

			# Resolve parameters using hierarchical resolution
			batch_size = resolve_param(batch_size, self$param_set$values$batch_size, 5000L)
			early_stopping = resolve_param(
				early_stopping,
				self$param_set$values$early_stopping,
				TRUE
			)
			se_threshold = resolve_param(
				se_threshold,
				self$param_set$values$se_threshold,
				0.01
			)
			min_permutations = resolve_param(
				min_permutations,
				self$param_set$values$min_permutations,
				3L
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

			# Phase 2: early-stopping calibration (sequential, iter 1 only).
			# When early stopping is on, iter 1 must run the sequential
			# checkpoint/convergence loop to determine n_permutations_used.
			partials = list()
			iters_remaining = seq_len(self$resampling$iters)

			if (early_stopping) {
				first_result = private$.compute_sage_scores(
					learner = rr$learners[[1]],
					test_dt = self$task$data(rows = rr$resampling$test_set(1)),
					n_permutations = self$n_permutations,
					batch_size = batch_size,
					early_stopping = TRUE,
					se_threshold = se_threshold,
					min_permutations = min_permutations,
					check_interval = check_interval
				)
				self$convergence_history = first_result$convergence_data$convergence_history
				self$converged = first_result$convergence_data$converged
				self$n_permutations_used = first_result$convergence_data$n_permutations_used

				# Fold iter 1 in as a precomputed partial.
				sv = first_result$scores$importance * self$n_permutations_used
				names(sv) = first_result$scores$feature
				partials[[1]] = list(
					iter = 1L,
					sv = sv,
					sv_sq = sv, # unused downstream (SE not recomputed for SAGE scores)
					# sage_reduce_partials sums `n` via vapply(integer(1)); n_completed
					# is numeric, so coerce to keep the partial-list type contract.
					n = as.integer(self$n_permutations_used)
				)
				iters_remaining = iters_remaining[-1]
				perms_per_iter = self$n_permutations_used
			} else {
				self$convergence_history = NULL
				self$converged = FALSE
				self$n_permutations_used = self$n_permutations
				perms_per_iter = self$n_permutations
			}

			# Phase 3: flattened parallel bulk over {iter} x {perm-group}.
			if (length(iters_remaining) > 0L) {
				# target_units: tunable via xplain_opt("sage_target_units").
				# Default 64L; lower (e.g. 8L) for ConditionalSAGE on big
				# samplers where per-dispatch payload dominates.
				target_units = xplain_opt("sage_target_units")
				group_size = max(1L, perms_per_iter %/% target_units)

				# Pre-generate permutations caller-side (reproducible set)
				# and hoist per-iter baselines before dispatch.
				work = list()
				baselines = list()
				test_dts = list()
				for (it in iters_remaining) {
					test_dt_it = self$task$data(rows = rr$resampling$test_set(it))
					test_dts[[as.character(it)]] = test_dt_it
					baselines[[as.character(it)]] = private$.sage_baseline(
						rr$learners[[it]],
						test_dt_it,
						batch_size
					)
					perms = replicate(perms_per_iter, sample(self$features), simplify = FALSE)
					groups = split(perms, ceiling(seq_along(perms) / group_size))
					for (g in groups) {
						work[[length(work) + 1L]] = list(iter = it, perms = g)
					}
				}

				if (xplain_opt("verbose") && !xplain_opt("progress")) {
					cli::cli_inform(paste0(
						"Computing SAGE: {length(iters_remaining)} iteration{?s} x ",
						"{perms_per_iter} permutations across {length(work)} work unit{?s}"
					))
				}

				this_learners = rr$learners
				sampler = if (inherits(self, "ConditionalSAGE")) self$sampler else NULL
				learner_packages = self$learner$packages

				# compute_chunk_partial needs features/task/measure/sampler
				# but never resample_result; ship a shallow clone with the
				# heavy fold-learner blob dropped so each parallel worker
				# payload stays small (learners are hoisted separately).
				self_for_workers = self$clone(deep = FALSE)
				self_for_workers$resample_result = NULL

				bulk = xplainfi_map(
					length(work),
					private$.sage_chunk_worker,
					work,
					.args = list(
						self_obj = self_for_workers,
						learners = this_learners,
						test_dts = test_dts,
						baselines = baselines,
						batch_size = batch_size,
						sampler = sampler,
						learner_packages = learner_packages,
						arf_workers = xplain_opt("arf_workers")
					)
				)

				partials = c(partials, bulk)
			}

			private$.scores = sage_reduce_partials(partials, self$features)
		},

		#' @description
		#' Plot convergence history of SAGE values.
		#' @param features (`character` | `NULL`) Features to plot. If NULL, plots all features.
		#' @return A [ggplot2][ggplot2::ggplot] object
		plot_convergence = function(features = NULL) {
			require_package("ggplot2")

			if (is.null(self$convergence_history)) {
				cli::cli_abort("No convergence history available. Run $compute() first.")
			}

			# Create a copy to avoid modifying the original
			plot_data = copy(self$convergence_history)

			if (!is.null(features)) {
				plot_data = plot_data[feature %in% features]
			}

			p = ggplot2::ggplot(
				plot_data,
				ggplot2::aes(x = n_permutations, y = importance, fill = feature, color = feature)
			) +
				ggplot2::geom_ribbon(
					ggplot2::aes(ymin = importance - se, ymax = importance + se),
					alpha = 1 / 3
				) +
				ggplot2::geom_line(linewidth = 1) +
				ggplot2::geom_point(size = 2) +
				ggplot2::labs(
					title = "SAGE Value Convergence",
					subtitle = if (self$converged) {
						sprintf(
							"Converged after %d permutations (saved %d)",
							self$n_permutations_used,
							self$n_permutations - self$n_permutations_used
						)
					} else {
						sprintf("Completed all %d permutations", self$n_permutations)
					},
					x = "Number of Permutations",
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
		},

		#' @description
		#' Compute partial SAGE sums for one permutation chunk. Internal
		#' worker entry point for parallel execution; not intended for
		#' direct use. Pure given `(learner, test_dt, perm_sublist,
		#' baseline)`, additive across chunks.
		#' @param learner Trained [mlr3::Learner] for this resampling iteration.
		#' @param test_dt ([data.table::data.table]) Test data for this iteration.
		#' @param perm_sublist (`list`) Feature-name permutations to evaluate.
		#' @param baseline (`numeric(1)`) Shared empty-coalition loss.
		#' @param batch_size (`integer(1)` | `NULL`) Prediction batch size.
		#' @return `list(sv, sv_sq, n)`: named numeric vectors over features
		#'   and the number of permutations processed.
		#' @keywords internal
		compute_chunk_partial = function(learner, test_dt, perm_sublist, baseline, batch_size = NULL) {
			coalitions = sage_growing_coalitions(perm_sublist)
			losses = private$.evaluate_coalitions_batch(
				learner,
				test_dt,
				coalitions,
				batch_size
			)
			acc = sage_marginal_contributions(
				perm_sublist,
				losses,
				baseline,
				self$features
			)
			list(sv = acc$sv, sv_sq = acc$sv_sq, n = length(perm_sublist))
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

				checkpoint_losses = private$.evaluate_coalitions_batch(
					learner,
					test_dt,
					checkpoint_coalitions,
					batch_size
				)

				if (xplain_opt("progress")) {
					cli::cli_progress_update(inc = 1)
				}

				# Empty-coalition loss (first slot) captured once in the
				# first checkpoint, then reused as the running baseline.
				if (n_completed == 0) {
					baseline_loss = checkpoint_losses[1]
				}

				# Shared closed-form accumulation: same helper as
				# compute_chunk_partial. `offset` skips the leading
				# empty-coalition slot in the first checkpoint. Replaces the
				# former O(n^2) which(sapply()) coalition-map lookup.
				acc = sage_marginal_contributions(
					checkpoint_permutations,
					checkpoint_losses,
					baseline_loss,
					self$features,
					offset = offset
				)
				# Name-aligned add (defensive: positional add only valid if
				# orders match; reindex by name to be safe).
				sage_values = sage_values + acc$sv[names(sage_values)]
				sage_values_sq = sage_values_sq + acc$sv_sq[names(sage_values_sq)]

				# Update the count of completed permutations.
				n_completed = n_completed + checkpoint_size

				# Calculate the current average SAGE values and standard errors based on completed permutations.
				current_avg = sage_values / n_completed

				# Calculate running variance and standard errors for each feature
				# Variance = E[X^2] - E[X]^2, SE = sqrt(Var / n)
				current_variance = (sage_values_sq / n_completed) - (current_avg^2)
				# Ensure variance is non-negative (numerical precision issues)
				current_variance[current_variance < 0] = 0
				current_se = sqrt(current_variance / n_completed)

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

				# Check for convergence if early stopping is enabled and enough permutations have been processed.
				if (early_stopping && n_completed >= min_permutations && length(convergence_history) > 1) {
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

			# Return the computed scores and convergence data.
			list(
				scores = data.table(
					feature = names(final_sage_values),
					importance = as.numeric(final_sage_values)
				),
				baseline = baseline_loss,
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

		# Empty-coalition (no-feature) loss. Hoisted out of the
		# checkpoint loop so every permutation chunk shares one
		# baseline anchor across parallel workers.
		.sage_baseline = function(learner, test_dt, batch_size = NULL) {
			private$.evaluate_coalitions_batch(
				learner,
				test_dt,
				list(character(0)),
				batch_size
			)[1]
		},

		# Worker entry for the flattened {iter} x {perm-group} map in
		# compute(). Passed as the .f to xplainfi_map; receives one
		# work unit plus the hoisted .args. Kept on the SAGE object so
		# the heavy inline closure no longer clutters compute() and is
		# unit-test-reachable via private$.sage_chunk_worker.
		.sage_chunk_worker = function(
			unit,
			self_obj,
			learners,
			test_dts,
			baselines,
			batch_size,
			sampler,
			learner_packages,
			arf_workers,
			is_sequential = TRUE
		) {
			xplain_worker_preamble(learner_packages, sampler, arf_workers, is_sequential)
			it = unit$iter
			res = self_obj$compute_chunk_partial(
				learners[[it]],
				test_dts[[as.character(it)]],
				unit$perms,
				baselines[[as.character(it)]],
				batch_size
			)
			list(iter = it, sv = res$sv, sv_sq = res$sv_sq, n = res$n)
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
		# Calculates losses from averaged predictions for each coalition
		.calculate_coalition_losses = function(avg_preds, n_test, test_dt) {
			n_coalitions = length(unique(avg_preds$.coalition_id))
			coalition_losses = numeric(n_coalitions)

			for (i in seq_len(n_coalitions)) {
				coalition_data = avg_preds[.coalition_id == i]

				pred_obj = if (self$task$task_type == "classif") {
					PredictionClassif$new(
						row_ids = seq_len(n_test),
						truth = test_dt[[self$task$target_names]],
						prob = as.matrix(coalition_data[, .SD, .SDcols = self$task$class_names])
					)
				} else {
					PredictionRegr$new(
						row_ids = seq_len(n_test),
						truth = test_dt[[self$task$target_names]],
						response = coalition_data$avg_pred
					)
				}

				coalition_losses[i] = pred_obj$score(self$measure)
			}

			coalition_losses
		}
	)
)

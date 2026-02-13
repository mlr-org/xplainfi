# Higher-level test functions for FeatureImportanceMethod subclasses
#
# These functions encapsulate common test patterns to reduce duplication
# and make writing new tests easier. Works for PFI, CFI, RFI, WVIM, LOCO, etc.

# -----------------------------------------------------------------------------
# test_default_behavior
# -----------------------------------------------------------------------------

#' Test that a FeatureImportanceMethod works with minimal parameters
#'
#' Verifies that:
#' - Constructor fails without any arguments (basic validation)
#' - Method can be constructed with just task and learner
#' - Default measure is automatically selected based on task type
#' - Default resampling is holdout
#' - compute() and importance() work correctly
#'
#' @param method_class R6 class (PFI, CFI, RFI, WVIM, LOCO, etc.)
#' @param task_type "regr" or "classif"
#' @param ... Additional arguments passed to method constructor (e.g., conditioning_set for RFI)
test_default_behavior = function(method_class, task_type = "regr", ...) {
	# Constructor must fail without any arguments
	expect_error(method_class$new())

	if (task_type == "regr") {
		task = tgen("friedman1")$generate(n = 100)
		learner = lrn("regr.rpart")
		expected_measure_class = "MeasureRegr"
	} else {
		task = tgen("2dnormals")$generate(n = 100)
		learner = lrn("classif.rpart", predict_type = "prob")
		expected_measure_class = "MeasureClassif"
	}

	# Construct with minimal parameters
	method = method_class$new(
		task = task,
		learner = learner,
		...
	)

	# Check default measure was selected
	checkmate::expect_r6(method$measure, expected_measure_class)

	# Check default resampling is holdout
	expect_equal(method$resampling$id, "holdout")

	# Compute and validate
	method$compute()
	expect_importance_dt(method$importance(), features = method$features)

	invisible(method)
}

# -----------------------------------------------------------------------------
# test_featureless_zero_importance
# -----------------------------------------------------------------------------

#' Test that featureless learner produces zero importance for all features
#'
#' @param method_class R6 class (PFI, CFI, or RFI)
#' @param task_type "regr" or "classif"
#' @param ... Additional arguments passed to method constructor
test_featureless_zero_importance = function(method_class, task_type = "classif", ...) {
	if (task_type == "regr") {
		task = tgen("friedman1")$generate(n = 200)
		learner = lrn("regr.featureless")
		measure = msr("regr.mse")
	} else {
		task = tgen("xor")$generate(n = 200)
		learner = lrn("classif.featureless")
		measure = msr("classif.ce")
	}

	method = method_class$new(
		task = task,
		learner = learner,
		measure = measure,
		...
	)

	method$compute()

	result = method$importance()

	# Verify structure
	expect_importance_dt(result, features = method$features)

	# All importance values should be essentially zero
	checkmate::expect_numeric(result$importance, lower = -1e-10, upper = 1e-10)

	invisible(method)
}

# -----------------------------------------------------------------------------
# test_n_repeats_and_scores
# -----------------------------------------------------------------------------

#' Test multiple repeats and validate scores structure
#'
#' @param method_class R6 class (PFI, CFI, or RFI)
#' @param task mlr3 Task
#' @param learner mlr3 Learner
#' @param measure mlr3 Measure
#' @param n_repeats Number of repeats
#' @param resampling mlr3 Resampling (default: CV with 3 folds)
#' @param ... Additional arguments passed to method constructor
test_n_repeats_and_scores = function(
	method_class,
	task,
	learner,
	measure,
	n_repeats = 2L,
	resampling = rsmp("cv", folds = 3),
	...
) {
	method = method_class$new(
		task = task,
		learner = learner,
		measure = measure,
		resampling = resampling,
		n_repeats = n_repeats,
		...
	)

	method$compute()

	# Validate importance
	expect_importance_dt(method$importance(), features = method$features)

	# Validate scores structure
	expected_nrows = resampling$iters * n_repeats * length(method$features)

	checkmate::expect_data_table(
		method$scores(),
		types = c("character", "integer", "numeric"),
		nrows = expected_nrows,
		ncols = 6,
		any.missing = FALSE,
		min.cols = 6
	)

	invisible(method)
}

# -----------------------------------------------------------------------------
# test_single_feature
# -----------------------------------------------------------------------------
#' Test computing importance for a single feature
#'
#' @param method_class R6 class (PFI, CFI, or RFI)
#' @param task mlr3 Task (should have feature named in `feature` param)
#' @param learner mlr3 Learner
#' @param measure mlr3 Measure
#' @param feature Single feature name to compute importance for
#' @param n_repeats Number of repeats
#' @param resampling mlr3 Resampling
#' @param ... Additional arguments passed to method constructor
test_single_feature = function(
	method_class,
	task,
	learner,
	measure,
	feature = "important4",
	n_repeats = 2L,
	resampling = rsmp("cv", folds = 3),
	...
) {
	method = method_class$new(
		task = task,
		learner = learner,
		measure = measure,
		resampling = resampling,
		n_repeats = n_repeats,
		features = feature,
		...
	)

	method$compute()

	# Validate importance for single feature
	expect_importance_dt(method$importance(), features = feature)

	# Validate scores structure (only 1 feature)
	expected_nrows = resampling$iters * n_repeats

	checkmate::expect_data_table(
		method$scores(),
		types = c("character", "integer", "numeric"),
		nrows = expected_nrows,
		ncols = 6,
		any.missing = FALSE,
		min.cols = 6
	)

	invisible(method)
}

# -----------------------------------------------------------------------------
# test_friedman1_sensible_ranking
# -----------------------------------------------------------------------------

#' Test that friedman1 DGP produces sensible feature ranking
#'
#' Verifies that important features (important1-5) have higher mean importance
#' than unimportant features (unimportant1-5).
#'
#' @param method_class R6 class (PFI, CFI, RFI, MarginalSAGE, ConditionalSAGE, etc.)
#' @param learner mlr3 Learner for regression
#' @param measure mlr3 Measure for regression
#' @param n Sample size for friedman1 task (default 500 for stable ranking)
#' @param ... Additional arguments passed to method constructor (e.g., n_repeats for perturbation methods)
test_friedman1_sensible_ranking = function(
	method_class,
	learner = lrn("regr.rpart"),
	measure = msr("regr.mse"),
	n = 500L,
	...
) {
	task = tgen("friedman1")$generate(n = n)

	method = method_class$new(
		task = task,
		learner = learner,
		measure = measure,
		...
	)

	method$compute()
	result = method$importance()

	expect_importance_dt(result, features = method$features)

	# Extract scores by feature type
	important_features = grep("^important", result$feature, value = TRUE)
	unimportant_features = grep("^unimportant", result$feature, value = TRUE)

	important_scores = result[feature %in% important_features]$importance
	unimportant_scores = result[feature %in% unimportant_features]$importance

	# Important features should have higher mean importance
	expect_gt(mean(important_scores), mean(unimportant_scores))

	# Scores should be finite and not all zero
	checkmate::expect_numeric(result$importance, finite = TRUE)
	expect_gt(max(abs(result$importance)), 0)

	invisible(method)
}

# -----------------------------------------------------------------------------
# test_relation_parameter
# -----------------------------------------------------------------------------

#' Test difference vs ratio relation parameter
#'
#' @param method_class R6 class (PFI, CFI, or RFI)
#' @param task mlr3 Task
#' @param learner mlr3 Learner
#' @param measure mlr3 Measure
#' @param ... Additional arguments passed to method constructor
test_relation_parameter = function(
	method_class,
	task,
	learner,
	measure,
	...
) {
	method = method_class$new(
		task = task,
		learner = learner,
		measure = measure,
		...
	)

	method$compute()

	res_diff = method$importance(relation = "difference")
	res_ratio = method$importance(relation = "ratio")
	res_default = method$importance()

	# All should be valid
	expect_importance_dt(res_diff, method$features)
	expect_importance_dt(res_ratio, method$features)

	# Default should be "difference"
	expect_identical(res_default, res_diff)

	# Different relations should give different results
	expect_false(isTRUE(all.equal(res_diff, res_ratio)))

	invisible(method)
}

# -----------------------------------------------------------------------------
# test_parameter_validation
# -----------------------------------------------------------------------------

#' Test that invalid parameters are rejected
#'
#' @param method_class R6 class (PFI, CFI, or RFI)
#' @param task mlr3 Task
#' @param learner mlr3 Learner
#' @param measure mlr3 Measure
#' @param ... Additional arguments passed to method constructor
test_parameter_validation = function(
	method_class,
	task,
	learner,
	measure,
	...
) {
	# n_repeats = 0 should fail
	expect_error(
		method_class$new(
			task = task,
			learner = learner,
			measure = measure,
			n_repeats = 0L,
			...
		)
	)

	# n_repeats = -1 should fail
	expect_error(
		method_class$new(
			task = task,
			learner = learner,
			measure = measure,
			n_repeats = -1L,
			...
		)
	)
}

# -----------------------------------------------------------------------------
# test_grouped_importance
# -----------------------------------------------------------------------------

#' Test grouped feature importance
#'
#' @param method_class R6 class (PFI, CFI, or RFI)
#' @param task mlr3 Task (should have features matching group definitions)
#' @param learner mlr3 Learner
#' @param measure mlr3 Measure
#' @param groups Named list of feature groups
#' @param expected_classes Character vector of expected R6 class names
#' @param ... Additional arguments passed to method constructor
test_grouped_importance = function(
	method_class,
	task,
	learner,
	measure,
	groups,
	expected_classes = NULL,
	...
) {
	method = method_class$new(
		task = task,
		learner = learner,
		measure = measure,
		groups = groups,
		...
	)

	# Check class hierarchy if specified
	if (!is.null(expected_classes)) {
		checkmate::expect_r6(method, expected_classes)
	}

	# Groups should be stored
	expect_false(is.null(method$groups))
	expect_equal(names(method$groups), names(groups))

	method$compute()
	result = method$importance()

	# Should have one row per group
	expect_equal(nrow(result), length(groups))
	expect_equal(result$feature, names(groups))
	expect_importance_dt(result, features = names(groups))

	invisible(method)
}

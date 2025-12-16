# Custom testthat expectations for FeatureSampler testing
#
# These follow testthat 3e conventions.
# See: https://testthat.r-lib.org/articles/custom-expectation.html

# -----------------------------------------------------------------------------
# Helper: Generate task from sampler's supported feature types
# -----------------------------------------------------------------------------

#' Generate a test task based on sampler's supported feature types
#'
#' @param supported_types Character vector of supported feature types
#' @param n Number of observations
#' @return An mlr3 regression task
generate_test_task = function(supported_types, n = 100) {
	xdat = data.table::data.table(
		x_num1 = rnorm(n),
		x_num2 = runif(n)
	)

	if ("integer" %in% supported_types) {
		xdat[, x_int := sample(1L:10L, n, replace = TRUE)]
	}
	if ("factor" %in% supported_types) {
		xdat[, x_fct := factor(sample(c("a", "b", "c"), n, replace = TRUE))]
	}
	if ("ordered" %in% supported_types) {
		xdat[,
			x_ord := ordered(
				sample(c("low", "mid", "high"), n, replace = TRUE),
				levels = c("low", "mid", "high")
			)
		]
	}
	if ("logical" %in% supported_types) {
		xdat[, x_lgl := sample(c(TRUE, FALSE), n, replace = TRUE)]
	}

	xdat[, y := x_num1 + 0.5 * x_num2 + rnorm(n, sd = 0.1)]
	mlr3::as_task_regr(xdat, target = "y")
}

# -----------------------------------------------------------------------------
# expect_feature_type_consistency
# -----------------------------------------------------------------------------

#' Expect sampled output has consistent feature types with task
#'
#' Compares feature classes in sampled data against task$feature_types.
#'
#' @param sampled A data.table returned from sampler$sample()
#' @param task The mlr3 task used for sampling
#' @return Invisibly returns sampled data for piping
expect_feature_type_consistency = function(sampled, task) {
	expected_classes = stats::setNames(
		task$feature_types$type,
		task$feature_types$id
	)
	actual_classes = sapply(
		sampled[, task$feature_names, with = FALSE],
		function(x) class(x)[1]
	)

	mismatches = expected_classes != actual_classes
	if (any(mismatches)) {
		bad_feats = names(expected_classes)[mismatches]
		msg = glue::glue(
			"Feature type mismatch in sampled data.\n",
			"Features with wrong type: {paste(bad_feats, collapse = ', ')}\n",
			"Expected: {paste(expected_classes[mismatches], collapse = ', ')}\n",
			"Actual: {paste(actual_classes[mismatches], collapse = ', ')}"
		)
	} else {
		msg = ""
	}

	testthat::expect(!any(mismatches), msg)
	invisible(sampled)
}

# -----------------------------------------------------------------------------
# expect_non_sampled_unchanged
# -----------------------------------------------------------------------------

#' Expect non-sampled features are unchanged after sampling
#'
#' Verifies that features not passed to $sample() remain identical
#' between the original data and the sampled output.
#'
#' @param sampled A data.table returned from sampler$sample()
#' @param original The original data before sampling
#' @param features Character vector of feature names that should be unchanged
#' @return Invisibly returns sampled data for piping
expect_non_sampled_unchanged = function(sampled, original, features) {
	if (length(features) == 0) {
		testthat::expect(TRUE, "No features to check")
		return(invisible(sampled))
	}

	for (feat in features) {
		ok = identical(sampled[[feat]], original[[feat]])
		testthat::expect(
			ok,
			glue::glue(
				"Feature '{feat}' was modified in sampled data.\n",
				"Non-sampled features must remain unchanged during sampling."
			)
		)
	}

	invisible(sampled)
}

# -----------------------------------------------------------------------------
# expect_sampled_features_changed
# -----------------------------------------------------------------------------

#' Expect sampled features differ from original (stochastic check)
#'
#' Verifies that at least some values in sampled features differ from original.
#' This is a probabilistic check - with sufficient data variability, randomly
#' sampling identical values is extremely unlikely.
#'
#' @param sampled A data.table returned from sampler$sample()
#' @param original The original data before sampling
#' @param sampled_features Character vector of feature names that were sampled
#' @return Invisibly returns sampled data for piping
expect_sampled_features_changed = function(sampled, original, sampled_features) {
	for (feat in sampled_features) {
		ok = !identical(sampled[[feat]], original[[feat]])
		testthat::expect(
			ok,
			glue::glue(
				"Sampled feature '{feat}' is identical to original.\n",
				"This suggests the sampler did not modify the feature.\n",
				"(Note: This could theoretically be a false positive with very low probability)"
			)
		)
	}

	invisible(sampled)
}

# -----------------------------------------------------------------------------
# expect_sampler_output_structure
# -----------------------------------------------------------------------------

#' Expect sampler output has correct structure
#'
#' Verifies the sampled data is a data.table with correct columns and dimensions.
#'
#' @param sampled A data.table returned from sampler$sample()
#' @param task The mlr3 task used for sampling
#' @param nrows Expected number of rows (NULL to skip check)
#' @return Invisibly returns sampled data for piping
expect_sampler_output_structure = function(sampled, task, nrows = NULL) {
	testthat::expect(
		data.table::is.data.table(sampled),
		glue::glue(
			"Sampled data is not a data.table.\n",
			"Actual class: {paste(class(sampled), collapse = ', ')}"
		)
	)

	expected_cols = c(task$target_names, task$feature_names)
	actual_cols = names(sampled)

	testthat::expect(
		identical(actual_cols, expected_cols),
		glue::glue(
			"Sampled data has incorrect columns.\n",
			"Expected: {paste(expected_cols, collapse = ', ')}\n",
			"Actual: {paste(actual_cols, collapse = ', ')}"
		)
	)

	if (!is.null(nrows)) {
		testthat::expect(
			nrow(sampled) == nrows,
			glue::glue(
				"Sampled data has incorrect number of rows.\n",
				"Expected: {nrows}\n",
				"Actual: {nrow(sampled)}"
			)
		)
	}

	invisible(sampled)
}

# -----------------------------------------------------------------------------
# expect_marginal_sampling
# -----------------------------------------------------------------------------

#' Expect conditional sampler handles marginal sampling correctly
#'
#' Tests that a conditional sampler works correctly with an empty conditioning
#' set (character(0)), which should trigger marginal sampling behavior.
#'
#' @param sampler A ConditionalSampler instance
#' @param feature Feature to sample
#' @param row_ids Row IDs to sample
#' @return Invisibly returns sampled data for piping
expect_marginal_sampling = function(sampler, feature, row_ids = 1:10) {
	testthat::expect(
		inherits(sampler, "ConditionalSampler"),
		"Sampler is not a ConditionalSampler. Marginal sampling test only applies to conditional samplers."
	)

	original = sampler$task$data(rows = row_ids)

	# Sample with empty conditioning set (marginal sampling)
	sampled = sampler$sample(
		feature = feature,
		row_ids = row_ids,
		conditioning_set = character(0)
	)

	# Check structure
	expect_sampler_output_structure(sampled, sampler$task, nrows = length(row_ids))

	# Check types
	expect_feature_type_consistency(sampled, sampler$task)

	# Non-sampled features should remain unchanged. The key test here is that
	# conditional samplers can handle an empty conditioning set without error.
	non_sampled = setdiff(sampler$task$feature_names, feature)
	expect_non_sampled_unchanged(sampled, original, non_sampled)

	invisible(sampled)
}

# -----------------------------------------------------------------------------
# expect_conditional_sampling
# -----------------------------------------------------------------------------

#' Expect conditional sampler handles conditional sampling correctly
#'
#' Tests that a conditional sampler correctly preserves conditioning features
#' and modifies sampled features.
#'
#' @param sampler A ConditionalSampler instance
#' @param feature Feature(s) to sample
#' @param conditioning_set Features to condition on
#' @param row_ids Row IDs to sample
#' @return Invisibly returns sampled data for piping
expect_conditional_sampling = function(sampler, feature, conditioning_set, row_ids = 1:10) {
	testthat::expect(
		inherits(sampler, "ConditionalSampler"),
		"Sampler is not a ConditionalSampler. Conditional sampling test only applies to conditional samplers."
	)

	original = sampler$task$data(rows = row_ids)

	sampled = sampler$sample(
		feature = feature,
		row_ids = row_ids,
		conditioning_set = conditioning_set
	)

	# Check structure
	expect_sampler_output_structure(sampled, sampler$task, nrows = length(row_ids))

	# Check types
	expect_feature_type_consistency(sampled, sampler$task)

	# Non-sampled features must be unchanged
	expect_non_sampled_unchanged(sampled, original, conditioning_set)

	# Sampled features should change (stochastic check)
	expect_sampled_features_changed(sampled, original, feature)

	invisible(sampled)
}

# -----------------------------------------------------------------------------
# Omnibus test functions (combine multiple expectations)
# -----------------------------------------------------------------------------

#' Run comprehensive feature type tests for a sampler class
#'
#' Generates a task based on the sampler's supported feature types and tests
#' that all sampling operations preserve correct types.
#'
#' @param sampler_class R6 class for the sampler to test
#' @param ... Additional arguments passed to sampler constructor
#' @return NULL (used for side effects via testthat expectations)
test_sampler_feature_types = function(sampler_class, ...) {
	supported_types = sampler_class$public_fields$feature_types
	task = generate_test_task(supported_types)

	sampler = sampler_class$new(task, ...)
	is_conditional = inherits(sampler, "ConditionalSampler")

	# Test sampling each feature

	for (feat in task$feature_names) {
		sampled = sampler$sample(feat, row_ids = 1:10)
		expect_sampler_output_structure(sampled, task, nrows = 10)
		expect_feature_type_consistency(sampled, task)

		# For conditional samplers, also test with explicit conditioning set
		if (is_conditional) {
			other_feats = setdiff(task$feature_names, feat)
			if (length(other_feats) >= 1) {
				expect_conditional_sampling(
					sampler,
					feature = feat,
					conditioning_set = other_feats[1],
					row_ids = 1:10
				)
			}
			# Also test marginal case
			expect_marginal_sampling(sampler, feature = feat, row_ids = 1:10)
		}
	}

	invisible(NULL)
}

#' Test conditioning_set parameter behavior for conditional samplers
#'
#' Verifies that a conditional sampler correctly:
#' 1. Stores conditioning_set in param_set when provided during initialization
#' 2. Can sample without specifying conditioning_set (uses stored value)
#' 3. Can override conditioning_set in $sample() calls
#' 4. Handles NULL conditioning_set (defaults to all other features)
#' 5. Handles empty conditioning_set character(0) (marginal sampling)
#'
#' @param sampler_class R6 class for the sampler to test
#' @param task mlr3 Task to use for testing (must have at least 3 features)
#' @param ... Additional arguments passed to sampler constructor
#' @return NULL (used for side effects via testthat expectations)
test_conditioning_set_behavior = function(sampler_class, task, ...) {
	features = task$feature_names
	checkmate::assert_true(
		length(features) >= 3,
		.var.name = "task must have at least 3 features"
	)

	target_feature = features[1]
	cond_set_1 = features[2]
	cond_set_2 = features[3]
	other_features = setdiff(features, target_feature)

	# Test 1: conditioning_set stored in param_set when provided
	sampler_with_cond = sampler_class$new(task, conditioning_set = cond_set_1, ...)
	testthat::expect_identical(
		sampler_with_cond$param_set$values$conditioning_set,
		cond_set_1
	)

	# Test 2: Can sample using stored conditioning_set
	original_data = task$data(rows = 1:5)
	result_stored = sampler_with_cond$sample(feature = target_feature, row_ids = 1:5)
	expect_sampler_output_structure(result_stored, task, nrows = 5)
	expect_non_sampled_unchanged(result_stored, original_data, cond_set_1)

	# Test 3: Can override conditioning_set in $sample() call
	result_override = sampler_with_cond$sample(
		feature = target_feature,
		row_ids = 1:5,
		conditioning_set = cond_set_2
	)
	expect_sampler_output_structure(result_override, task, nrows = 5)
	expect_non_sampled_unchanged(result_override, original_data, cond_set_2)

	# Test 4: NULL conditioning_set during initialization
	sampler_no_cond = sampler_class$new(task, ...)
	testthat::expect_null(sampler_no_cond$param_set$values$conditioning_set)

	# Test 5: Can specify conditioning_set in $sample() when not set during init
	result_specified = sampler_no_cond$sample(
		feature = target_feature,
		row_ids = 1:5,
		conditioning_set = cond_set_1
	)
	expect_non_sampled_unchanged(result_specified, original_data, cond_set_1)

	# Test 6: NULL conditioning_set should default to all other features
	# Use debug mode to verify resolved conditioning_set
	messages_null = utils::capture.output(
		withr::with_options(
			list(xplain.debug = TRUE),
			sampler_no_cond$sample(
				feature = target_feature,
				row_ids = 1:5,
				conditioning_set = NULL
			)
		),
		type = "message"
	)

	resolved_null = paste(messages_null, collapse = " ")
	testthat::expect_true(
		grepl("Resolved conditioning_set:", resolved_null, fixed = TRUE)
	)

	# All other features should be in conditioning set when NULL
	for (feat in other_features) {
		testthat::expect_true(grepl(feat, resolved_null, fixed = TRUE))
	}

	# Test 7: character(0) should result in empty conditioning set (marginal)
	messages_empty = utils::capture.output(
		withr::with_options(
			list(xplain.debug = TRUE),
			sampler_no_cond$sample(
				feature = target_feature,
				row_ids = 1:5,
				conditioning_set = character(0)
			)
		),
		type = "message"
	)

	resolved_empty = paste(messages_empty, collapse = " ")
	testthat::expect_true(
		grepl("Resolved conditioning_set:", resolved_empty, fixed = TRUE)
	)

	# No features should appear in conditioning set with character(0)
	for (feat in other_features) {
		testthat::expect_false(grepl(feat, resolved_empty, fixed = TRUE))
	}

	invisible(NULL)
}

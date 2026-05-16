test_that(".compute_score guards zero baseline in ratio relation", {
	# regr.mse -> minimize = TRUE -> ratio = scores_post / scores_pre,
	# so scores_pre == 0 is the undefined (Inf / NaN) case.
	task = sim_dgp_independent(n = 60)
	pfi = PFI$new(
		task = task,
		learner = lrn("regr.rpart"),
		measure = msr("regr.mse")
	)
	cs = pfi$.__enclos_env__$private$.compute_score

	expect_warning(
		res <- cs(
			scores_pre = c(0, 1, 0),
			scores_post = c(1, 2, 0),
			relation = "ratio"
		),
		regexp = "zero baseline"
	)
	# 1/0 -> NA, 2/1 -> 2, 0/0 -> NA
	checkmate::expect_numeric(res, len = 3L)
	expect_equal(res, c(NA_real_, 2, NA_real_))

	# difference relation must be unaffected: no NA, no warning
	expect_no_warning(
		diff_res <- cs(
			scores_pre = c(0, 1, 0),
			scores_post = c(1, 2, 0),
			relation = "difference"
		)
	)
	expect_equal(diff_res, c(1, 1, 0))
})

test_that(".compute_score guards zero baseline for minimize = FALSE (classif.acc)", {
	# classif.acc -> minimize = FALSE -> ratio = scores_pre / scores_post,
	# so scores_post == 0 is the undefined case.
	set.seed(531)
	task = tgen("2dnormals")$generate(n = 60)
	pfi = PFI$new(
		task = task,
		learner = lrn("classif.rpart", predict_type = "prob"),
		measure = msr("classif.acc")
	)
	cs = pfi$.__enclos_env__$private$.compute_score

	# minimize = FALSE: ratio = scores_pre / scores_post; scores_post == 0 is undefined
	# scores_pre = c(0, 1, 0), scores_post = c(1, 2, 0)
	# ratio = c(0/1, 1/2, 0/0) = c(0, 0.5, NaN) but scores_post[3] == 0 -> NA + warning
	expect_warning(
		res <- cs(
			scores_pre = c(0, 1, 0),
			scores_post = c(1, 2, 0),
			relation = "ratio"
		),
		regexp = "zero baseline"
	)
	checkmate::expect_numeric(res, len = 3L)
	expect_equal(res, c(0, 0.5, NA_real_))
})

test_that("safe_ratio handles NA denominator without error", {
	# safe_ratio is a package-internal standalone function (not an R6 method),
	# kept off the object to keep FeatureImportanceMethod lean for serialization.

	# All-NA denominator: division produces NA, no zero-denom warning
	expect_no_error(r <- safe_ratio(1, NA_real_))
	expect_identical(r, NA_real_)

	# Mixed: one true zero, one NA denominator
	# zero denom -> warn + NA; NA denom -> NA via division (no extra warn)
	expect_warning(
		r2 <- safe_ratio(c(1, 2), c(0, NA_real_)),
		regexp = "zero baseline"
	)
	expect_equal(r2, c(NA_real_, NA_real_))
})

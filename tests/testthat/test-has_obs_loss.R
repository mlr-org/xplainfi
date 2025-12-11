test_that("has_obs_loss returns TRUE for measures with obs_loss", {
	library(mlr3)
	# Classification measures with obs_loss
	expect_true(has_obs_loss(msr("classif.ce")))
	expect_true(has_obs_loss(msr("classif.acc")))
	expect_true(has_obs_loss(msr("classif.logloss")))

	# Regression measures with obs_loss
	expect_true(has_obs_loss(msr("regr.mse")))
	expect_true(has_obs_loss(msr("regr.mae")))
})

test_that("has_obs_loss returns FALSE for measures without obs_loss", {
	library(mlr3)
	# These measures don't have observation-wise losses
	expect_false(has_obs_loss(msr("classif.auc")))
	expect_false(has_obs_loss(msr("regr.rsq")))
	expect_false(has_obs_loss(msr("time_train")))
})
test_that("has_obs_loss works with character measure IDs", {
	# Should look up measure by ID and check
	expect_true(has_obs_loss("classif.ce"))
	expect_true(has_obs_loss("regr.mse"))
	expect_false(has_obs_loss("classif.auc"))
	expect_false(has_obs_loss("regr.rsq"))
})

test_that("has_obs_loss returns FALSE for invalid inputs", {
	# Non-existent measure ID
	expect_false(has_obs_loss("nonexistent_measure"))

	# Non-Measure objects
	expect_false(has_obs_loss(NULL))
	expect_false(has_obs_loss(42))
	expect_false(has_obs_loss(list()))
	expect_false(has_obs_loss(data.frame()))
})

test_that("has_obs_loss handles edge cases", {
	# Empty string
	expect_false(has_obs_loss(""))

	# NA values
	expect_false(has_obs_loss(NA))
	expect_false(has_obs_loss(NA_character_))
})

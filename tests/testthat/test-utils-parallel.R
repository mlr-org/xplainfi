# =============================================================================
# xplainfi_map execution backends
# =============================================================================

test_that("xplainfi_map preserves names across backends", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  x = c(a = "a", b = "b", c = "c")
  f = function(foi, is_sequential = TRUE) toupper(foi)

  expect_named(xplainfi_map(length(x), f, x), names(x))

  withr::defer(future::plan("sequential"))
  future::plan("multisession", workers = 2)
  expect_named(xplainfi_map(length(x), f, x), names(x))
  expect_equal(unlist(xplainfi_map(length(x), f, x)), c(a = "A", b = "B", c = "C"))
})

test_that("xplainfi_map preserves names with mirai daemons", {
  compute = getOption("mlr3.mirai_parallelization", "mlr3_parallelization")
  withr::defer(mirai::daemons(0, .compute = compute))
  mirai::daemons(2, .compute = compute)

  x = c(a = "a", b = "b", c = "c")
  f = function(foi, is_sequential = TRUE) toupper(foi)

  expect_equal(unlist(xplainfi_map(length(x), f, x)), c(a = "A", b = "B", c = "C"))
})

test_that("PFI labels features correctly under a non-sequential future plan", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  withr::defer(future::plan("sequential"))
  future::plan("multisession", workers = 2)

  task = sim_dgp_independent(n = 100L)
  pfi = PFI$new(
    task = task,
    learner = lrn("regr.rpart"),
    measure = msr("regr.mse"),
    n_repeats = 2L
  )
  pfi$compute()

  expect_method_output(pfi)
})

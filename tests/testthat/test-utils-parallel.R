# =============================================================================
# xplainfi_map execution backends
#
# The `feature` column of all perturbation results is built from the names of
# the per-feature result list (`rbindlist(idcol = )`), so every backend must
# preserve them. Worker processes are skipped on CRAN to stay within the
# two-core policy and to keep check runtime down; one worker is enough to take
# the non-sequential code path.
# =============================================================================

upper_map = function(x) {
  f = function(foi, is_sequential = TRUE) toupper(foi)
  unlist(xplainfi_map(length(x), f, x))
}

test_that("xplainfi_map preserves names when run sequentially", {
  expect_equal(upper_map(c(a = "a", b = "b", c = "c")), c(a = "A", b = "B", c = "C"))
})

test_that("xplainfi_map preserves names with a future plan", {
  skip_on_cran() # spawns a worker process
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  withr::defer(future::plan("sequential"))
  future::plan("multisession", workers = 1)

  expect_equal(upper_map(c(a = "a", b = "b", c = "c")), c(a = "A", b = "B", c = "C"))
})

test_that("xplainfi_map preserves names with mirai daemons", {
  skip_on_cran() # spawns a daemon process

  compute = getOption("mlr3.mirai_parallelization", "mlr3_parallelization")
  withr::defer(mirai::daemons(0, .compute = compute))
  mirai::daemons(1, .compute = compute)

  expect_equal(upper_map(c(a = "a", b = "b", c = "c")), c(a = "A", b = "B", c = "C"))
})

test_that("PFI labels features correctly under a non-sequential future plan", {
  skip_on_cran() # spawns a worker process
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  withr::defer(future::plan("sequential"))
  future::plan("multisession", workers = 1)

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

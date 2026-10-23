test_that("weights_arf returns a valid weight function", {
  skip_if_not_installed("arf")
  set.seed(913)
  task = sim_dgp_correlated(n = 300, r = 0.9)
  sampler = ConditionalARFSampler$new(task, num_trees = 20L, verbose = FALSE)
  wf = weights_arf(sampler)
  checkmate::expect_function(wf, args = c("data", "feature"))

  data = task$data(cols = task$feature_names)
  perm = copy(data)[, x1 := sample(x1)]
  w = wf(perm, "x1")
  checkmate::expect_numeric(w, len = nrow(data), lower = 0, finite = TRUE, any.missing = FALSE)
})

test_that("weights_arf: weights are flat for an independent feature and dispersed for a dependent one", {
  skip_if_not_installed("arf")
  set.seed(4711)
  task = sim_dgp_correlated(n = 500, r = 0.9)
  sampler = ConditionalARFSampler$new(task, num_trees = 20L, verbose = FALSE)
  wf = weights_arf(sampler)
  data = task$data(cols = task$feature_names)
  ess = function(w) sum(w)^2 / sum(w^2)

  w_dep = wf(copy(data)[, x1 := sample(x1)], "x1")
  w_ind = wf(copy(data)[, x4 := sample(x4)], "x4")
  expect_gt(ess(w_ind), 0.8 * nrow(data))
  expect_lt(ess(w_dep), ess(w_ind))
})

test_that("weights_arf conditioning_set: empty is constant, subset uses only those columns", {
  skip_if_not_installed("arf")
  set.seed(31)
  task = sim_dgp_correlated(n = 300, r = 0.9)
  sampler = ConditionalARFSampler$new(task, num_trees = 20L, verbose = FALSE)
  data = task$data(cols = task$feature_names)
  perm = copy(data)[, x1 := sample(x1)]
  ess = function(w) sum(w)^2 / sum(w^2)

  expect_equal(weights_arf(sampler, character(0))(perm, "x1"), rep(1, nrow(data)))
  w_x2 = weights_arf(sampler, "x2")(perm, "x1")
  w_x4 = weights_arf(sampler, "x4")(perm, "x1")
  checkmate::expect_numeric(w_x2, len = nrow(data), lower = 0, any.missing = FALSE)
  expect_lt(ess(w_x2), ess(w_x4))
  expect_gt(ess(w_x4), 0.8 * nrow(data))
  # Perturbed feature is dropped from the conditioning set
  expect_equal(weights_arf(sampler, c("x1", "x2"))(perm, "x1"), w_x2)
  expect_error(weights_arf(sampler, "nope"), "subset")
})

test_that("weights_arf accepts feature groups", {
  skip_if_not_installed("arf")
  set.seed(77)
  task = sim_dgp_correlated(n = 200, r = 0.9)
  sampler = ConditionalARFSampler$new(task, num_trees = 20L, verbose = FALSE)
  data = task$data(cols = task$feature_names)
  w = weights_arf(sampler)(copy(data)[, `:=`(x1 = sample(x1), x3 = sample(x3))], c("x1", "x3"))
  checkmate::expect_numeric(w, len = nrow(data), lower = 0, any.missing = FALSE)
})

test_that("weights_arf group weights are flat for independent features permuted column-wise", {
  skip_if_not_installed("arf")
  set.seed(1109)
  task = sim_dgp_independent(n = 400)
  sampler = ConditionalARFSampler$new(task, num_trees = 20L, verbose = FALSE)
  data = task$data(cols = task$feature_names)
  group = task$feature_names[1:2]
  perm = copy(data)[, (group) := lapply(.SD, sample), .SDcols = group]
  w = weights_arf(sampler)(perm, group)
  expect_gt(sum(w)^2 / sum(w^2), 0.8 * nrow(data))
})

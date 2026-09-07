# Custom testthat expectations for importance result validation
#
# These follow testthat 3e conventions.
# See: https://testthat.r-lib.org/articles/custom-expectation.html

# -----------------------------------------------------------------------------
# expect_importance_dt
# -----------------------------------------------------------------------------

#' Expectation for aggregated importance score tables
#'
#' Validates columns:
#' - `feature` is a character value without missings
#' - `importance` is numeric vector without missings or infinite values
#' - Variance-related columns (se, estimate, conf_lower, conf_upper, statistic, p.value) may contain NA
#'
#' @param x (data.table()) Importance result table to validate.
#' @param features (character()) Feature names used to test names and order of importance scores.
expect_importance_dt = function(x, features) {
  checkmate::expect_data_table(
    x,
    types = c("character", "numeric"),
    nrows = length(features),
    min.cols = 2
  )

  # Core columns must not have missing values
  checkmate::expect_character(x$feature, any.missing = FALSE)
  checkmate::expect_numeric(x$importance, any.missing = FALSE)

  # Variance-related columns may contain NA (e.g., CPI test statistics can fail for some features)
  variance_cols = c("se", "sd", "estimate", "conf_lower", "conf_upper", "statistic", "p.value")
  for (col in intersect(variance_cols, colnames(x))) {
    checkmate::expect_numeric(x[[col]], any.missing = TRUE)
  }
}

# -----------------------------------------------------------------------------
# expect_scores_dt
# -----------------------------------------------------------------------------

#' Expectation for iteration-wise importance score tables
#'
#' Validates $scores() output. Works for all FeatureImportanceMethod subclasses
#' (PFI, CFI, RFI, WVIM/LOCO, SAGE) despite their different column structures.
#'
#' @param x (data.table()) Score result table from $scores().
#' @param features (character()) Feature names that should appear in the table.
expect_scores_dt = function(x, features) {
  checkmate::expect_data_table(x, min.rows = length(features), any.missing = FALSE)
  checkmate::expect_character(x$feature, any.missing = FALSE)
  checkmate::expect_numeric(x$importance, any.missing = FALSE)
  expect_true(all(features %in% x$feature))
}

# -----------------------------------------------------------------------------
# expect_obs_loss_dt
# -----------------------------------------------------------------------------

#' Expectation for observation-wise loss tables
#'
#' Validates $obs_loss() output. Only applicable for perturbation methods with
#' decomposable measures (e.g., regr.mse, classif.ce).
#'
#' @param x (data.table()) Observation-wise loss table from $obs_loss().
#' @param features (character()) Feature names that should appear in the table.
expect_obs_loss_dt = function(x, features) {
  checkmate::expect_data_table(x, min.rows = length(features), any.missing = FALSE)
  checkmate::expect_character(x$feature, any.missing = FALSE)
  expect_true(all(c("row_ids", "loss_baseline", "loss_post", "obs_importance") %in% names(x)))
  checkmate::expect_numeric(x$obs_importance, any.missing = FALSE)
  expect_true(all(features %in% x$feature))
}

# -----------------------------------------------------------------------------
# expect_method_output
# -----------------------------------------------------------------------------

#' Omnibus expectation for a computed FeatureImportanceMethod
#'
#' Validates all three main outputs of a computed method:
#' - $importance(): always checked
#' - $scores(): always checked
#' - $obs_loss(): checked if the method supports it (decomposable measure + perturbation method)
#'
#' @param method A computed FeatureImportanceMethod (must have had $compute() called)
expect_method_output = function(method) {
  features = method$features

  expect_importance_dt(method$importance(), features = features)
  expect_scores_dt(method$scores(), features = features)

  # obs_loss is only available for perturbation methods with decomposable measures
  if (has_obs_loss(method$measure) && !inherits(method, "SAGE")) {
    expect_obs_loss_dt(method$obs_loss(), features = features)
  }
}

# Brute-force exact Shapley values of the empirical SAGE value function.
# Enumerates all coalitions via the method's own value function and averages
# marginal contributions over all m! feature orderings. Serves as the shared
# ground-truth reference for the sampling and exact estimators (kept in one
# place so both correctness anchors validate against the same computation).
brute_force_shapley = function(sage, task) {
  feats = sage$features
  m = length(feats)
  priv = sage$.__enclos_env__$private
  rr = sage$resample_result
  learner_fitted = rr$learners[[1]]
  test_dt = task$data(rows = rr$resampling$test_set(1))

  subsets = unlist(lapply(0:m, function(k) combn(m, k, simplify = FALSE)), recursive = FALSE)
  losses = priv$.evaluate_coalitions_batch(learner_fitted, test_dt, lapply(subsets, function(ix) feats[ix]), NULL)
  key = function(ix) if (length(ix) == 0L) "E" else paste(sort(ix), collapse = ",")
  vmap = new.env()
  for (i in seq_along(subsets)) {
    assign(key(subsets[[i]]), losses[i], envir = vmap)
  }
  baseline = get("E", envir = vmap)
  vfun = function(ix) baseline - get(key(ix), envir = vmap)

  gen_perms = function(x) {
    if (length(x) == 1L) {
      return(list(x))
    }
    out = list()
    for (i in seq_along(x)) {
      for (p in gen_perms(x[-i])) {
        out[[length(out) + 1L]] = c(x[i], p)
      }
    }
    out
  }
  perms = gen_perms(seq_len(m))
  phi = numeric(m)
  for (p in perms) {
    prev = vfun(integer(0))
    S = integer(0)
    for (j in p) {
      S = c(S, j)
      cur = vfun(S)
      phi[j] = phi[j] + (cur - prev)
      prev = cur
    }
  }
  phi = phi / length(perms)
  names(phi) = feats
  phi
}

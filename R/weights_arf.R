#' @title ARF-based importance sampling weights
#'
#' @description
#' Creates a weight function for [PerturbationImportance] methods that turns marginal perturbation into an
#' importance sampling estimator of conditional importance.
#' For a perturbed observation \eqn{(x_j', x_{-j})}, the weight is the density ratio
#' \deqn{w = \frac{p(x_j' \mid x_{-j})}{p(x_j')} = \frac{p(x_j', x_{-j})}{p(x_j')\,p(x_{-j})},}
#' with all densities estimated by the ARF stored in `sampler` via [arf::lik].
#' Weighting marginally perturbed losses by \eqn{w} yields the expectation under the conditional distribution
#' \eqn{p(x_j \mid x_{-j})}, i.e. the [CFI] target, without conditional sampling.
#'
#' @details
#' Weights are unnormalized and self-normalized downstream, so only ratios matter.
#' For a group of features, the weights assume that each column was permuted independently, as
#' [MarginalPermutationSampler] does, so the proposal density is the product of the per-feature marginals.
#' The marginal densities are evaluated on the unique rows of the respective column subsets, which for permutation
#' sampling are the original rows, so the cost per call is one full-column [arf::lik] call on all perturbed rows
#' plus two partial-column calls on the original rows.
#' This is comparable to conditional sampling with [arf::forge] at typical `n_repeats`, since `forge` amortizes
#' its leaf lookup over `samples_per_row` while `lik` is linear in the number of perturbed rows.
#'
#' The effective sample size \eqn{(\sum_i w_i)^2 / \sum_i w_i^2}, reported in `$scores()` as `ess`, shrinks as the
#' conditional \eqn{p(x_j \mid x_{-j})} becomes narrower relative to the marginal \eqn{p(x_j)}, i.e. with stronger
#' dependence and larger conditioning sets.
#' A small `ess` indicates that the weighted estimate is driven by few observations and conditional sampling
#' ([CFI] with `estimator = "sampling"`) should be preferred.
#'
#' Fit the sampler with `finite_bounds = "local"` so that leaf densities do not extend past the observed range of
#' each feature; unbounded leaves put mass where no data exist and distort the density ratios in the tails.
#'
#' @param sampler ([ConditionalARFSampler]) Fitted sampler whose `psi` and `arf_model` provide the density estimate.
#' @param conditioning_set (`character()` | `NULL`: `NULL`) Features to condition on.
#'   `NULL` conditions on all features except the perturbed one(s) ([CFI]).
#'   A subset gives the [RFI] weight \eqn{p(x_j' \mid x_S) / p(x_j')}, `character(0)` gives constant weights ([PFI]).
#'   The perturbed feature(s) are always removed from the set.
#' @return (`function(data, feature)`) Weight function returning a non-negative `numeric(nrow(data))`.
#'   `data` is a [data.table][data.table::data.table] of perturbed feature values (all task features),
#'   `feature` the perturbed feature(s).
#' @export
#' @examplesIf requireNamespace("arf", quietly = TRUE)
#' library(mlr3)
#' task = sim_dgp_correlated(n = 300)
#' sampler = ConditionalARFSampler$new(task, finite_bounds = "local", verbose = FALSE)
#' wf = weights_arf(sampler)
#' perturbed = task$data(cols = task$feature_names)[, x1 := sample(x1)]
#' w = wf(perturbed, "x1")
#' sum(w)^2 / sum(w^2) # effective sample size
#'
#' # Same weights inside CFI:
#' cfi = CFI$new(
#'   task = task,
#'   learner = lrn("regr.rpart"),
#'   measure = msr("regr.mse"),
#'   sampler = sampler,
#'   estimator = "weighting",
#'   n_repeats = 5
#' )
#' cfi$compute()
#' cfi$scores()[, .(feature, iter_repeat, importance, ess)]
weights_arf = function(sampler, conditioning_set = NULL) {
  checkmate::assert_r6(sampler, "ConditionalARFSampler")
  psi = sampler$psi
  arf_model = sampler$arf_model
  features = sampler$task$feature_names
  checkmate::assert_subset(conditioning_set, features)

  # Log-density of the unique rows of `cols`, mapped back to all rows.
  # Partial-column queries take arf's slow marginalization path, so deduplicating matters.
  log_marginal = function(data, cols) {
    sub = data[, .SD, .SDcols = cols]
    uniq = unique(sub)
    ll = arf::lik(psi, uniq, log = TRUE, parallel = FALSE)
    ll[uniq[sub, which = TRUE, on = cols]]
  }

  function(data, feature) {
    checkmate::assert_data_table(data)
    checkmate::assert_subset(feature, features)
    cond = setdiff(conditioning_set %??% features, feature)
    if (length(cond) == 0) {
      return(rep(1, nrow(data)))
    }
    data = data[, .SD, .SDcols = features]

    # Full-column query takes the fast terminal-node path; partial queries (RFI) do not.
    log_joint = if (length(cond) == length(features) - length(feature)) {
      arf::lik(psi, data, arf = arf_model, log = TRUE, parallel = FALSE)
    } else {
      log_marginal(data, c(feature, cond))
    }
    # Groups are permuted column by column, so the proposal is the product of per-feature marginals.
    log_proposal = Reduce(`+`, lapply(feature, \(k) log_marginal(data, k)))
    log_w = log_joint - log_proposal - log_marginal(data, cond)
    # Weights are self-normalized downstream; shifting by the max avoids underflow.
    exp(log_w - max(log_w))
  }
}

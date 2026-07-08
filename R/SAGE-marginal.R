#' @title Marginal SAGE
#'
#' @description [SAGE] with marginal sampling (features are marginalized independently).
#' This is the standard SAGE implementation.
#'
#' @details
#' Marginal SAGE samples held-out features from their marginal distribution, following the sampling
#' algorithm of Covert et al. (2020) and matching the behavior of the reference Python `sage` package.
#' Note that SAGE's theoretical definition marginalizes held-out features conditionally (their Eq. 1);
#' marginal sampling is the tractable approximation, and the two differ when features are dependent.
#' See [ConditionalSAGE] for the conditional variant.
#'
#' See [SAGE] for details on the available estimators, standard errors and confidence intervals,
#' and how this implementation relates to the reference Python `sage` package.
#'
#' @seealso [ConditionalSAGE]
#'
#' @examplesIf requireNamespace("ranger", quietly = TRUE) && requireNamespace("mlr3learners", quietly = TRUE)
#' library(mlr3)
#' task = tgen("friedman1")$generate(n = 100)
#' sage = MarginalSAGE$new(
#'   task = task,
#'   learner = lrn("regr.ranger", num.trees = 50),
#'   measure = msr("regr.mse"),
#'   n_permutations = 3L,
#'   n_samples = 20
#' )
#' sage$compute()
#' @export
MarginalSAGE = R6Class(
  "MarginalSAGE",
  inherit = SAGE,
  public = list(
    #' @description
    #' Creates a new instance of the MarginalSAGE class.
    #' @param task,learner,measure,resampling,features,estimator,n_permutations,n_coalitions,kernel_variant,max_features,batch_size,n_samples,early_stopping,se_threshold,min_permutations,check_interval Passed to [SAGE].
    initialize = function(
      task,
      learner,
      measure = NULL,
      resampling = NULL,
      features = NULL,
      estimator = c("permutation", "kernel", "exact"),
      n_permutations = NULL,
      n_coalitions = NULL,
      kernel_variant = NULL,
      max_features = 12L,
      batch_size = 5000L,
      n_samples = 100L,
      early_stopping = FALSE,
      se_threshold = 0.01,
      min_permutations = 10L,
      check_interval = 1L
    ) {
      # No need to initialize sampler as marginal sampling is done differently here
      super$initialize(
        task = task,
        learner = learner,
        measure = measure,
        resampling = resampling,
        features = features,
        estimator = estimator,
        n_permutations = n_permutations,
        n_coalitions = n_coalitions,
        kernel_variant = kernel_variant,
        max_features = max_features,
        batch_size = batch_size,
        n_samples = n_samples,
        early_stopping = early_stopping,
        se_threshold = se_threshold,
        min_permutations = min_permutations,
        check_interval = check_interval
      )
      # Use the task's data as reference for later marginalization. The subsample is
      # drawn once here, so post-construction edits of $param_set$values$n_samples do
      # not redraw it (documented in the n_samples param docs).
      private$reference_data = self$task$data(cols = self$task$feature_names)

      # Subsample reference data if it's too large for efficiency
      if (nrow(private$reference_data) > n_samples) {
        sample_idx = sample(nrow(private$reference_data), size = n_samples)
        private$reference_data = private$reference_data[sample_idx, ]
      }

      self$label = "Marginal SAGE"
    }
  ),

  private = list(
    reference_data = NULL,

    # Marginal-specific data expansion: Cartesian product with reference data
    .expand_coalitions_data = function(test_dt, all_coalitions) {
      n_test = nrow(test_dt)
      n_reference = nrow(private$reference_data)
      all_expanded_data = vector("list", length(all_coalitions))

      for (i in seq_along(all_coalitions)) {
        coalition = all_coalitions[[i]]

        # Cartesian product: each test instance * all reference instances
        test_expanded = test_dt[rep(seq_len(n_test), each = n_reference)]
        reference_expanded = private$reference_data[rep(seq_len(n_reference), times = n_test)]

        # Add tracking columns BEFORE marginalization
        test_expanded[, .coalition_id := i]
        test_expanded[, .test_instance_id := rep(seq_len(n_test), each = n_reference)]

        # Replace marginalized features with reference values
        # Maintains correlation structure in out-of-coalition features (block-wise sampling)
        marginalize_features = setdiff(self$features, coalition)
        if (length(marginalize_features) > 0) {
          test_expanded[,
            (marginalize_features) := reference_expanded[, .SD, .SDcols = marginalize_features]
          ]
        }

        all_expanded_data[[i]] = test_expanded
      }

      rbindlist(all_expanded_data)
    }
  )
)

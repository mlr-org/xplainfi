#' @title Gaussian Conditional Sampler
#'
#' @description Implements conditional sampling assuming features follow a multivariate
#' Gaussian distribution. Computes conditional distributions analytically using standard
#' formulas for multivariate normal distributions.
#'
#' @details
#' For a joint Gaussian distribution \eqn{X \sim N(\mu, \Sigma)}, partitioned as
#' \eqn{X = (X_A, X_B)}, the conditional distribution is:
#'
#' \deqn{X_B | X_A = x_A \sim N(\mu_{B|A}, \Sigma_{B|A})}
#'
#' where:
#' \deqn{\mu_{B|A} = \mu_B + \Sigma_{BA} \Sigma_{AA}^{-1} (x_A - \mu_A)}
#' \deqn{\Sigma_{B|A} = \Sigma_{BB} - \Sigma_{BA} \Sigma_{AA}^{-1} \Sigma_{AB}}
#'
#' This is equivalent to the regression formulation used by fippy:
#' \deqn{\beta = \Sigma_{BA} \Sigma_{AA}^{-1}}
#' \deqn{\mu_{B|A} = \mu_B + \beta (x_A - \mu_A)}
#' \deqn{\Sigma_{B|A} = \Sigma_{BB} - \beta \Sigma_{AB}}
#'
#' **Assumptions:**
#' - Features are approximately multivariate normal
#' - Only continuous features are supported
#'
#' **Advantages:**
#' - Very fast (closed-form solution)
#' - Deterministic (given seed)
#' - No hyperparameters
#' - Memory efficient
#'
#' **Limitations:**
#' - Strong distributional assumption
#' - May produce out-of-range values for bounded features
#' - Cannot handle categorical features
#' - Integer features are treated as continuous and rounded back to integers
#'
#' @examples
#' library(mlr3)
#' task = tgen("friedman1")$generate(n = 100)
#' sampler = ConditionalGaussianSampler$new(task)
#'
#' # Sample x2, x3 conditioned on x1
#' test_data = task$data(rows = 1:5)
#' sampled = sampler$sample_newdata(
#'   feature = c("important2", "important3"),
#'   newdata = test_data,
#'   conditioning_set = "important1"
#' )
#'
#' @references `r print_bib("anderson_2003")`
#'
#' @export
ConditionalGaussianSampler = R6Class(
	"ConditionalGaussianSampler",
	inherit = ConditionalSampler,
	public = list(
		#' @field feature_types (`character()`) Feature types supported by the sampler.
		feature_types = c("numeric", "integer"),

		#' @field mu (`numeric()`) Mean vector estimated from training data.
		mu = NULL,

		#' @field sigma (`matrix()`) Covariance matrix estimated from training data.
		sigma = NULL,

		#' @description
		#' Creates a new ConditionalGaussianSampler.
		#' @param task ([mlr3::Task]) Task to sample from. Must have only numeric/integer features.
		#' @param conditioning_set (`character` | `NULL`) Default conditioning set to use in `$sample()`.
		initialize = function(task, conditioning_set = NULL) {
			super$initialize(task, conditioning_set = conditioning_set)

			# Extract feature data as matrix
			X = as.matrix(self$task$data(cols = self$task$feature_names))

			# Estimate mean and covariance
			self$mu = colMeans(X)
			self$sigma = stats::cov(X)

			self$label = "Gaussian Conditional Sampler"
		}
	),

	private = list(
		# Core sampling logic implementing conditional Gaussian sampling
		.sample_conditional = function(data, feature, conditioning_set, samples_per_row = 1L, ...) {
			# Marginal case (no conditioning): identical mean for every row, just stack draws.
			if (length(conditioning_set) == 0) {
				mu_feature = self$mu[feature]
				sigma_feature = self$sigma[feature, feature, drop = FALSE]
				n = nrow(data)

				out = data[rep.int(seq_len(.N), times = samples_per_row)]
				samples = mvtnorm::rmvnorm(n * samples_per_row, mean = mu_feature, sigma = sigma_feature)
				out[, (feature) := as.data.table(samples)]
				out = private$.ensure_feature_types(out)
				return(out[, .SD, .SDcols = c(self$task$target_names, self$task$feature_names)])
			}

			# Conditional case
			training_data = self$task$data(cols = self$task$feature_names)
			X_cond = as.matrix(training_data[, .SD, .SDcols = conditioning_set])
			X_cond = cbind(1, X_cond)

			n_features = length(feature)
			coef_list = vector("list", n_features)
			residuals_mat = matrix(0, nrow = nrow(training_data), ncol = n_features)
			for (j in seq_along(feature)) {
				y = training_data[[feature[j]]]
				fit = lm.fit(X_cond, y)
				coef_list[[j]] = fit$coefficients
				residuals_mat[, j] = fit$residuals
			}
			cond_cov = stats::cov(residuals_mat)
			if (n_features == 1) {
				cond_cov = matrix(cond_cov, 1, 1)
			}

			X_new = as.matrix(data[, .SD, .SDcols = conditioning_set])
			X_new = cbind(1, X_new)

			pred_mat = matrix(0, nrow = nrow(data), ncol = n_features)
			for (j in seq_along(feature)) {
				pred_mat[, j] = X_new %*% coef_list[[j]]
			}

			n = nrow(data)
			out = data[rep.int(seq_len(.N), times = samples_per_row)]

			# `samples_per_row` independent residual draws across all evidence rows, stacked in draw-major.
			# Means are repeated to align with the replicated evidence rows.
			noise = mvtnorm::rmvnorm(n * samples_per_row, mean = rep(0, n_features), sigma = cond_cov)
			means_replicated = pred_mat[rep.int(seq_len(n), times = samples_per_row), , drop = FALSE]
			samples = noise + means_replicated

			out[, (feature) := as.data.table(samples)]
			out = private$.ensure_feature_types(out)

			out[, .SD, .SDcols = c(self$task$target_names, self$task$feature_names)]
		}
	)
)

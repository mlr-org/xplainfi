#' Simulate data as in Ewald et al. (2024)
#'
#' @description
#' Reproduces the data generating process from Ewald et al. (2024) for benchmarking
#' feature importance methods. Includes correlated features and interaction effects.
#'
#' @details
#' **Mathematical Model:**
#' \deqn{X_1, X_3, X_5 \sim \text{Uniform}(0,1)}
#' \deqn{X_2 = X_1 + \varepsilon_2, \quad \varepsilon_2 \sim N(0, \sqrt{0.001})}
#' \deqn{X_4 = X_3 + \varepsilon_4, \quad \varepsilon_4 \sim N(0, \sqrt{0.1})}
#' \deqn{Y = X_4 + X_5 + X_4 \cdot X_5 + \varepsilon, \quad \varepsilon \sim N(0, \sqrt{0.1})}
#'
#' **Feature Properties:**
#' - X1, X3, X5: Independent uniform(0,1) distributions
#' - X2: Nearly perfect copy of X1 (correlation approximately 0.99)
#' - X4: Noisy copy of X3 (correlation approximately 0.94)
#' - Y depends on X4, X5, and their interaction
#'
#' @param n (`integer(1)`) Number of samples to create.
#'
#' @return A regression task ([mlr3::TaskRegr]) with [data.table][data.table::data.table] backend.
#' @export
#' @references `r print_bib("ewald_2024")`
#' @family simulation
#' @examples
#' sim_dgp_ewald(100)
#'
sim_dgp_ewald = function(n = 500) {
  x1 = runif(n)
  x3 = runif(n)
  x5 = runif(n)

  x2 = x1 + rnorm(n, 0, sd = 0.001)
  x4 = x3 + rnorm(n, 0, sd = 0.1)

  y = x4 + x5 + x4 * x5 + rnorm(n, 0, sd = 0.1)

  xdf = data.table::data.table(
    y,
    x1,
    x2,
    x3,
    x4,
    x5
  )

  mlr3::TaskRegr$new(backend = xdf, target = "y", id = paste0("Ewald_", n))
}

#' @title Simulation DGPs for Feature Importance Method Comparison
#' @name sim_dgp_scenarios
#' @description
#' These data generating processes (DGPs) are designed to illustrate specific
#' strengths and weaknesses of different feature importance methods like PFI, CFI, and RFI.
#' Each DGP focuses on one primary challenge to make the differences between methods clear.
#'
#' The `_nonlinear` variants change only the link functions of a base DGP and keep everything else
#' (features, roles, noise) identical, so each base/variant pair differs in exactly one respect.
#' They exist where nonlinearity is methodologically interesting on its own, not for completeness:
#' the interaction case is already covered by [mlr3::mlr_task_generators_friedman1],
#' and the confounded and mediated scenarios gain nothing from a nonlinear link.
#'
#' @references `r print_bib("ewald_2024")`
NULL

# Shared by the _nonlinear variants so the two cannot drift apart: the value of having
# both is that they differ only in feature correlation, not in the outcome model.
sine_effect = function(x) {
  2 * sin(2 * x)
}

#' @describeIn sim_dgp_scenarios Correlated features demonstrating PFI's limitations
#'
#' @details
#' **Correlated Features DGP:**
#' This DGP creates highly correlated predictors where PFI will show artificially low
#' importance due to redundancy, while CFI will correctly identify each feature's
#' conditional contribution.
#'
#' **Mathematical Model:**
#' \deqn{(X_1, X_2)^T \sim \text{MVN}(0, \Sigma)}
#' where \eqn{\Sigma} is a \eqn{2 \times 2} covariance matrix with 1 on the diagonal and correlation \eqn{r} on the off-diagonal.
#' \deqn{X_3 \sim N(0,1), \quad X_4 \sim N(0,1)}
#' \deqn{Y = 2 \cdot X_1 + X_3 + \varepsilon}
#' where \eqn{\varepsilon \sim N(0, 0.2^2)}.
#'
#' **Feature Properties:**
#' - `x1`: Standard normal from MVN, direct causal effect on y (\eqn{\beta = 2.0})
#' - `x2`: Correlated with `x1` (correlation = `r`), NO causal effect on y (\eqn{\beta = 0})
#' - `x3`: Independent standard normal, direct causal effect on y (\eqn{\beta = 1.0})
#' - `x4`: Independent standard normal, no effect on y (\eqn{\beta = 0})
#'
#' **Expected Behavior:**
#' - Will depend on the used learner and the strength of correlation (`r`)
#' - **Marginal methods** (PFI, Marginal SAGE): Should falsely assign importance to x2 due to correlation with x1
#' - **CFI** Should correctly assign near-zero importance to x2
#' - x2 is a "spurious predictor" - correlated with causal feature but not causal itself
#'
#' @param n (`integer(1)`) Number of samples to generate.
#' @param r (`numeric(1)`: `0.9`) Correlation between x1 and x2. Must be between -1 and 1.
#' @return A regression task ([mlr3::TaskRegr]) with [data.table][data.table::data.table] backend.
#' @export
#' @family simulation
#' @examples
#' task = sim_dgp_correlated(200)
#' task$data()
#'
#' # With different correlation
#' task_high_cor = sim_dgp_correlated(200, r = 0.95)
#' cor(task_high_cor$data()$x1, task_high_cor$data()$x2)
sim_dgp_correlated = function(n = 500L, r = 0.9) {
  # Generate x1 (causal) and x2 (spurious) with fixed correlation
  x12 = mvtnorm::rmvnorm(n, sigma = stats::toeplitz(r^(0:1)))
  x1 = x12[, 1]
  # Spurious feature: correlated with x1 but no causal effect
  x2 = x12[, 2]

  # Independent features
  x3 = rnorm(n) # Has causal effect
  x4 = rnorm(n) # No causal effect (noise)

  # Outcome depends only on x1 and x3 (x2 has NO causal effect despite correlation)
  y = 2 * x1 + x3 + rnorm(n, 0, 0.2)

  data.table::data.table(
    y = y,
    x1 = x1,
    x2 = x2,
    x3 = x3,
    x4 = x4
  ) |>
    mlr3::TaskRegr$new(target = "y", id = paste0("correlated_", n))
}

#' @describeIn sim_dgp_scenarios Correlated features with a non-monotone causal effect
#'
#' @details
#' **Correlated Nonlinear DGP:**
#' Identical to the correlated DGP except that the causal effect of `x1` is a sine.
#' The spurious feature `x2` now proxies a *non-monotone* effect, and a linear learner
#' recovers little of the signal from either.
#'
#' **Mathematical Model:**
#' \deqn{(X_1, X_2)^T \sim \text{MVN}(0, \Sigma), \quad X_3 \sim N(0,1), \quad X_4 \sim N(0,1)}
#' \deqn{Y = 2 \cdot \sin(2 X_1) + X_3 + \varepsilon}
#' where \eqn{\Sigma} has correlation \eqn{r} off the diagonal and \eqn{\varepsilon \sim N(0, 0.2^2)}.
#'
#' **Feature Properties:**
#' - `x1`: Non-monotone causal effect, correlated with `x2`
#' - `x2`: Correlated with `x1` (correlation = `r`), NO causal effect on y
#' - `x3`: Independent, linear causal effect (\eqn{\beta = 1.0}), kept as a within-DGP reference
#' - `x4`: Independent, no effect on y
#'
#' **Expected Behavior:**
#' - As for the correlated DGP, but the size of the spurious `x2` importance now depends on how well the
#'   learner captures the sine, so it separates learner flexibility from the marginal-vs-conditional question
#' - A linear learner assigns low importance to `x1` and `x2` alike
#'
#' @export
#' @family simulation
#' @examples
#' task = sim_dgp_correlated_nonlinear(200)
#' task$data()
sim_dgp_correlated_nonlinear = function(n = 500L, r = 0.9) {
  x12 = mvtnorm::rmvnorm(n, sigma = stats::toeplitz(r^(0:1)))
  x1 = x12[, 1]
  x2 = x12[, 2]
  x3 = rnorm(n)
  x4 = rnorm(n)

  y = sine_effect(x1) + x3 + rnorm(n, 0, 0.2)

  data.table::data.table(
    y = y,
    x1 = x1,
    x2 = x2,
    x3 = x3,
    x4 = x4
  ) |>
    mlr3::TaskRegr$new(target = "y", id = paste0("correlated_nonlinear_", n))
}

#' @describeIn sim_dgp_scenarios Mediated effects showing direct vs total importance
#'
#' @details
#' **Mediated Effects DGP:**
#' This DGP demonstrates the difference between total and direct causal effects.
#' Some features affect the outcome only through mediators.
#'
#' **Mathematical Model:**
#' \deqn{\text{exposure} \sim N(0,1), \quad \text{direct} \sim N(0,1)}
#' \deqn{\text{mediator} = 0.8 \cdot \text{exposure} + 0.6 \cdot \text{direct} + \varepsilon_m}
#' \deqn{Y = 1.5 \cdot \text{mediator} + 0.5 \cdot \text{direct} + \varepsilon}
#' where \eqn{\varepsilon_m \sim N(0, 0.3^2)} and \eqn{\varepsilon \sim N(0, 0.2^2)}.
#'
#' **Feature Properties:**
#' - `exposure`: Has no direct effect on y, only through mediator (total effect = 1.2)
#' - `mediator`: Mediates the effect of exposure on y
#' - `direct`: Has both direct effect on y and effect on mediator
#' - `noise`: No causal relationship to y
#'
#' **Causal Structure:** exposure -> mediator -> y <- direct -> mediator
#'
#' @export
#' @family simulation
#' @examples
#' task = sim_dgp_mediated(200)
#' task$data()
sim_dgp_mediated = function(n = 500L) {
  # Initial exposure variable
  exposure = rnorm(n)

  # Direct predictor that affects both mediator and outcome
  direct = rnorm(n)

  # Mediator affected by both exposure and direct
  mediator = 0.8 * exposure + 0.6 * direct + rnorm(n, 0, 0.3)

  # Noise variable
  noise = rnorm(n)

  # Outcome depends on mediator and direct effect, but NOT directly on exposure
  y = 1.5 * mediator + 0.5 * direct + rnorm(n, 0, 0.2)

  data.table::data.table(
    y = y,
    exposure = exposure,
    mediator = mediator,
    direct = direct,
    noise = noise
  ) |>
    mlr3::TaskRegr$new(target = "y", id = paste0("mediated_", n))
}

#' @describeIn sim_dgp_scenarios Confounding scenario for conditional sampling
#'
#' @details
#' **Confounding DGP:**
#' This DGP includes a confounder that affects both a feature and the outcome.
#' Uses simple coefficients for easy interpretation.
#'
#' **Mathematical Model:**
#' \deqn{H \sim N(0,1)}
#' \deqn{X_1 = H + \varepsilon_1}
#' \deqn{\text{proxy} = H + \varepsilon_p, \quad \text{independent} \sim N(0,1)}
#' \deqn{Y = H + X_1 + \text{independent} + \varepsilon}
#' where all \eqn{\varepsilon \sim N(0, 0.5^2)} independently.
#'
#' **Model Structure:**
#' - Confounder H ~ N(0,1) (potentially unobserved)
#' - x1 = H + noise (affected by confounder)
#' - proxy = H + noise (noisy measurement of confounder)
#' - independent ~ N(0,1) (truly independent)
#' - y = H + x1 + independent + noise
#'
#' **Expected Behavior:**
#' - **PFI**: Will show inflated importance for x1 due to confounding
#' - **CFI**: Should partially account for confounding through conditional sampling and reduce its importance
#' - **RFI conditioning on proxy**: Should reduce confounding bias by conditioning on proxy
#'
#' @param n (`integer(1)`: `500L`) Number of observations to generate.
#' @param hidden (`logical(1)`: `TRUE`) Whether to hide the confounder from the returned task.
#'   If `FALSE`, the confounder is included as a feature, allowing direct adjustment.
#'   If `TRUE` (default), only the proxy is available, simulating unmeasured confounding.
#'
#' @export
#' @family simulation
#' @examples
#' # Hidden confounder scenario (traditional)
#' task_hidden = sim_dgp_confounded(200, hidden = TRUE)
#' task_hidden$feature_names  # proxy available but not confounder
#'
#' # Observable confounder scenario
#' task_observed = sim_dgp_confounded(200, hidden = FALSE)
#' task_observed$feature_names  # both confounder and proxy available
sim_dgp_confounded = function(n = 500L, hidden = TRUE) {
  # Confounder
  confounder = rnorm(n)

  # Feature affected by confounder
  x1 = confounder + rnorm(n, 0, 0.5)

  # Proxy measurement of confounder (observable but noisy)
  proxy = confounder + rnorm(n, 0, 0.5)

  # Independent feature unaffected by confounder
  independent = rnorm(n)

  # Outcome affected by confounder, x1, and independent feature
  y = confounder + x1 + independent + rnorm(n, 0, 0.5)

  # Create data.table conditionally including the confounder
  if (hidden) {
    # Traditional scenario: confounder is hidden, only proxy available
    dt = data.table::data.table(
      y = y,
      x1 = x1,
      proxy = proxy,
      independent = independent
    )
    task_id = paste0("confounded_hidden_", n)
  } else {
    # Observable confounder scenario: both confounder and proxy available
    dt = data.table::data.table(
      y = y,
      x1 = x1,
      confounder = confounder,
      proxy = proxy,
      independent = independent
    )
    task_id = paste0("confounded_observed_", n)
  }

  mlr3::TaskRegr$new(dt, target = "y", id = task_id)
}

#' @describeIn sim_dgp_scenarios Interaction effects between features
#'
#' @details
#' **Interaction Effects DGP:**
#' This DGP demonstrates a pure interaction effect where features have no main effects.
#'
#' **Mathematical Model:**
#' \deqn{Y = 2 \cdot X_1 \cdot X_2 + X_3 + \varepsilon}
#' where \eqn{X_j \sim N(0,1)} independently and \eqn{\varepsilon \sim N(0, 0.5^2)}.
#'
#' **Feature Properties:**
#' - `x1`, `x2`: Independent features with ONLY interaction effect (no main effects)
#' - `x3`: Independent feature with main effect only
#' - `noise1`, `noise2`: No causal effects
#'
#' **Expected Behavior:**
#' - Will depend on the used learner and its ability to model interactions
#' @export
#' @family simulation
#' @examples
#' task = sim_dgp_interactions(200)
#' task$data()
sim_dgp_interactions = function(n = 500L) {
  # Independent features for interaction
  x1 = rnorm(n)
  x2 = rnorm(n)

  # Independent feature with main effect
  x3 = rnorm(n)

  # Noise features
  noise1 = rnorm(n)
  noise2 = rnorm(n)

  # Outcome with ONLY interaction between x1 and x2 (no main effects), plus main effect of x3
  y = 2 * x1 * x2 + x3 + rnorm(n, 0, 0.5)

  data.table::data.table(
    y = y,
    x1 = x1,
    x2 = x2,
    x3 = x3,
    noise1 = noise1,
    noise2 = noise2
  ) |>
    mlr3::TaskRegr$new(target = "y", id = paste0("interactions_", n))
}

#' @describeIn sim_dgp_scenarios Independent features baseline scenario
#'
#' @details
#' **Independent Features DGP:**
#' This is a baseline scenario where all features are independent and their
#' effects are additive. All importance methods should give similar results.
#'
#' **Mathematical Model:**
#' \deqn{Y = 2.0 \cdot X_1 + 1.0 \cdot X_2 + 0.5 \cdot X_3 + \varepsilon}
#' where \eqn{X_j \sim N(0,1)} independently and \eqn{\varepsilon \sim N(0, 0.2^2)}.
#'
#' **Feature Properties:**
#' - `important1-3`: Independent features with different effect sizes
#' - `unimportant1-2`: Independent noise features with no effect
#'
#' **Expected Behavior:**
#' - **All methods**: Should rank features consistently by their true effect sizes
#' - **Ground truth**: important1 > important2 > important3 > unimportant1,2 (approximately 0)
#'
#' @export
#' @family simulation
#' @examples
#' task = sim_dgp_independent(200)
#' task$data()
sim_dgp_independent = function(n = 500L) {
  # Independent important features with different effect sizes
  important1 = rnorm(n)
  important2 = rnorm(n)
  important3 = rnorm(n)

  # Independent unimportant features
  unimportant1 = rnorm(n)
  unimportant2 = rnorm(n)

  # Additive linear outcome
  y = 2.0 * important1 + 1.0 * important2 + 0.5 * important3 + rnorm(n, 0, 0.2)

  data.table::data.table(
    y = y,
    important1 = important1,
    important2 = important2,
    important3 = important3,
    unimportant1 = unimportant1,
    unimportant2 = unimportant2
  ) |>
    mlr3::TaskRegr$new(target = "y", id = paste0("independent_", n))
}

#' @describeIn sim_dgp_scenarios Independent features with nonlinear additive effects
#'
#' @details
#' **Independent Nonlinear DGP:**
#' Identical to the independent DGP except that two of the three causal effects are nonlinear.
#' The effects stay additive, so this isolates nonlinearity from interactions
#' (for which see [mlr3::mlr_task_generators_friedman1]).
#'
#' **Mathematical Model:**
#' \deqn{Y = 2 \cdot \sin(2 X_1) + X_2^2 + 0.5 \cdot X_3 + \varepsilon}
#' where \eqn{X_j \sim N(0,1)} independently and \eqn{\varepsilon \sim N(0, 0.2^2)}.
#'
#' **Feature Properties:**
#' - `important1`: Non-monotone (sine) effect
#' - `important2`: Symmetric (quadratic) effect, so its linear correlation with y is zero
#' - `important3`: Linear effect (\eqn{\beta = 0.5}), kept as a within-DGP reference
#' - `unimportant1-2`: Independent noise features with no effect
#'
#' **Expected Behavior:**
#' - **Flexible learners** (e.g. random forests): Rank important1 and important2 above important3
#' - **Linear learners**: Assign important2 essentially zero importance, since a symmetric effect has no
#'   linear signal; the importance ranking then depends on the learner rather than on the DGP
#'
#' @export
#' @family simulation
#' @examples
#' task = sim_dgp_independent_nonlinear(200)
#' task$data()
sim_dgp_independent_nonlinear = function(n = 500L) {
  important1 = rnorm(n)
  important2 = rnorm(n)
  important3 = rnorm(n)
  unimportant1 = rnorm(n)
  unimportant2 = rnorm(n)

  y = sine_effect(important1) + important2^2 + 0.5 * important3 + rnorm(n, 0, 0.2)

  data.table::data.table(
    y = y,
    important1 = important1,
    important2 = important2,
    important3 = important3,
    unimportant1 = unimportant1,
    unimportant2 = unimportant2
  ) |>
    mlr3::TaskRegr$new(target = "y", id = paste0("independent_nonlinear_", n))
}

# Shapley Additive Global Importance (SAGE)

``` r
library(xplainfi)
library(mlr3)
library(mlr3learners)
library(ggplot2)
```

## Introduction

Shapley Additive Global Importance (SAGE) is a feature importance method
based on cooperative game theory that uses Shapley values to fairly
distribute the total prediction performance among all features. Unlike
permutation-based methods that measure the drop in performance when
features are perturbed, SAGE measures how much each feature contributes
to the model’s overall performance by marginalizing (removing) features.

The key insight of SAGE is that it provides a complete decomposition of
the model’s performance: the sum of all SAGE values equals the
difference between the model’s performance and the performance when all
features are marginalized.

**xplainfi** provides two implementations of SAGE:

- **MarginalSAGE**: Marginalizes features independently (standard SAGE)
- **ConditionalSAGE**: Marginalizes features conditionally using ARF
  sampling

## Demonstration with Correlated Features

To showcase the difference between Marginal and Conditional SAGE, we’ll
use the
[`sim_dgp_correlated()`](https://jemus42.github.io/xplainfi/reference/sim_dgp_scenarios.md)
function which creates a simple linear DGP with two correlated features.

**Model:** \\(X_1, X_2)^T \sim \text{MVN}(0, \Sigma)\\

where \\\Sigma\\ is a 2×2 covariance matrix with 1 on the diagonal and
correlation \\r\\ (default 0.5) on the off-diagonal.

\\X_3 \sim N(0,1), \quad X_4 \sim N(0,1)\\ \\Y = 2 \cdot X_1 + X_3 +
\varepsilon\\

where \\\varepsilon \sim N(0, 0.2^2)\\.

**Key properties:**

- `x1` has a direct causal effect on y (β=2.0)
- `x2` is correlated with x1 (r = 0.5 from MVN) but has **no causal
  effect** on y
- `x3` is independent with a causal effect (β=1.0)
- `x4` is independent noise (β=0)

``` r
set.seed(123)
task = sim_dgp_correlated(n = 1000, r = 0.5)

# Check the correlation structure
task_data = task$data()
correlation_matrix = cor(task_data[, c("x1", "x2", "x3", "x4")])
round(correlation_matrix, 3)
#>        x1     x2     x3     x4
#> x1  1.000  0.447 -0.005 -0.048
#> x2  0.447  1.000 -0.049 -0.054
#> x3 -0.005 -0.049  1.000  0.051
#> x4 -0.048 -0.054  0.051  1.000
```

**Expected behavior:**

- **Marginal SAGE**: Should show high importance for both x1 and x2 due
  to their correlation, even though x2 has no causal effect
- **Conditional SAGE**: Should show high importance for x1 but near-zero
  importance for x2 (correctly identifying the spurious predictor)

Let’s set up our learner and measure. We’ll use a random forest and
instantiate a resampling to ensure both methods see the same data:

``` r
learner = lrn("regr.ranger")
measure = msr("regr.mse")
resampling = rsmp("holdout")
resampling$instantiate(task)
```

## Marginal SAGE

Marginal SAGE marginalizes features independently by averaging
predictions over a reference dataset. This is the standard SAGE
implementation described in the original paper.

``` r
# Create Marginal SAGE instance
marginal_sage = MarginalSAGE$new(
    task = task,
    learner = learner,
    measure = measure,
    resampling = resampling,
    n_permutations = 15L,
    n_samples = 100L
)

# Compute SAGE values
marginal_sage$compute(batch_size = 5000L)
```

Let’s visualize the results:

![](sage-methods_files/figure-html/marginal-sage-plot-1.png)

We can also keep track of the SAGE value approximation across
permutations:

``` r
marginal_sage$plot_convergence()
```

![](sage-methods_files/figure-html/convergance-marginal-1.png)

### Early Stopping Based on Convergence

SAGE supports early stopping to save computation time when the
importance values have converged. By default, early stopping is enabled
with `early_stopping = TRUE`. Convergence is detected by monitoring the
standard error (SE) of the SAGE value estimates in the first resampling
iteration.

**Convergence criterion**: SAGE normalizes the SE by the range of their
values (max - min) to make convergence detection scale-invariant across
different loss metrics. Convergence is detected when:

\\\max_j \left(\frac{SE_j}{\max_i(\text{SAGE}\_i) -
\min_i(\text{SAGE}\_i)}\right) \< \text{threshold}\\

The default threshold is `se_threshold = 0.01` (1%), meaning convergence
occurs when the relative SE is below 1% of the importance range for all
features.

You can customize convergence detection in `$compute()`:

``` r
# More strict convergence (requires more permutations)
sage$compute(early_stopping = TRUE, se_threshold = 0.005, min_permutations = 5L)

# Disable early stopping to always run all permutations
sage$compute(early_stopping = FALSE)
```

After computation, you can check convergence status:

``` r
marginal_sage$converged  # TRUE if converged early
marginal_sage$n_permutations_used  # Actual permutations used
```

If a resampling with multiple iterations (i.e., not holdout) is
supplied, the value of `n_permutations_used` will be set as the value
for `n_permutations` in all subsequent iterations to avoid some
computational overhead.

## Conditional SAGE

Conditional SAGE uses conditional sampling (via ARF by default) to
marginalize features while preserving dependencies between the remaining
features. This can provide different insights, especially when features
are correlated.

``` r
# Create Conditional SAGE instance using a conditional sampler
sampler_gaussian = ConditionalGaussianSampler$new(task)

conditional_sage = ConditionalSAGE$new(
    task = task,
    learner = learner,
    measure = measure,
    resampling = resampling,
    n_permutations = 15L,
    n_samples = 100L,
    sampler = sampler_gaussian
)

# Compute SAGE values
conditional_sage$compute(batch_size = 5000L)
```

Let’s visualize the conditional SAGE results:

![](sage-methods_files/figure-html/conditional-sage-plot-1.png)

``` r
conditional_sage$plot_convergence()
```

![](sage-methods_files/figure-html/convergance-conditional-1.png)

## Comparison of Methods

Let’s compare the two SAGE methods side by side:

![](sage-methods_files/figure-html/comparison-1.png)

### Interpretation

## Comparison with PFI and CFI

For reference, let’s compare SAGE methods with the analogous PFI and CFI
methods on the same data:

``` r
# Quick PFI and CFI comparison for context
pfi = PFI$new(task, learner, measure)
#> ℹ No <Resampling> provided
#> Using `resampling = rsmp("holdout")` with default `ratio = 0.67`.
cfi = CFI$new(task, learner, measure, sampler = sampler_gaussian)
#> ℹ No <Resampling> provided
#> Using `resampling = rsmp("holdout")` with default `ratio = 0.67`.

pfi$compute()
cfi$compute()
pfi_results = pfi$importance()
cfi_results = cfi$importance()

# Create comparison data frame
method_comparison = data.frame(
    feature = rep(c("x1", "x2", "x3", "x4"), 4),
    importance = c(
        pfi_results$importance,
        cfi_results$importance,
        marginal_results$importance,
        conditional_results$importance
    ),
    method = rep(c("PFI", "CFI", "Marginal SAGE", "Conditional SAGE"), each = 4),
    approach = rep(c("Marginal", "Conditional", "Marginal", "Conditional"), each = 4)
)

# Create comparison plot
ggplot(method_comparison, aes(x = feature, y = importance, fill = method)) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_fill_manual(
        values = c(
            "PFI" = "lightblue",
            "CFI" = "blue",
            "Marginal SAGE" = "lightcoral",
            "Conditional SAGE" = "darkred"
        )
    ) +
    labs(
        title = "Comparison: PFI/CFI vs Marginal/Conditional SAGE",
        subtitle = "Both pairs show similar patterns: marginal methods inflate correlated feature importance",
        x = "Features",
        y = "Importance Value",
        fill = "Method"
    ) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](sage-methods_files/figure-html/pfi-cfi-comparison-1.png)

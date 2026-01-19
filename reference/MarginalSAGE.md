# Marginal SAGE

[SAGE](https://mlr-org.github.io/xplainfi/reference/SAGE.md) with
marginal sampling (features are marginalized independently). This is the
standard SAGE implementation.

## See also

[ConditionalSAGE](https://mlr-org.github.io/xplainfi/reference/ConditionalSAGE.md)

## Super classes

[`xplainfi::FeatureImportanceMethod`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.md)
-\>
[`xplainfi::SAGE`](https://mlr-org.github.io/xplainfi/reference/SAGE.md)
-\> `MarginalSAGE`

## Methods

### Public methods

- [`MarginalSAGE$new()`](#method-MarginalSAGE-new)

- [`MarginalSAGE$clone()`](#method-MarginalSAGE-clone)

Inherited methods

- [`xplainfi::FeatureImportanceMethod$importance()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-importance)
- [`xplainfi::FeatureImportanceMethod$obs_loss()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-obs_loss)
- [`xplainfi::FeatureImportanceMethod$print()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-print)
- [`xplainfi::FeatureImportanceMethod$reset()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-reset)
- [`xplainfi::FeatureImportanceMethod$scores()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-scores)
- [`xplainfi::SAGE$compute()`](https://mlr-org.github.io/xplainfi/reference/SAGE.html#method-compute)
- [`xplainfi::SAGE$plot_convergence()`](https://mlr-org.github.io/xplainfi/reference/SAGE.html#method-plot_convergence)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of the MarginalSAGE class.

#### Usage

    MarginalSAGE$new(
      task,
      learner,
      measure = NULL,
      resampling = NULL,
      features = NULL,
      n_permutations = 10L,
      batch_size = 5000L,
      n_samples = 100L,
      early_stopping = FALSE,
      se_threshold = 0.01,
      min_permutations = 10L,
      check_interval = 1L
    )

#### Arguments

- `task, learner, measure, resampling, features, n_permutations, batch_size, n_samples, early_stopping, se_threshold, min_permutations, check_interval`:

  Passed to
  [SAGE](https://mlr-org.github.io/xplainfi/reference/SAGE.md).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MarginalSAGE$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(mlr3)
task = tgen("friedman1")$generate(n = 100)
sage = MarginalSAGE$new(
  task = task,
  learner = lrn("regr.ranger", num.trees = 50),
  measure = msr("regr.mse"),
  n_permutations = 3L,
  n_samples = 20
)
#> ℹ No <Resampling> provided, using `resampling = rsmp("holdout", ratio = 2/3)`
#>   (test set size: 67)
sage$compute()
```

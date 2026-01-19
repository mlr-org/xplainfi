# Conditional SAGE

[SAGE](https://mlr-org.github.io/xplainfi/reference/SAGE.md) with
conditional sampling (features are "marginalized" conditionally). Uses
[ConditionalARFSampler](https://mlr-org.github.io/xplainfi/reference/ConditionalARFSampler.md)
as default
[ConditionalSampler](https://mlr-org.github.io/xplainfi/reference/ConditionalSampler.md).

## See also

[MarginalSAGE](https://mlr-org.github.io/xplainfi/reference/MarginalSAGE.md)

## Super classes

[`xplainfi::FeatureImportanceMethod`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.md)
-\>
[`xplainfi::SAGE`](https://mlr-org.github.io/xplainfi/reference/SAGE.md)
-\> `ConditionalSAGE`

## Public fields

- `sampler`:

  ([ConditionalSampler](https://mlr-org.github.io/xplainfi/reference/ConditionalSampler.md))
  Sampler for conditional marginalization.

## Methods

### Public methods

- [`ConditionalSAGE$new()`](#method-ConditionalSAGE-new)

- [`ConditionalSAGE$clone()`](#method-ConditionalSAGE-clone)

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

Creates a new instance of the ConditionalSAGE class.

#### Usage

    ConditionalSAGE$new(
      task,
      learner,
      measure = NULL,
      resampling = NULL,
      features = NULL,
      n_permutations = 10L,
      sampler = NULL,
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

- `sampler`:

  ([ConditionalSampler](https://mlr-org.github.io/xplainfi/reference/ConditionalSampler.md))
  Optional custom sampler. Defaults to
  [ConditionalARFSampler](https://mlr-org.github.io/xplainfi/reference/ConditionalARFSampler.md).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ConditionalSAGE$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(mlr3)
task = tgen("friedman1")$generate(n = 100)

if (FALSE) { # \dontrun{
# Using default ConditionalARFSampler
sage = ConditionalSAGE$new(
  task = task,
  learner = lrn("regr.ranger", num.trees = 50),
  measure = msr("regr.mse"),
  n_permutations = 3L,
  n_samples = 20
)
sage$compute()
} # }
# \donttest{
# For alternative conditional samplers:
custom_sampler = ConditionalGaussianSampler$new(
  task = task
)
sage_custom = ConditionalSAGE$new(
  task = task,
  learner = lrn("regr.ranger", num.trees = 50),
  measure = msr("regr.mse"),
  n_permutations = 5L,
  n_samples = 20,
  sampler = custom_sampler
)
#> ℹ No <Resampling> provided, using `resampling = rsmp("holdout", ratio = 2/3)`
#>   (test set size: 67)
sage_custom$compute()
# }
```

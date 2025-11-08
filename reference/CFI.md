# Conditional Feature Importance

Implementation of CFI using modular sampling approach

## References

Blesch K, Koenen N, Kapar J, Golchian P, Burk L, Loecher M, Wright M
(2025). “Conditional Feature Importance with Generative Modeling Using
Adversarial Random Forests.” *Proceedings of the AAAI Conference on
Artificial Intelligence*, **39**(15), 15596–15604.
[doi:10.1609/aaai.v39i15.33712](https://doi.org/10.1609/aaai.v39i15.33712)
.

## Super classes

[`xplainfi::FeatureImportanceMethod`](https://jemus42.github.io/xplainfi/reference/FeatureImportanceMethod.md)
-\>
[`xplainfi::PerturbationImportance`](https://jemus42.github.io/xplainfi/reference/PerturbationImportance.md)
-\> `CFI`

## Methods

### Public methods

- [`CFI$new()`](#method-CFI-new)

- [`CFI$compute()`](#method-CFI-compute)

- [`CFI$clone()`](#method-CFI-clone)

Inherited methods

- [`xplainfi::FeatureImportanceMethod$importance()`](https://jemus42.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-importance)
- [`xplainfi::FeatureImportanceMethod$obs_loss()`](https://jemus42.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-obs_loss)
- [`xplainfi::FeatureImportanceMethod$print()`](https://jemus42.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-print)
- [`xplainfi::FeatureImportanceMethod$reset()`](https://jemus42.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-reset)
- [`xplainfi::FeatureImportanceMethod$scores()`](https://jemus42.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-scores)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of the CFI class

#### Usage

    CFI$new(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      groups = NULL,
      relation = "difference",
      n_repeats = 1L,
      batch_size = NULL,
      sampler = NULL
    )

#### Arguments

- `task, learner, measure, resampling, features, groups, relation, n_repeats, batch_size`:

  Passed to
  [PerturbationImportance](https://jemus42.github.io/xplainfi/reference/PerturbationImportance.md).

- `sampler`:

  ([ConditionalSampler](https://jemus42.github.io/xplainfi/reference/ConditionalSampler.md))
  Optional custom sampler. Defaults to instantiating
  `ConditionalARFSampler` internally with default parameters.

------------------------------------------------------------------------

### Method `compute()`

Compute CFI scores

#### Usage

    CFI$compute(
      n_repeats = NULL,
      batch_size = NULL,
      store_models = TRUE,
      store_backends = TRUE
    )

#### Arguments

- `n_repeats`:

  (integer(1)) Number of permutation iterations. If `NULL`, uses stored
  value.

- `batch_size`:

  (`integer(1)` \| `NULL`: `NULL`) Maximum number of rows to predict at
  once. If `NULL`, uses stored value.

- `store_models, store_backends`:

  (`logical(1)`: `TRUE`) Whether to store fitted models / data backends,
  passed to
  [mlr3::resample](https://mlr3.mlr-org.com/reference/resample.html)
  internally for the initial fit of the learner. This may be required
  for certain measures and is recommended to leave enabled unless really
  necessary.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CFI$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(mlr3)
task = tgen("2dnormals")$generate(n = 100)

# Using default ConditionalARFSampler
cfi = CFI$new(
  task = task,
  learner = lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
  measure = msr("classif.ce")
)
#> ℹ No `sampler` provided, using <ConditionalARFSampler> with default settings.
#> ℹ No <Resampling> provided
#> Using `resampling = rsmp("holdout")` with default `ratio = 0.67`.
cfi$compute()
cfi$importance()
#> Key: <feature>
#>    feature importance
#>     <char>      <num>
#> 1:      x1 0.09090909
#> 2:      x2 0.09090909
if (FALSE) { # \dontrun{
# For more control over conditional sampling:
custom_sampler = ConditionalARFSampler$new(
  task = task,
  finite_bounds = "local" # can improve sampling behavior
)
cfi_custom = CFI$new(
  task = task,
  learner = lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
  measure = msr("classif.ce"),
  sampler = custom_sampler
)
cfi_custom$compute()
cfi_custom$importance()
} # }
```

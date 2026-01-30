# Relative Feature Importance

RFI generalizes CFI and PFI with arbitrary conditioning sets and
samplers.

## References

König G, Molnar C, Bischl B, Grosse-Wentrup M (2021). “Relative Feature
Importance.” In *2020 25th International Conference on Pattern
Recognition (ICPR)*, 9318–9325.
[doi:10.1109/ICPR48806.2021.9413090](https://doi.org/10.1109/ICPR48806.2021.9413090)
.

## Super classes

[`xplainfi::FeatureImportanceMethod`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.md)
-\>
[`xplainfi::PerturbationImportance`](https://mlr-org.github.io/xplainfi/reference/PerturbationImportance.md)
-\> `RFI`

## Methods

### Public methods

- [`RFI$new()`](#method-RFI-new)

- [`RFI$compute()`](#method-RFI-compute)

- [`RFI$clone()`](#method-RFI-clone)

Inherited methods

- [`xplainfi::FeatureImportanceMethod$obs_loss()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-obs_loss)
- [`xplainfi::FeatureImportanceMethod$print()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-print)
- [`xplainfi::FeatureImportanceMethod$reset()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-reset)
- [`xplainfi::FeatureImportanceMethod$scores()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-scores)
- [`xplainfi::PerturbationImportance$importance()`](https://mlr-org.github.io/xplainfi/reference/PerturbationImportance.html#method-importance)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of the RFI class

#### Usage

    RFI$new(
      task,
      learner,
      measure = NULL,
      resampling = NULL,
      features = NULL,
      groups = NULL,
      conditioning_set = NULL,
      relation = "difference",
      n_repeats = 30L,
      batch_size = NULL,
      sampler = NULL
    )

#### Arguments

- `task, learner, measure, resampling, features, groups, relation, n_repeats, batch_size`:

  Passed to
  [PerturbationImportance](https://mlr-org.github.io/xplainfi/reference/PerturbationImportance.md).

- `conditioning_set`:

  ([`character()`](https://rdrr.io/r/base/character.html)) Set of
  features to condition on. Can be overridden in `$compute()`. Default
  (`character(0)`) is equivalent to `PFI`. In `CFI`, this would be set
  to all features except that of interest.

- `sampler`:

  ([ConditionalSampler](https://mlr-org.github.io/xplainfi/reference/ConditionalSampler.md))
  Optional custom sampler. Defaults to `ConditionalARFSampler`.

------------------------------------------------------------------------

### Method `compute()`

Compute RFI scores

#### Usage

    RFI$compute(
      conditioning_set = NULL,
      n_repeats = NULL,
      batch_size = NULL,
      store_models = TRUE,
      store_backends = TRUE
    )

#### Arguments

- `conditioning_set`:

  ([`character()`](https://rdrr.io/r/base/character.html)) Set of
  features to condition on. If `NULL`, uses the stored parameter value.

- `n_repeats`:

  (`integer(1)`) Number of permutation iterations. If `NULL`, uses
  stored value.

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

    RFI$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(mlr3)
task = tgen("friedman1")$generate(n = 200)
rfi = RFI$new(
  task = task,
  learner = lrn("regr.ranger", num.trees = 50),
  measure = msr("regr.mse"),
  conditioning_set = c("important1")
)
#> ℹ No <ConditionalSampler> provided, using <ConditionalARFSampler> with default settings.
#> ℹ No <Resampling> provided, using `resampling = rsmp("holdout", ratio = 2/3)`
#>   (test set size: 67)
rfi$compute()
rfi$importance()
#> Key: <feature>
#>          feature  importance
#>           <char>       <num>
#>  1:   important1  0.00000000
#>  2:   important2  3.98572044
#>  3:   important3  0.70127228
#>  4:   important4  8.68608941
#>  5:   important5  1.66898162
#>  6: unimportant1  0.07271123
#>  7: unimportant2 -0.13309431
#>  8: unimportant3  0.04559203
#>  9: unimportant4  0.18750985
#> 10: unimportant5 -0.07063373
```

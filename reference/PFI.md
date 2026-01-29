# Permutation Feature Importance

Implementation of Permutation Feature Importance (PFI) using modular
sampling approach. PFI measures the importance of a feature by
calculating the increase in model error when the feature's values are
randomly permuted, breaking the relationship between the feature and the
target variable.

## Details

Permutation Feature Importance was originally introduced by Breiman
(2001) as part of the Random Forest algorithm. The method works by:

1.  Computing baseline model performance on the original dataset

2.  For each feature, randomly permuting its values while keeping other
    features unchanged

3.  Computing model performance on the permuted dataset

4.  Calculating importance as the difference (or ratio) between permuted
    and original performance

## References

Breiman L (2001). “Random Forests.” *Machine Learning*, **45**(1), 5–32.
[doi:10.1023/A:1010933404324](https://doi.org/10.1023/A%3A1010933404324)
. Fisher A, Rudin C, Dominici F (2019). “All Models Are Wrong, but Many
Are Useful: Learning a Variable's Importance by Studying an Entire Class
of Prediction Models Simultaneously.” *Journal of Machine Learning
Research*, **20**, 177.
<https://pmc.ncbi.nlm.nih.gov/articles/PMC8323609/>. Strobl C,
Boulesteix A, Kneib T, Augustin T, Zeileis A (2008). “Conditional
Variable Importance for Random Forests.” *BMC Bioinformatics*, **9**(1),
307.
[doi:10.1186/1471-2105-9-307](https://doi.org/10.1186/1471-2105-9-307) .

## Super classes

[`xplainfi::FeatureImportanceMethod`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.md)
-\>
[`xplainfi::PerturbationImportance`](https://mlr-org.github.io/xplainfi/reference/PerturbationImportance.md)
-\> `PFI`

## Methods

### Public methods

- [`PFI$new()`](#method-PFI-new)

- [`PFI$compute()`](#method-PFI-compute)

- [`PFI$clone()`](#method-PFI-clone)

Inherited methods

- [`xplainfi::FeatureImportanceMethod$obs_loss()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-obs_loss)
- [`xplainfi::FeatureImportanceMethod$print()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-print)
- [`xplainfi::FeatureImportanceMethod$reset()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-reset)
- [`xplainfi::FeatureImportanceMethod$scores()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-scores)
- [`xplainfi::PerturbationImportance$importance()`](https://mlr-org.github.io/xplainfi/reference/PerturbationImportance.html#method-importance)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of the PFI class

#### Usage

    PFI$new(
      task,
      learner,
      measure = NULL,
      resampling = NULL,
      features = NULL,
      groups = NULL,
      relation = "difference",
      n_repeats = 1L,
      batch_size = NULL
    )

#### Arguments

- `task, learner, measure, resampling, features, groups, relation, n_repeats, batch_size`:

  Passed to
  [PerturbationImportance](https://mlr-org.github.io/xplainfi/reference/PerturbationImportance.md)

------------------------------------------------------------------------

### Method `compute()`

Compute PFI scores

#### Usage

    PFI$compute(
      n_repeats = NULL,
      batch_size = NULL,
      store_models = TRUE,
      store_backends = TRUE
    )

#### Arguments

- `n_repeats`:

  (`integer(1)`; `NULL`) Number of permutation iterations. If `NULL`,
  uses stored value.

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

    PFI$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(mlr3)
library(mlr3learners)

task <- sim_dgp_correlated(n = 500)

pfi <- PFI$new(
  task = task,
  learner = lrn("regr.ranger", num.trees = 10),
  measure = msr("regr.mse")
)
#> ℹ No <Resampling> provided, using `resampling = rsmp("holdout", ratio = 2/3)`
#>   (test set size: 167)
pfi$compute()
pfi$importance()
#> Key: <feature>
#>    feature   importance
#>     <char>        <num>
#> 1:      x1  3.527932276
#> 2:      x2  0.924809854
#> 3:      x3  1.313079583
#> 4:      x4 -0.001277584
```

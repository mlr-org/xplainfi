# Marginal Permutation Sampler

Implements marginal permutation-based sampling for Permutation Feature
Importance (PFI). Each specified feature is randomly shuffled (permuted)
independently, breaking the relationship between the feature and the
target as well as between rows.

## Details

The permutation sampler randomly shuffles feature values across
observations:

- Each feature is permuted **independently** within its column

- The association between feature values and target values is broken

- The association between feature values **across rows** is broken

- The marginal distribution of each feature is preserved

**Important distinction from SAGE's "marginal" approach:**

- `MarginalPermutationSampler`: Shuffles features independently,
  breaking row structure

- `MarginalSAGE`: Uses reference data but keeps rows intact (features in
  coalition stay together)

This is the classic approach used in Permutation Feature Importance
(PFI) and assumes features are independent.

## Super classes

[`xplainfi::FeatureSampler`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.md)
-\>
[`xplainfi::MarginalSampler`](https://mlr-org.github.io/xplainfi/reference/MarginalSampler.md)
-\> `MarginalPermutationSampler`

## Methods

### Public methods

- [`MarginalPermutationSampler$new()`](#method-MarginalPermutationSampler-new)

- [`MarginalPermutationSampler$clone()`](#method-MarginalPermutationSampler-clone)

Inherited methods

- [`xplainfi::FeatureSampler$print()`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.html#method-print)
- [`xplainfi::MarginalSampler$sample()`](https://mlr-org.github.io/xplainfi/reference/MarginalSampler.html#method-sample)
- [`xplainfi::MarginalSampler$sample_newdata()`](https://mlr-org.github.io/xplainfi/reference/MarginalSampler.html#method-sample_newdata)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of the MarginalPermutationSampler class.

#### Usage

    MarginalPermutationSampler$new(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)) Task to
  sample from.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MarginalPermutationSampler$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(mlr3)
task = tgen("2dnormals")$generate(n = 10)
task$data()
#>          y          x1          x2
#>     <fctr>       <num>       <num>
#>  1:      B -1.62336621 -2.52744877
#>  2:      B -3.20834127 -2.33654963
#>  3:      B -2.07210337 -1.95102128
#>  4:      B  0.62447655 -0.09529361
#>  5:      A  1.11332585  1.47213262
#>  6:      B -2.24618392 -2.79422346
#>  7:      B -2.09712405  0.95903606
#>  8:      A  0.06582087 -0.46066759
#>  9:      A  3.01367029  1.80727980
#> 10:      B -2.61284600 -0.40300231
sampler = MarginalPermutationSampler$new(task)

# Sample using row_ids from stored task
sampler$sample("x1")
#>          y          x1          x2
#>     <fctr>       <num>       <num>
#>  1:      B -2.61284600 -2.52744877
#>  2:      B -3.20834127 -2.33654963
#>  3:      B -2.09712405 -1.95102128
#>  4:      B  1.11332585 -0.09529361
#>  5:      A -2.24618392  1.47213262
#>  6:      B -1.62336621 -2.79422346
#>  7:      B  0.06582087  0.95903606
#>  8:      A  0.62447655 -0.46066759
#>  9:      A -2.07210337  1.80727980
#> 10:      B  3.01367029 -0.40300231

# Or use external data
data = task$data()
sampler$sample_newdata("x1", newdata = data)
#>          y          x1          x2
#>     <fctr>       <num>       <num>
#>  1:      B -2.07210337 -2.52744877
#>  2:      B  1.11332585 -2.33654963
#>  3:      B -2.61284600 -1.95102128
#>  4:      B -1.62336621 -0.09529361
#>  5:      A -3.20834127  1.47213262
#>  6:      B -2.09712405 -2.79422346
#>  7:      B -2.24618392  0.95903606
#>  8:      A  3.01367029 -0.46066759
#>  9:      A  0.06582087  1.80727980
#> 10:      B  0.62447655 -0.40300231
```

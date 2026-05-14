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

[`FeatureSampler`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.md)
-\>
[`MarginalSampler`](https://mlr-org.github.io/xplainfi/reference/MarginalSampler.md)
-\> `MarginalPermutationSampler`

## Methods

### Public methods

- [`MarginalPermutationSampler$new()`](#method-MarginalPermutationSampler-initialize)

- [`MarginalPermutationSampler$clone()`](#method-MarginalPermutationSampler-clone)

Inherited methods

- [`FeatureSampler$print()`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.html#method-print)
- [`MarginalSampler$sample()`](https://mlr-org.github.io/xplainfi/reference/MarginalSampler.html#method-sample)
- [`MarginalSampler$sample_newdata()`](https://mlr-org.github.io/xplainfi/reference/MarginalSampler.html#method-sample_newdata)

------------------------------------------------------------------------

### `MarginalPermutationSampler$new()`

Creates a new instance of the MarginalPermutationSampler class.

#### Usage

    MarginalPermutationSampler$new(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)) Task to
  sample from.

------------------------------------------------------------------------

### `MarginalPermutationSampler$clone()`

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
#>          y          x1         x2
#>     <fctr>       <num>      <num>
#>  1:      A  1.19626930  0.9726073
#>  2:      B -0.49604861  0.3189972
#>  3:      A -0.12100031  2.2561295
#>  4:      A -0.06132352  0.3388700
#>  5:      B -0.56410149 -1.3176832
#>  6:      B -1.37048905 -2.5669256
#>  7:      B -0.95759051 -1.1891020
#>  8:      A  1.81476223  0.3741859
#>  9:      B -1.33160410  0.2405210
#> 10:      A  3.56562248  0.2692606
sampler = MarginalPermutationSampler$new(task)

# Sample using row_ids from stored task
sampler$sample("x1")
#>          y          x1         x2
#>     <fctr>       <num>      <num>
#>  1:      A -1.33160410  0.9726073
#>  2:      B  1.81476223  0.3189972
#>  3:      A -0.56410149  2.2561295
#>  4:      A -0.12100031  0.3388700
#>  5:      B  3.56562248 -1.3176832
#>  6:      B -0.95759051 -2.5669256
#>  7:      B -1.37048905 -1.1891020
#>  8:      A -0.49604861  0.3741859
#>  9:      B  1.19626930  0.2405210
#> 10:      A -0.06132352  0.2692606

# Or use external data
data = task$data()
sampler$sample_newdata("x1", newdata = data)
#>          y          x1         x2
#>     <fctr>       <num>      <num>
#>  1:      A -0.49604861  0.9726073
#>  2:      B -0.95759051  0.3189972
#>  3:      A  1.81476223  2.2561295
#>  4:      A -0.12100031  0.3388700
#>  5:      B -0.56410149 -1.3176832
#>  6:      B  1.19626930 -2.5669256
#>  7:      B  3.56562248 -1.1891020
#>  8:      A -0.06132352  0.3741859
#>  9:      B -1.37048905  0.2405210
#> 10:      A -1.33160410  0.2692606
```

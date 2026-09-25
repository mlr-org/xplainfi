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
#>          y         x1          x2
#>     <fctr>      <num>       <num>
#>  1:      B -2.0898435 -1.43939963
#>  2:      A -1.0967134  3.55726688
#>  3:      B -1.6661733 -0.67184012
#>  4:      A  0.9398231 -0.05575113
#>  5:      B -0.7376130 -1.58338434
#>  6:      A  2.5271536  1.44435025
#>  7:      B -3.9122489 -1.06684038
#>  8:      B -1.9505794  0.45315517
#>  9:      A  1.1286319  0.83148956
#> 10:      A  1.9263453  1.12759719
sampler = MarginalPermutationSampler$new(task)

# Sample using row_ids from stored task
sampler$sample("x1")
#>          y         x1          x2
#>     <fctr>      <num>       <num>
#>  1:      B -1.0967134 -1.43939963
#>  2:      A  2.5271536  3.55726688
#>  3:      B -2.0898435 -0.67184012
#>  4:      A -1.9505794 -0.05575113
#>  5:      B  1.1286319 -1.58338434
#>  6:      A -3.9122489  1.44435025
#>  7:      B -0.7376130 -1.06684038
#>  8:      B  0.9398231  0.45315517
#>  9:      A  1.9263453  0.83148956
#> 10:      A -1.6661733  1.12759719

# Or use external data
data = task$data()
sampler$sample_newdata("x1", newdata = data)
#>          y         x1          x2
#>     <fctr>      <num>       <num>
#>  1:      B  1.1286319 -1.43939963
#>  2:      A  0.9398231  3.55726688
#>  3:      B -1.9505794 -0.67184012
#>  4:      A  1.9263453 -0.05575113
#>  5:      B -3.9122489 -1.58338434
#>  6:      A -0.7376130  1.44435025
#>  7:      B -2.0898435 -1.06684038
#>  8:      B -1.0967134  0.45315517
#>  9:      A  2.5271536  0.83148956
#> 10:      A -1.6661733  1.12759719
```

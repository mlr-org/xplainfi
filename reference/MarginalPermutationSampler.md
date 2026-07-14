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
#>          y         x1         x2
#>     <fctr>      <num>      <num>
#>  1:      A  2.3233233  0.3086548
#>  2:      A  0.6763551 -2.1299770
#>  3:      B -0.7174068 -0.4695579
#>  4:      A  0.8901893  0.8300322
#>  5:      B -2.2574025 -0.3868116
#>  6:      A  0.2830972  0.9050875
#>  7:      A -1.5382508  0.7703612
#>  8:      A  0.5948388  0.5815871
#>  9:      A  0.9719462  1.1955671
#> 10:      B -1.0768650 -0.7354938
sampler = MarginalPermutationSampler$new(task)

# Sample using row_ids from stored task
sampler$sample("x1")
#>          y         x1         x2
#>     <fctr>      <num>      <num>
#>  1:      A -1.0768650  0.3086548
#>  2:      A -2.2574025 -2.1299770
#>  3:      B  0.5948388 -0.4695579
#>  4:      A -0.7174068  0.8300322
#>  5:      B  0.9719462 -0.3868116
#>  6:      A -1.5382508  0.9050875
#>  7:      A  0.2830972  0.7703612
#>  8:      A  2.3233233  0.5815871
#>  9:      A  0.8901893  1.1955671
#> 10:      B  0.6763551 -0.7354938

# Or use external data
data = task$data()
sampler$sample_newdata("x1", newdata = data)
#>          y         x1         x2
#>     <fctr>      <num>      <num>
#>  1:      A -1.5382508  0.3086548
#>  2:      A  0.2830972 -2.1299770
#>  3:      B -2.2574025 -0.4695579
#>  4:      A -1.0768650  0.8300322
#>  5:      B  0.5948388 -0.3868116
#>  6:      A  0.6763551  0.9050875
#>  7:      A  0.9719462  0.7703612
#>  8:      A  2.3233233  0.5815871
#>  9:      A  0.8901893  1.1955671
#> 10:      B -0.7174068 -0.7354938
```

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
#>          y         x1         x2
#>     <fctr>      <num>      <num>
#>  1:      A  0.9350420 -0.3295782
#>  2:      A -0.6893674  0.5985551
#>  3:      B  0.1463412  0.6616391
#>  4:      B -1.9948937 -0.9802949
#>  5:      B -2.4267797 -2.2721073
#>  6:      A  0.2440375  1.4316231
#>  7:      B -1.4203440 -0.9403282
#>  8:      B -1.0117049 -0.2062054
#>  9:      B -1.3771618 -1.1197334
#> 10:      A  1.0632517  0.7192422
sampler = MarginalPermutationSampler$new(task)

# Sample using row_ids from stored task
sampler$sample("x1")
#>          y         x1         x2
#>     <fctr>      <num>      <num>
#>  1:      A -1.3771618 -0.3295782
#>  2:      A -1.0117049  0.5985551
#>  3:      B -1.4203440  0.6616391
#>  4:      B -2.4267797 -0.9802949
#>  5:      B -0.6893674 -2.2721073
#>  6:      A  1.0632517  1.4316231
#>  7:      B -1.9948937 -0.9403282
#>  8:      B  0.9350420 -0.2062054
#>  9:      B  0.2440375 -1.1197334
#> 10:      A  0.1463412  0.7192422

# Or use external data
data = task$data()
sampler$sample_newdata("x1", newdata = data)
#>          y         x1         x2
#>     <fctr>      <num>      <num>
#>  1:      A -2.4267797 -0.3295782
#>  2:      A -1.4203440  0.5985551
#>  3:      B -1.3771618  0.6616391
#>  4:      B -1.0117049 -0.9802949
#>  5:      B -1.9948937 -2.2721073
#>  6:      A  1.0632517  1.4316231
#>  7:      B  0.9350420 -0.9403282
#>  8:      B  0.2440375 -0.2062054
#>  9:      B  0.1463412 -1.1197334
#> 10:      A -0.6893674  0.7192422
```

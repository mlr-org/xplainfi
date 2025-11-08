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

[`xplainfi::FeatureSampler`](https://jemus42.github.io/xplainfi/reference/FeatureSampler.md)
-\>
[`xplainfi::MarginalSampler`](https://jemus42.github.io/xplainfi/reference/MarginalSampler.md)
-\> `MarginalPermutationSampler`

## Methods

### Public methods

- [`MarginalPermutationSampler$new()`](#method-MarginalPermutationSampler-new)

- [`MarginalPermutationSampler$clone()`](#method-MarginalPermutationSampler-clone)

Inherited methods

- [`xplainfi::FeatureSampler$print()`](https://jemus42.github.io/xplainfi/reference/FeatureSampler.html#method-print)
- [`xplainfi::MarginalSampler$sample()`](https://jemus42.github.io/xplainfi/reference/MarginalSampler.html#method-sample)
- [`xplainfi::MarginalSampler$sample_newdata()`](https://jemus42.github.io/xplainfi/reference/MarginalSampler.html#method-sample_newdata)

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
#>  1:      B -1.7331763 -3.2349193
#>  2:      A  2.6408984  1.3567425
#>  3:      B -0.8446155 -0.7495950
#>  4:      B -1.3301100 -1.4135048
#>  5:      B -0.5485220 -0.5963963
#>  6:      B -1.6467495 -1.0429258
#>  7:      B -1.6185175 -1.5749875
#>  8:      A  1.8603482  0.9467555
#>  9:      A  0.9774552  0.9619044
#> 10:      A  0.6314264  1.6367233
sampler = MarginalPermutationSampler$new(task)

# Sample using row_ids from stored task
sampler$sample("x1")
#>          y         x1         x2
#>     <fctr>      <num>      <num>
#>  1:      B -1.6185175 -3.2349193
#>  2:      A  1.8603482  1.3567425
#>  3:      B  2.6408984 -0.7495950
#>  4:      B -1.3301100 -1.4135048
#>  5:      B  0.6314264 -0.5963963
#>  6:      B -0.5485220 -1.0429258
#>  7:      B -1.6467495 -1.5749875
#>  8:      A  0.9774552  0.9467555
#>  9:      A -1.7331763  0.9619044
#> 10:      A -0.8446155  1.6367233

# Or use external data
data = task$data()
sampler$sample_newdata("x1", newdata = data)
#>          y         x1         x2
#>     <fctr>      <num>      <num>
#>  1:      B -0.8446155 -3.2349193
#>  2:      A  1.8603482  1.3567425
#>  3:      B -1.6185175 -0.7495950
#>  4:      B -0.5485220 -1.4135048
#>  5:      B  0.6314264 -0.5963963
#>  6:      B  0.9774552 -1.0429258
#>  7:      B -1.3301100 -1.5749875
#>  8:      A -1.7331763  0.9467555
#>  9:      A -1.6467495  0.9619044
#> 10:      A  2.6408984  1.6367233
```

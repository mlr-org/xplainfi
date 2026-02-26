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
#>          y         x1          x2
#>     <fctr>      <num>       <num>
#>  1:      A  1.7773348  2.26175298
#>  2:      B -2.0069035  0.05488779
#>  3:      A  1.4039615  0.91474000
#>  4:      A  1.6617618  2.08743917
#>  5:      A  1.2320348  2.61978528
#>  6:      A  1.9551732 -0.87643544
#>  7:      B  0.3575475 -1.20454686
#>  8:      B -0.2497584 -1.51014735
#>  9:      B -0.2405774 -1.17051133
#> 10:      A -0.4771260  0.31415101
sampler = MarginalPermutationSampler$new(task)

# Sample using row_ids from stored task
sampler$sample("x1")
#>          y         x1          x2
#>     <fctr>      <num>       <num>
#>  1:      A  1.6617618  2.26175298
#>  2:      B  1.4039615  0.05488779
#>  3:      A -0.4771260  0.91474000
#>  4:      A  1.7773348  2.08743917
#>  5:      A  0.3575475  2.61978528
#>  6:      A  1.9551732 -0.87643544
#>  7:      B -0.2497584 -1.20454686
#>  8:      B  1.2320348 -1.51014735
#>  9:      B -2.0069035 -1.17051133
#> 10:      A -0.2405774  0.31415101

# Or use external data
data = task$data()
sampler$sample_newdata("x1", newdata = data)
#>          y         x1          x2
#>     <fctr>      <num>       <num>
#>  1:      A  1.7773348  2.26175298
#>  2:      B  1.4039615  0.05488779
#>  3:      A  0.3575475  0.91474000
#>  4:      A -0.2497584  2.08743917
#>  5:      A -0.4771260  2.61978528
#>  6:      A  1.9551732 -0.87643544
#>  7:      B -0.2405774 -1.20454686
#>  8:      B  1.6617618 -1.51014735
#>  9:      B -2.0069035 -1.17051133
#> 10:      A  1.2320348  0.31415101
```

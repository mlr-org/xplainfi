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
#>  1:      A  0.7982296  2.2068573
#>  2:      B -1.2354196 -0.5875541
#>  3:      A  0.1509640  1.3277959
#>  4:      B -1.3994408 -0.5458946
#>  5:      B -1.2539472 -1.5003423
#>  6:      A  2.1400989  0.1583906
#>  7:      B  0.4780871 -2.3614850
#>  8:      B -1.7427055 -0.9942252
#>  9:      B -0.8380797 -1.6470720
#> 10:      A  1.5556170  1.3060192
sampler = MarginalPermutationSampler$new(task)

# Sample using row_ids from stored task
sampler$sample("x1")
#>          y         x1         x2
#>     <fctr>      <num>      <num>
#>  1:      A -1.7427055  2.2068573
#>  2:      B  0.7982296 -0.5875541
#>  3:      A -1.3994408  1.3277959
#>  4:      B -0.8380797 -0.5458946
#>  5:      B -1.2539472 -1.5003423
#>  6:      A -1.2354196  0.1583906
#>  7:      B  2.1400989 -2.3614850
#>  8:      B  0.1509640 -0.9942252
#>  9:      B  0.4780871 -1.6470720
#> 10:      A  1.5556170  1.3060192

# Or use external data
data = task$data()
sampler$sample_newdata("x1", newdata = data)
#>          y         x1         x2
#>     <fctr>      <num>      <num>
#>  1:      A  0.1509640  2.2068573
#>  2:      B -0.8380797 -0.5875541
#>  3:      A  0.7982296  1.3277959
#>  4:      B  1.5556170 -0.5458946
#>  5:      B -1.7427055 -1.5003423
#>  6:      A -1.2354196  0.1583906
#>  7:      B  2.1400989 -2.3614850
#>  8:      B -1.2539472 -0.9942252
#>  9:      B -1.3994408 -1.6470720
#> 10:      A  0.4780871  1.3060192
```

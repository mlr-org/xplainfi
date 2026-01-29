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
#>          y          x1         x2
#>     <fctr>       <num>      <num>
#>  1:      B -0.62965001 -1.6683784
#>  2:      B -0.08001655 -3.8871235
#>  3:      B -1.84888994 -2.4336265
#>  4:      B -0.76591719 -1.4743742
#>  5:      A  1.14671304 -0.5613565
#>  6:      A -1.15062835 -0.7063118
#>  7:      B -0.06612565 -1.5379414
#>  8:      B -0.75505266 -1.7142646
#>  9:      A  0.97286802  2.1950540
#> 10:      B -0.65541353 -0.4925467
sampler = MarginalPermutationSampler$new(task)

# Sample using row_ids from stored task
sampler$sample("x1")
#>          y          x1         x2
#>     <fctr>       <num>      <num>
#>  1:      B -0.65541353 -1.6683784
#>  2:      B  1.14671304 -3.8871235
#>  3:      B -0.62965001 -2.4336265
#>  4:      B -0.06612565 -1.4743742
#>  5:      A -1.15062835 -0.5613565
#>  6:      A -0.08001655 -0.7063118
#>  7:      B -1.84888994 -1.5379414
#>  8:      B -0.75505266 -1.7142646
#>  9:      A  0.97286802  2.1950540
#> 10:      B -0.76591719 -0.4925467

# Or use external data
data = task$data()
sampler$sample_newdata("x1", newdata = data)
#>          y          x1         x2
#>     <fctr>       <num>      <num>
#>  1:      B -1.84888994 -1.6683784
#>  2:      B -0.62965001 -3.8871235
#>  3:      B -0.75505266 -2.4336265
#>  4:      B -0.76591719 -1.4743742
#>  5:      A -0.06612565 -0.5613565
#>  6:      A -0.08001655 -0.7063118
#>  7:      B  1.14671304 -1.5379414
#>  8:      B -1.15062835 -1.7142646
#>  9:      A -0.65541353  2.1950540
#> 10:      B  0.97286802 -0.4925467
```

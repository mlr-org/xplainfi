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
#>          y          x1          x2
#>     <fctr>       <num>       <num>
#>  1:      A  0.16480850  2.96706409
#>  2:      A  1.50560594 -0.35452371
#>  3:      A -1.54097692 -0.08810451
#>  4:      A -1.03368752  1.49119977
#>  5:      B -1.21721457 -0.88976437
#>  6:      A -0.09054171 -0.91980417
#>  7:      A  2.07455305 -0.15308410
#>  8:      B -1.21622913 -1.52479442
#>  9:      B -0.87526364 -1.09935464
#> 10:      B -2.08028369 -1.97371087
sampler = MarginalPermutationSampler$new(task)

# Sample using row_ids from stored task
sampler$sample("x1")
#>          y          x1          x2
#>     <fctr>       <num>       <num>
#>  1:      A -1.21721457  2.96706409
#>  2:      A  2.07455305 -0.35452371
#>  3:      A -1.03368752 -0.08810451
#>  4:      A -1.21622913  1.49119977
#>  5:      B -0.87526364 -0.88976437
#>  6:      A  0.16480850 -0.91980417
#>  7:      A -2.08028369 -0.15308410
#>  8:      B -0.09054171 -1.52479442
#>  9:      B -1.54097692 -1.09935464
#> 10:      B  1.50560594 -1.97371087

# Or use external data
data = task$data()
sampler$sample_newdata("x1", newdata = data)
#>          y          x1          x2
#>     <fctr>       <num>       <num>
#>  1:      A -1.21622913  2.96706409
#>  2:      A -2.08028369 -0.35452371
#>  3:      A  0.16480850 -0.08810451
#>  4:      A -0.87526364  1.49119977
#>  5:      B -1.03368752 -0.88976437
#>  6:      A  2.07455305 -0.91980417
#>  7:      A -0.09054171 -0.15308410
#>  8:      B  1.50560594 -1.52479442
#>  9:      B -1.21721457 -1.09935464
#> 10:      B -1.54097692 -1.97371087
```

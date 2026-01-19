# Gaussian Knockoff Conditional Sampler

A
[KnockoffSampler](https://mlr-org.github.io/xplainfi/reference/KnockoffSampler.md)
defaulting to second-order Gaussian knockoffs as created by
[knockoff::create.second_order](https://rdrr.io/pkg/knockoff/man/create.second_order.html).

## Details

This is equivalent to
[KnockoffSampler](https://mlr-org.github.io/xplainfi/reference/KnockoffSampler.md)
using the default `knockoff_fun`.

## References

Watson D, Wright M (2021). “Testing Conditional Independence in
Supervised Learning Algorithms.” *Machine Learning*, **110**(8),
2107–2129.
[doi:10.1007/s10994-021-06030-6](https://doi.org/10.1007/s10994-021-06030-6)
.

Blesch K, Watson D, Wright M (2023). “Conditional Feature Importance for
Mixed Data.” *AStA Advances in Statistical Analysis*, **108**(2),
259–278.
[doi:10.1007/s10182-023-00477-9](https://doi.org/10.1007/s10182-023-00477-9)
.

## Super classes

[`xplainfi::FeatureSampler`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.md)
-\>
[`xplainfi::KnockoffSampler`](https://mlr-org.github.io/xplainfi/reference/KnockoffSampler.md)
-\> `KnockoffGaussianSampler`

## Public fields

- `feature_types`:

  ([`character()`](https://rdrr.io/r/base/character.html)) Feature types
  supported by the sampler. Will be checked against the provided
  [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html) to ensure
  compatibility.

- `x_tilde`:

  Knockoff matrix

## Methods

### Public methods

- [`KnockoffGaussianSampler$new()`](#method-KnockoffGaussianSampler-new)

- [`KnockoffGaussianSampler$clone()`](#method-KnockoffGaussianSampler-clone)

Inherited methods

- [`xplainfi::FeatureSampler$print()`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.html#method-print)
- [`xplainfi::FeatureSampler$sample_newdata()`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.html#method-sample_newdata)
- [`xplainfi::KnockoffSampler$sample()`](https://mlr-org.github.io/xplainfi/reference/KnockoffSampler.html#method-sample)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance using Gaussian knockoffs via
[knockoff::create.second_order](https://rdrr.io/pkg/knockoff/man/create.second_order.html).

#### Usage

    KnockoffGaussianSampler$new(task, iters = 1)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)) Task to
  sample from.

- `iters`:

  (`integer(1)`: 1) Number of repetitions the `knockoff_fun` is applied
  to create multiple `x_tilde` instances per observation.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    KnockoffGaussianSampler$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(mlr3)
task = tgen("2dnormals")$generate(n = 100)
# Create sampler
sampler = KnockoffGaussianSampler$new(task)
# Sample using row_ids from stored task
sampled_data = sampler$sample("x1")
```

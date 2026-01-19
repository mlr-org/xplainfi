# Sequential Knockoff Conditional Sampler

A
[KnockoffSampler](https://mlr-org.github.io/xplainfi/reference/KnockoffSampler.md)
using sequential knockoffs as created by `seqknockoff::knockoffs_seq`.

## Details

This is equivalent to
[KnockoffSampler](https://mlr-org.github.io/xplainfi/reference/KnockoffSampler.md)
using `knockoff_fun = seqknockoff::knockoffs_seq`.

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
-\> `KnockoffSequentialSampler`

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

- [`KnockoffSequentialSampler$new()`](#method-KnockoffSequentialSampler-new)

- [`KnockoffSequentialSampler$clone()`](#method-KnockoffSequentialSampler-clone)

Inherited methods

- [`xplainfi::FeatureSampler$print()`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.html#method-print)
- [`xplainfi::FeatureSampler$sample_newdata()`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.html#method-sample_newdata)
- [`xplainfi::KnockoffSampler$sample()`](https://mlr-org.github.io/xplainfi/reference/KnockoffSampler.html#method-sample)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance using sequential knockoffs via
`seqknockoff::knockoffs_seq`.

#### Usage

    KnockoffSequentialSampler$new(task, iters = 1)

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

    KnockoffSequentialSampler$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Example requires sequential knockoffs (https://github.com/kormama1/seqknockoff)
# Not on CRAN, install via pak::pak("kormama1/seqknockoff")
if (FALSE) { # \dontrun{
task = tgen("simplex")$generate(n = 100)
sampler_seq = KnockoffSampler$new(task)
sampled_seq = sampler_seq$sample("x1")
} # }
```

# Knockoff Sampler

Implements conditional sampling using Knockoffs.

## Details

The `KnockoffSampler` samples
[Knockoffs](https://rdrr.io/pkg/knockoff/man/knockoff.html) based on the
task data. This class allows arbitrary `knockoff_fun`, which also means
that no input checking against supported feature types can be done. Use
[KnockoffGaussianSampler](https://mlr-org.github.io/xplainfi/reference/KnockoffGaussianSampler.md)
for the Gaussian knockoff sampler for numeric features. Alternative
knockoff samplers include `knockoff_seq()` from the `seqknockoff`
package available on GitHub: <https://github.com/kormama1/seqknockoff>.

Knockoffs are related to the `ConditionalSampler` familty, with key
differences: They do not allow specifying a `conditioning_set`

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

## Super class

[`xplainfi::FeatureSampler`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.md)
-\> `KnockoffSampler`

## Public fields

- `x_tilde`:

  Knockoff matrix with one (or `iters`) row(s) per original observation
  in `task`.

## Methods

### Public methods

- [`KnockoffSampler$new()`](#method-KnockoffSampler-new)

- [`KnockoffSampler$sample()`](#method-KnockoffSampler-sample)

- [`KnockoffSampler$clone()`](#method-KnockoffSampler-clone)

Inherited methods

- [`xplainfi::FeatureSampler$print()`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.html#method-print)
- [`xplainfi::FeatureSampler$sample_newdata()`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.html#method-sample_newdata)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of the KnockoffSampler class.

#### Usage

    KnockoffSampler$new(
      task,
      knockoff_fun = function(x) knockoff::create.second_order(as.matrix(x)),
      iters = 1
    )

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)) Task to
  sample from

- `knockoff_fun`:

  (`function`) Function used to create knockoff matrix. Default are
  second-order Gaussian knockoffs
  ([`knockoff::create.second_order()`](https://rdrr.io/pkg/knockoff/man/create.second_order.html))

- `iters`:

  (`integer(1)`: `1`) Number of repetitions the `knockoff_fun` is
  applied to create multiple `x_tilde` instances per observation.

------------------------------------------------------------------------

### Method [`sample()`](https://rdrr.io/r/base/sample.html)

Sample from stored task using knockoff values. Replaces specified
feature(s) with their knockoff counterparts from the pre-generated
knockoff matrix.

#### Usage

    KnockoffSampler$sample(feature, row_ids = NULL)

#### Arguments

- `feature`:

  (`character`) Feature(s) to sample.

- `row_ids`:

  ([`integer()`](https://rdrr.io/r/base/integer.html) \| `NULL`) Row IDs
  to use. If `NULL`, uses all rows.

#### Returns

Modified copy with knockoff feature(s).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    KnockoffSampler$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(mlr3)
task = tgen("2dnormals")$generate(n = 100)
# Create sampler with default parameters
sampler = KnockoffSampler$new(task)
# Sample using row_ids from stored task
sampled_data = sampler$sample("x1")
```

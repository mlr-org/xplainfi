# Marginal Reference Sampler

Samples complete observations from reference data to replace feature
values. This approach samples from the marginal distribution while
preserving within-row feature dependencies.

## Details

This sampler implements what is called "marginal imputation" in the SAGE
literature (Covert et al. 2020). For each observation, it samples a
complete row from reference data and takes the specified feature values
from that row. This approach:

- Samples from the marginal distribution \\P(X_S)\\ where S is the set
  of features

- Preserves dependencies **within** the sampled reference row

- Breaks dependencies **between** test and reference data

**Terminology note:** In SAGE literature, this is called "marginal
imputation" because features outside the coalition are "imputed" by
sampling from their marginal distribution. We use
`MarginalReferenceSampler` to avoid confusion with missing data
imputation and to clarify that it samples from reference data.

**Comparison with other samplers:**

- `MarginalPermutationSampler`: Shuffles each feature independently,
  breaking all row structure

- `MarginalReferenceSampler`: Samples complete rows, preserving
  within-row dependencies

- `ConditionalSampler`: Samples from \\P(X_S \| X\_{-S})\\, conditioning
  on other features

**Use in SAGE:**

This is the default approach for `MarginalSAGE`. For a test observation
x and features to marginalize S, it samples a reference row x_ref and
creates a "hybrid" observation combining x's coalition features with
x_ref's marginalized features.

## References

Covert I, Lundberg S, Lee S (2020). “Understanding Global Feature
Contributions With Additive Importance Measures.” In *Advances in Neural
Information Processing Systems*, volume 33, 17212–17223.
<https://proceedings.neurips.cc/paper/2020/hash/c7bf0b7c1a86d5eb3be2c722cf2cf746-Abstract.html>.

## Super classes

[`xplainfi::FeatureSampler`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.md)
-\>
[`xplainfi::MarginalSampler`](https://mlr-org.github.io/xplainfi/reference/MarginalSampler.md)
-\> `MarginalReferenceSampler`

## Public fields

- `reference_data`:

  ([`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))
  Reference data to sample from for marginalization.

## Methods

### Public methods

- [`MarginalReferenceSampler$new()`](#method-MarginalReferenceSampler-new)

- [`MarginalReferenceSampler$clone()`](#method-MarginalReferenceSampler-clone)

Inherited methods

- [`xplainfi::FeatureSampler$print()`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.html#method-print)
- [`xplainfi::MarginalSampler$sample()`](https://mlr-org.github.io/xplainfi/reference/MarginalSampler.html#method-sample)
- [`xplainfi::MarginalSampler$sample_newdata()`](https://mlr-org.github.io/xplainfi/reference/MarginalSampler.html#method-sample_newdata)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of the MarginalReferenceSampler class.

#### Usage

    MarginalReferenceSampler$new(task, n_samples = NULL)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)) Task to
  sample from.

- `n_samples`:

  (`integer(1)` \| `NULL`) Number of reference samples to use. If
  `NULL`, uses all task data as reference.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MarginalReferenceSampler$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(mlr3)
task = tgen("friedman1")$generate(n = 100)

# Default: uses all task data as reference
sampler = MarginalReferenceSampler$new(task)
sampled = sampler$sample("important1", row_ids = 1:10)

# Subsample reference data to 50 rows
sampler_subsampled = MarginalReferenceSampler$new(task, n_samples = 50L)
sampled2 = sampler_subsampled$sample("important1", row_ids = 1:10)
```

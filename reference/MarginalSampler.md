# Marginal Sampler Base Class

Abstract base class for marginal sampling strategies that do not
condition on other features. Marginal samplers sample from P(X_S), the
marginal distribution of features S, ignoring any dependencies with
other features.

## Details

This class provides a common interface for different marginal sampling
approaches:

- **MarginalPermutationSampler**: Shuffles features independently within
  the dataset

- **MarginalReferenceSampler**: Samples complete rows from reference
  data

Both approaches sample from the marginal distribution P(X_S), but differ
in how they preserve or break within-row dependencies:

- Permutation breaks ALL dependencies (both with target and between
  features)

- Reference sampling preserves WITHIN-row dependencies but breaks
  dependencies with test data

**Comparison with ConditionalSampler:**

- `MarginalSampler`: Samples from \\P(X_S)\\ - no conditioning

- `ConditionalSampler`: Samples from \\P(X_S \| X\_{-S})\\- conditions
  on other features

This base class implements the public `$sample()` and
`$sample_newdata()` methods, delegating to private `.sample_marginal()`
which subclasses must implement.

## Super class

[`xplainfi::FeatureSampler`](https://jemus42.github.io/xplainfi/reference/FeatureSampler.md)
-\> `MarginalSampler`

## Methods

### Public methods

- [`MarginalSampler$sample()`](#method-MarginalSampler-sample)

- [`MarginalSampler$sample_newdata()`](#method-MarginalSampler-sample_newdata)

- [`MarginalSampler$clone()`](#method-MarginalSampler-clone)

Inherited methods

- [`xplainfi::FeatureSampler$initialize()`](https://jemus42.github.io/xplainfi/reference/FeatureSampler.html#method-initialize)
- [`xplainfi::FeatureSampler$print()`](https://jemus42.github.io/xplainfi/reference/FeatureSampler.html#method-print)

------------------------------------------------------------------------

### Method [`sample()`](https://rdrr.io/r/base/sample.html)

Sample features from their marginal distribution.

#### Usage

    MarginalSampler$sample(feature, row_ids = NULL)

#### Arguments

- `feature`:

  ([`character()`](https://rdrr.io/r/base/character.html)) Feature
  name(s) to sample.

- `row_ids`:

  ([`integer()`](https://rdrr.io/r/base/integer.html) \| `NULL`) Row IDs
  from task to use.

#### Returns

Modified copy with sampled feature(s).

------------------------------------------------------------------------

### Method `sample_newdata()`

Sample from external data.

#### Usage

    MarginalSampler$sample_newdata(feature, newdata)

#### Arguments

- `feature`:

  ([`character()`](https://rdrr.io/r/base/character.html)) Feature(s) to
  sample.

- `newdata`:

  ([`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))
  External data to use.

#### Returns

Modified copy with sampled feature(s).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MarginalSampler$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

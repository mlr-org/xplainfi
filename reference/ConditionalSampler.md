# Conditional Feature Sampler

Base class for conditional sampling methods where features are sampled
conditionally on other features. This is an abstract class that should
be extended by concrete implementations.

## Super class

[`FeatureSampler`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.md)
-\> `ConditionalSampler`

## Methods

### Public methods

- [`ConditionalSampler$new()`](#method-ConditionalSampler-initialize)

- [`ConditionalSampler$sample()`](#method-ConditionalSampler-sample)

- [`ConditionalSampler$sample_newdata()`](#method-ConditionalSampler-sample_newdata)

- [`ConditionalSampler$clone()`](#method-ConditionalSampler-clone)

Inherited methods

- [`FeatureSampler$print()`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.html#method-print)

------------------------------------------------------------------------

### `ConditionalSampler$new()`

Creates a new instance of the ConditionalSampler class

#### Usage

    ConditionalSampler$new(task, conditioning_set = NULL)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)) Task to
  sample from

- `conditioning_set`:

  (`character` \| `NULL`) Default conditioning set to use in
  `$sample()`.

------------------------------------------------------------------------

### `ConditionalSampler$sample()`

Sample from stored task conditionally on other features.

#### Usage

    ConditionalSampler$sample(
      feature,
      row_ids = NULL,
      conditioning_set = NULL,
      samples_per_row = 1L,
      ...
    )

#### Arguments

- `feature`:

  (`character`) Feature(s) to sample.

- `row_ids`:

  ([`integer()`](https://rdrr.io/r/base/integer.html) \| `NULL`) Row IDs
  to use. If `NULL`, uses all rows.

- `conditioning_set`:

  (`character` \| `NULL`) Features to condition on.

- `samples_per_row`:

  (`integer(1)`: `1L`) Number of independent samples per input row. See
  [FeatureSampler](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.md)`$sample()`
  for output shape and ordering.

- `...`:

  Additional arguments passed to the sampler implementation.

#### Returns

Modified copy with sampled feature(s).

------------------------------------------------------------------------

### `ConditionalSampler$sample_newdata()`

Sample from external data conditionally.

#### Usage

    ConditionalSampler$sample_newdata(
      feature,
      newdata,
      conditioning_set = NULL,
      samples_per_row = 1L,
      ...
    )

#### Arguments

- `feature`:

  (`character`) Feature(s) to sample.

- `newdata`:

  ([`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html))
  External data to use.

- `conditioning_set`:

  (`character` \| `NULL`) Features to condition on.

- `samples_per_row`:

  (`integer(1)`: `1L`) Number of independent samples per input row. See
  [FeatureSampler](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.md)`$sample()`
  for output shape and ordering.

- `...`:

  Additional arguments passed to the sampler implementation.

#### Returns

Modified copy with sampled feature(s).

------------------------------------------------------------------------

### `ConditionalSampler$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ConditionalSampler$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

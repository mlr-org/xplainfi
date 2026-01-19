# Conditional Feature Sampler

Base class for conditional sampling methods where features are sampled
conditionally on other features. This is an abstract class that should
be extended by concrete implementations.

## Super class

[`xplainfi::FeatureSampler`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.md)
-\> `ConditionalSampler`

## Methods

### Public methods

- [`ConditionalSampler$new()`](#method-ConditionalSampler-new)

- [`ConditionalSampler$sample()`](#method-ConditionalSampler-sample)

- [`ConditionalSampler$sample_newdata()`](#method-ConditionalSampler-sample_newdata)

- [`ConditionalSampler$clone()`](#method-ConditionalSampler-clone)

Inherited methods

- [`xplainfi::FeatureSampler$print()`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.html#method-print)

------------------------------------------------------------------------

### Method `new()`

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

### Method [`sample()`](https://rdrr.io/r/base/sample.html)

Sample from stored task conditionally on other features.

#### Usage

    ConditionalSampler$sample(
      feature,
      row_ids = NULL,
      conditioning_set = NULL,
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

- `...`:

  Additional arguments passed to the sampler implementation.

#### Returns

Modified copy with sampled feature(s).

------------------------------------------------------------------------

### Method `sample_newdata()`

Sample from external data conditionally.

#### Usage

    ConditionalSampler$sample_newdata(
      feature,
      newdata,
      conditioning_set = NULL,
      ...
    )

#### Arguments

- `feature`:

  (`character`) Feature(s) to sample.

- `newdata`:

  ([`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))
  External data to use.

- `conditioning_set`:

  (`character` \| `NULL`) Features to condition on.

- `...`:

  Additional arguments passed to the sampler implementation.

#### Returns

Modified copy with sampled feature(s).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ConditionalSampler$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

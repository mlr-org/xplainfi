# Feature Sampler Class

Base class for implementing different sampling strategies for feature
importance methods like PFI and CFI

## Public fields

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)) Original
  task.

- `label`:

  (`character(1)`) Name of the sampler.

- `feature_types`:

  ([`character()`](https://rdrr.io/r/base/character.html)) Feature types
  supported by the sampler. Will be checked against the provided
  [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html) to ensure
  compatibility.

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))
  Parameter set for the sampler.

## Methods

### Public methods

- [`FeatureSampler$new()`](#method-FeatureSampler-new)

- [`FeatureSampler$sample()`](#method-FeatureSampler-sample)

- [`FeatureSampler$sample_newdata()`](#method-FeatureSampler-sample_newdata)

- [`FeatureSampler$print()`](#method-FeatureSampler-print)

- [`FeatureSampler$clone()`](#method-FeatureSampler-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of the FeatureSampler class

#### Usage

    FeatureSampler$new(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)) Task to
  sample from

------------------------------------------------------------------------

### Method [`sample()`](https://rdrr.io/r/base/sample.html)

Sample values for feature(s) from stored task

#### Usage

    FeatureSampler$sample(feature, row_ids = NULL)

#### Arguments

- `feature`:

  (`character`) Feature name(s) to sample (can be single or multiple).
  Must match those in the stored
  [Task](https://mlr3.mlr-org.com/reference/Task.html).

- `row_ids`:

  ([`integer()`](https://rdrr.io/r/base/integer.html): `NULL`) Row IDs
  of the stored [Task](https://mlr3.mlr-org.com/reference/Task.html) to
  use as basis for sampling.

#### Returns

Modified copy of the input features with the feature(s) sampled: A
[data.table](https://rdrr.io/pkg/data.table/man/data.table.html) with
same number of columns and one row matching the supplied `row_ids`

------------------------------------------------------------------------

### Method `sample_newdata()`

Sample values for feature(s) using external data

#### Usage

    FeatureSampler$sample_newdata(feature, newdata)

#### Arguments

- `feature`:

  (`character`) Feature name(s) to sample (can be single or multiple)

- `newdata`:

  ([`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) )
  External data to use for sampling.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print sampler

#### Usage

    FeatureSampler$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FeatureSampler$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

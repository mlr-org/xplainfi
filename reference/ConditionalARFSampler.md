# ARF-based Conditional Sampler

Implements conditional sampling using Adversarial Random Forests (ARF).
ARF can handle mixed data types (continuous and categorical) and
provides flexible conditional sampling by modeling the joint
distribution.

## Details

The ConditionalARFSampler fits an [Adversarial Random
Forest](https://bips-hb.github.io/arf/reference/arf-package.html) model
on the task data, then uses it to generate samples from \\P(X_j \|
X\_{-j})\\ where \\X_j\\ is the feature of interest and \\X\_{-j}\\ are
the conditioning features.

## References

Watson D, Blesch K, Kapar J, Wright M (2023). “Adversarial Random
Forests for Density Estimation and Generative Modeling.” In *Proceedings
of The 26th International Conference on Artificial Intelligence and
Statistics*, 5357–5375.
<https://proceedings.mlr.press/v206/watson23a.html>.

Blesch K, Koenen N, Kapar J, Golchian P, Burk L, Loecher M, Wright M
(2025). “Conditional Feature Importance with Generative Modeling Using
Adversarial Random Forests.” *Proceedings of the AAAI Conference on
Artificial Intelligence*, **39**(15), 15596–15604.
[doi:10.1609/aaai.v39i15.33712](https://doi.org/10.1609/aaai.v39i15.33712)
.

## Super classes

[`xplainfi::FeatureSampler`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.md)
-\>
[`xplainfi::ConditionalSampler`](https://mlr-org.github.io/xplainfi/reference/ConditionalSampler.md)
-\> `ConditionalARFSampler`

## Public fields

- `feature_types`:

  ([`character()`](https://rdrr.io/r/base/character.html)) Feature types
  supported by the sampler. Will be checked against the provided
  [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html) to ensure
  compatibility.

- `arf_model`:

  Adversarial Random Forest model created by
  [arf::adversarial_rf](https://bips-hb.github.io/arf/reference/adversarial_rf.html).

- `psi`:

  Distribution parameters estimated from by
  [arf::forde](https://bips-hb.github.io/arf/reference/forde.html).

## Methods

### Public methods

- [`ConditionalARFSampler$new()`](#method-ConditionalARFSampler-new)

- [`ConditionalARFSampler$sample()`](#method-ConditionalARFSampler-sample)

- [`ConditionalARFSampler$sample_newdata()`](#method-ConditionalARFSampler-sample_newdata)

- [`ConditionalARFSampler$clone()`](#method-ConditionalARFSampler-clone)

Inherited methods

- [`xplainfi::FeatureSampler$print()`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of the ConditionalARFSampler class. To fit the
ARF in parallel, register a parallel backend first (see
[arf::arf](https://bips-hb.github.io/arf/reference/arf-package.html))
and set `parallel = TRUE`.

#### Usage

    ConditionalARFSampler$new(
      task,
      conditioning_set = NULL,
      num_trees = 10L,
      min_node_size = 20L,
      finite_bounds = "no",
      epsilon = 1e-15,
      round = TRUE,
      stepsize = 0,
      verbose = FALSE,
      parallel = FALSE,
      ...
    )

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)) Task to
  sample from.

- `conditioning_set`:

  (`character` \| `NULL`) Default conditioning set to use in
  `$sample()`. This parameter only affects the sampling behavior, not
  the ARF model fitting.

- `num_trees`:

  (`integer(1)`: `10L`) Number of trees for ARF. Passed to
  [arf::adversarial_rf](https://bips-hb.github.io/arf/reference/adversarial_rf.html).

- `min_node_size`:

  (`integer(1)`: `20L`) Minimum node size for ARF. Passed to
  [arf::adversarial_rf](https://bips-hb.github.io/arf/reference/adversarial_rf.html)
  and in turn to
  [ranger::ranger](http://imbs-hl.github.io/ranger/reference/ranger.md).
  This is increased to 20 to mitigate overfitting.

- `finite_bounds`:

  (`character(1)`: `"no"`) How to handle variable bounds. Passed to
  [arf::forde](https://bips-hb.github.io/arf/reference/forde.html).
  Default is `"no"` for compatibility. `"local"` may improve
  extrapolation but can cause issues with some data.

- `epsilon`:

  (`numeric(1)`: `0`) Slack parameter for when `finite_bounds != "no"`.
  Passed to
  [arf::forde](https://bips-hb.github.io/arf/reference/forde.html).

- `round`:

  (`logical(1)`: `TRUE`) Whether to round continuous variables back to
  their original precision in sampling. Can be overridden in `$sample()`
  calls.

- `stepsize`:

  (`numeric(1)`: `0`) Number of rows of evidence to process at a time
  when `parallel` is `TRUE`. Default (`0`) spreads evidence evenly over
  registered workers. Can be overridden in `$sample()` calls.

- `verbose`:

  (`logical(1)`: `FALSE`) Whether to print progress messages. Default is
  `FALSE` (arf's default is `TRUE`). Can be overridden in `$sample()`
  calls.

- `parallel`:

  (`logical(1)`: `FALSE`) Whether to use parallel processing via
  `foreach`. See examples in
  [`arf::forge()`](https://bips-hb.github.io/arf/reference/forge.html).
  Can be overridden in `$sample()` calls.

- `...`:

  Additional arguments passed to
  [arf::adversarial_rf](https://bips-hb.github.io/arf/reference/adversarial_rf.html).

------------------------------------------------------------------------

### Method [`sample()`](https://rdrr.io/r/base/sample.html)

Sample from stored task. Parameters use hierarchical resolution:
function argument \> stored `param_set` value \> hard-coded default.

#### Usage

    ConditionalARFSampler$sample(
      feature,
      row_ids = NULL,
      conditioning_set = NULL,
      round = NULL,
      stepsize = NULL,
      verbose = NULL,
      parallel = NULL
    )

#### Arguments

- `feature`:

  (`character`) Feature(s) to sample.

- `row_ids`:

  ([`integer()`](https://rdrr.io/r/base/integer.html) \| `NULL`) Row IDs
  to use. If `NULL`, uses all rows.

- `conditioning_set`:

  (`character` \| `NULL`) Features to condition on.

- `round`:

  (`logical(1)` \| `NULL`) Round continuous variables.

- `stepsize`:

  (`numeric(1)` \| `NULL`) Batch size for parallel processing.

- `verbose`:

  (`logical(1)` \| `NULL`) Print progress messages.

- `parallel`:

  (`logical(1)` \| `NULL`) Use parallel processing.

#### Returns

Modified copy with sampled feature(s).

------------------------------------------------------------------------

### Method `sample_newdata()`

Sample from external data. See `$sample()` for parameter details.

#### Usage

    ConditionalARFSampler$sample_newdata(
      feature,
      newdata,
      conditioning_set = NULL,
      round = NULL,
      stepsize = NULL,
      verbose = NULL,
      parallel = NULL
    )

#### Arguments

- `feature`:

  (`character`) Feature(s) to sample.

- `newdata`:

  ([`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html))
  External data to use.

- `conditioning_set`:

  (`character` \| `NULL`) Features to condition on.

- `round`:

  (`logical(1)` \| `NULL`) Round continuous variables.

- `stepsize`:

  (`numeric(1)` \| `NULL`) Batch size for parallel processing.

- `verbose`:

  (`logical(1)` \| `NULL`) Print progress messages.

- `parallel`:

  (`logical(1)` \| `NULL`) Use parallel processing.

#### Returns

Modified copy with sampled feature(s).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ConditionalARFSampler$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(mlr3)
task = tgen("2dnormals")$generate(n = 100)
# Create sampler with default parameters
sampler = ConditionalARFSampler$new(task, conditioning_set = "x2", verbose = FALSE)
# Sample using row_ids from stored task
sampled_data = sampler$sample("x1", row_ids = 1:10)
# Or use external data
data = task$data()
sampled_data_ext = sampler$sample_newdata("x1", newdata = data, conditioning_set = "x2")

# Example with custom ARF parameters
sampler_custom = ConditionalARFSampler$new(
  task,
  min_node_size = 10L,
  finite_bounds = "local",
  verbose = FALSE
)
sampled_custom = sampler_custom$sample("x1", conditioning_set = "x2")
```

# k-Nearest Neighbors Conditional Sampler

Implements conditional sampling using k-nearest neighbors (kNN). For
each observation, finds the `k` most similar observations based on
conditioning features, then samples the target features from these
neighbors.

## Details

This sampler approximates the conditional distribution \\P(X_B \| X_A =
x_A)\\ by:

1.  Finding the k nearest neighbors of \\x_A\\ in the training data

2.  Sampling uniformly from the target feature values \\X_B\\ of these k
    neighbors

This is a simple, non-parametric approach that:

- Requires no distributional assumptions

- Handles mixed feature types (numeric, integer, factor, ordered,
  logical)

- Is computationally efficient (no model fitting required)

- Adapts locally to the data structure

The method is related to hot-deck imputation and kNN imputation
techniques used in missing data problems. As \\k \to \infty\\ and \\k/n
\to 0\\, the kNN conditional distribution converges to the true
conditional distribution under mild regularity conditions (Lipschitz
continuity).

**Distance Metrics:**

The sampler supports two distance metrics:

- **Euclidean**: For numeric/integer features only. Standardizes
  features before computing distances.

- **Gower**: For mixed feature types. Handles numeric, factor, ordered,
  and logical features. Numeric features are range-normalized,
  categorical features use exact matching (0/1).

The `distance` parameter controls which metric to use:

- `"auto"` (default): Automatically selects Euclidean for all-numeric
  features, Gower otherwise

- `"euclidean"`: Forces Euclidean distance (errors if non-numeric
  features present)

- `"gower"`: Forces Gower distance (works with any feature types)

**Advantages:**

- Very fast (no model training)

- Works with any feature types

- Automatic distance metric selection

- Naturally respects local data structure

**Limitations:**

- Sensitive to choice of `k`

- The full task data is required for prediction

- Can produce duplicates if `k` is small

- May not extrapolate well to new regions

## References

Little R, Rubin D (2019). *Statistical Analysis with Missing Data*, 3rd
edition. John Wiley & Sons, Hoboken, NJ. ISBN 9780470526798.

Troyanskaya O, Cantor M, Sherlock G, Brown P, Hastie T, Tibshirani R,
Botstein D, Altman R (2001). “Missing Value Estimation Methods for DNA
Microarrays.” *Bioinformatics*, **17**(6), 520–525.
[doi:10.1093/bioinformatics/17.6.520](https://doi.org/10.1093/bioinformatics/17.6.520)
.

## Super classes

[`xplainfi::FeatureSampler`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.md)
-\>
[`xplainfi::ConditionalSampler`](https://mlr-org.github.io/xplainfi/reference/ConditionalSampler.md)
-\> `ConditionalKNNSampler`

## Public fields

- `feature_types`:

  ([`character()`](https://rdrr.io/r/base/character.html)) Feature types
  supported by the sampler.

## Methods

### Public methods

- [`ConditionalKNNSampler$new()`](#method-ConditionalKNNSampler-new)

- [`ConditionalKNNSampler$sample()`](#method-ConditionalKNNSampler-sample)

- [`ConditionalKNNSampler$sample_newdata()`](#method-ConditionalKNNSampler-sample_newdata)

- [`ConditionalKNNSampler$clone()`](#method-ConditionalKNNSampler-clone)

Inherited methods

- [`xplainfi::FeatureSampler$print()`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new ConditionalKNNSampler.

#### Usage

    ConditionalKNNSampler$new(task, conditioning_set = NULL, k = 5L)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)) Task to
  sample from.

- `conditioning_set`:

  (`character` \| `NULL`) Default conditioning set to use in
  `$sample()`.

- `k`:

  (`integer(1)`: `5L`) Number of nearest neighbors to sample from.

------------------------------------------------------------------------

### Method [`sample()`](https://rdrr.io/r/base/sample.html)

Sample features from their kNN-based conditional distribution.

#### Usage

    ConditionalKNNSampler$sample(
      feature,
      row_ids = NULL,
      conditioning_set = NULL,
      k = NULL
    )

#### Arguments

- `feature`:

  ([`character()`](https://rdrr.io/r/base/character.html)) Feature
  name(s) to sample.

- `row_ids`:

  ([`integer()`](https://rdrr.io/r/base/integer.html) \| `NULL`) Row IDs
  from task to use as conditioning values.

- `conditioning_set`:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)
  Features to condition on. If `NULL`, samples from marginal
  distribution (random sampling from training data).

- `k`:

  (`integer(1)` \| `NULL`) Number of neighbors. If `NULL`, uses stored
  parameter.

#### Returns

Modified copy with sampled feature(s).

------------------------------------------------------------------------

### Method `sample_newdata()`

Sample from external data conditionally.

#### Usage

    ConditionalKNNSampler$sample_newdata(
      feature,
      newdata,
      conditioning_set = NULL,
      k = NULL
    )

#### Arguments

- `feature`:

  ([`character()`](https://rdrr.io/r/base/character.html)) Feature(s) to
  sample.

- `newdata`:

  ([`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))
  External data to use.

- `conditioning_set`:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)
  Features to condition on.

- `k`:

  (`integer(1)` \| `NULL`) Number of neighbors. If `NULL`, uses stored
  parameter.

#### Returns

Modified copy with sampled feature(s).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ConditionalKNNSampler$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(mlr3)
task = tgen("friedman1")$generate(n = 100)
sampler = ConditionalKNNSampler$new(task, k = 5)

# Sample features conditioned on others
test_data = task$data(rows = 1:5)
sampled = sampler$sample_newdata(
  feature = c("important2", "important3"),
  newdata = test_data,
  conditioning_set = "important1"
)
```

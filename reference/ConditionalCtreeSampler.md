# (experimental) Conditional Inference Tree Conditional Sampler

Implements conditional sampling using conditional inference trees
(ctree). Builds a tree predicting target features from conditioning
features, then samples from the terminal node corresponding to each test
observation.

## Details

This sampler approximates the conditional distribution \\P(X_B \| X_A =
x_A)\\ by:

1.  Building a conditional inference tree with \\X_B\\ as response and
    \\X_A\\ as predictors

2.  For each test observation, finding its terminal (leaf) node in the
    tree

3.  Sampling uniformly from training observations in that same terminal
    node

Conditional inference trees (ctree) use permutation tests to determine
splits, which helps avoid overfitting and handles mixed feature types
naturally. The tree partitions the feature space based on the
conditioning variables, creating local neighborhoods that respect the
conditional distribution structure.

**Key advantages over other samplers:**

- Handles mixed feature types (continuous and categorical)

- Non-parametric (no distributional assumptions)

- Automatic feature selection (splits only on informative features)

- Can capture non-linear conditional relationships

- Statistically principled splitting criteria

**Hyperparameters** control tree complexity:

- `mincriterion`: Significance level for splits (higher = fewer splits)

- `minsplit`: Minimum observations required for a split

- `minbucket`: Minimum observations in terminal nodes

This implementation is inspired by shapr's ctree approach but simplified
for our use case (we build trees on-demand rather than pre-computing all
subsets).

**Advantages:**

- Works with any feature types

- Robust to outliers

- Interpretable tree structure

- Handles high-dimensional conditioning

**Limitations:**

- Requires model fitting (slower than kNN)

- Can produce duplicates if terminal nodes are small

- Tree building time increases with data size

## References

Hothorn T, Hornik K, Zeileis A (2006). “Unbiased Recursive Partitioning:
A Conditional Inference Framework.” *Journal of Computational and
Graphical Statistics*, **15**(3), 651–674.
[doi:10.1198/106186006X133933](https://doi.org/10.1198/106186006X133933)
.

Aas K, Jullum M, Løland A (2021). “Explaining Individual Predictions
When Features Are Dependent: More Accurate Approximations to Shapley
Values.” *Artificial Intelligence*, **298**, 103502.
[doi:10.1016/j.artint.2021.103502](https://doi.org/10.1016/j.artint.2021.103502)
.

## Super classes

[`xplainfi::FeatureSampler`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.md)
-\>
[`xplainfi::ConditionalSampler`](https://mlr-org.github.io/xplainfi/reference/ConditionalSampler.md)
-\> `ConditionalCtreeSampler`

## Public fields

- `feature_types`:

  ([`character()`](https://rdrr.io/r/base/character.html)) Feature types
  supported by the sampler. Will be checked against the provided
  [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html) to ensure
  compatibility.

- `tree_cache`:

  (`environment`) Cache for fitted ctree models.

## Methods

### Public methods

- [`ConditionalCtreeSampler$new()`](#method-ConditionalCtreeSampler-new)

- [`ConditionalCtreeSampler$clone()`](#method-ConditionalCtreeSampler-clone)

Inherited methods

- [`xplainfi::FeatureSampler$print()`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.html#method-print)
- [`xplainfi::ConditionalSampler$sample()`](https://mlr-org.github.io/xplainfi/reference/ConditionalSampler.html#method-sample)
- [`xplainfi::ConditionalSampler$sample_newdata()`](https://mlr-org.github.io/xplainfi/reference/ConditionalSampler.html#method-sample_newdata)

------------------------------------------------------------------------

### Method `new()`

Creates a new ConditionalCtreeSampler.

#### Usage

    ConditionalCtreeSampler$new(
      task,
      conditioning_set = NULL,
      mincriterion = 0.95,
      minsplit = 20L,
      minbucket = 7L,
      use_cache = TRUE
    )

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)) Task to
  sample from.

- `conditioning_set`:

  (`character` \| `NULL`) Default conditioning set to use in
  `$sample()`.

- `mincriterion`:

  (`numeric(1)`: `0.95`) Significance level threshold for splitting (1 -
  p-value). Higher values result in fewer splits (simpler trees).

- `minsplit`:

  (`integer(1)`: `20L`) Minimum number of observations required for a
  split.

- `minbucket`:

  (`integer(1)`: `7L`) Minimum number of observations in terminal nodes.

- `use_cache`:

  (`logical(1)`: `TRUE`) Whether to cache fitted trees.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ConditionalCtreeSampler$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# \donttest{
library(mlr3)
task = tgen("friedman1")$generate(n = 100)

# Create sampler with default parameters
sampler = ConditionalCtreeSampler$new(task)

# Sample features conditioned on others
test_data = task$data(rows = 1:5)
sampled = sampler$sample_newdata(
  feature = c("important2", "important3"),
  newdata = test_data,
  conditioning_set = "important1"
)
# }
```

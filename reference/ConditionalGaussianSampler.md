# Gaussian Conditional Sampler

Implements conditional sampling assuming features follow a multivariate
Gaussian distribution. Computes conditional distributions analytically
using standard formulas for multivariate normal distributions.

## Details

For a joint Gaussian distribution \\X \sim N(\mu, \Sigma)\\, partitioned
as \\X = (X_A, X_B)\\, the conditional distribution is:

\$\$X_B \| X_A = x_A \sim N(\mu\_{B\|A}, \Sigma\_{B\|A})\$\$

where: \$\$\mu\_{B\|A} = \mu_B + \Sigma\_{BA} \Sigma\_{AA}^{-1} (x_A -
\mu_A)\$\$ \$\$\Sigma\_{B\|A} = \Sigma\_{BB} - \Sigma\_{BA}
\Sigma\_{AA}^{-1} \Sigma\_{AB}\$\$

This is equivalent to the regression formulation used by fippy:
\$\$\beta = \Sigma\_{BA} \Sigma\_{AA}^{-1}\$\$ \$\$\mu\_{B\|A} = \mu_B +
\beta (x_A - \mu_A)\$\$ \$\$\Sigma\_{B\|A} = \Sigma\_{BB} - \beta
\Sigma\_{AB}\$\$

**Assumptions:**

- Features are approximately multivariate normal

- Only continuous features are supported

**Advantages:**

- Very fast (closed-form solution)

- Deterministic (given seed)

- No hyperparameters

- Memory efficient

**Limitations:**

- Strong distributional assumption

- May produce out-of-range values for bounded features

- Cannot handle categorical features

- Integer features are treated as continuous and rounded back to
  integers

## References

Anderson T (2003). *An Introduction to Multivariate Statistical
Analysis*, 3rd edition. Wiley-Interscience, Hoboken, NJ. ISBN
9780471360919.

## Super classes

[`xplainfi::FeatureSampler`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.md)
-\>
[`xplainfi::ConditionalSampler`](https://mlr-org.github.io/xplainfi/reference/ConditionalSampler.md)
-\> `ConditionalGaussianSampler`

## Public fields

- `feature_types`:

  ([`character()`](https://rdrr.io/r/base/character.html)) Feature types
  supported by the sampler.

- `mu`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html)) Mean vector
  estimated from training data.

- `sigma`:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html)) Covariance matrix
  estimated from training data.

## Methods

### Public methods

- [`ConditionalGaussianSampler$new()`](#method-ConditionalGaussianSampler-new)

- [`ConditionalGaussianSampler$clone()`](#method-ConditionalGaussianSampler-clone)

Inherited methods

- [`xplainfi::FeatureSampler$print()`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.html#method-print)
- [`xplainfi::ConditionalSampler$sample()`](https://mlr-org.github.io/xplainfi/reference/ConditionalSampler.html#method-sample)
- [`xplainfi::ConditionalSampler$sample_newdata()`](https://mlr-org.github.io/xplainfi/reference/ConditionalSampler.html#method-sample_newdata)

------------------------------------------------------------------------

### Method `new()`

Creates a new ConditionalGaussianSampler.

#### Usage

    ConditionalGaussianSampler$new(task, conditioning_set = NULL)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)) Task to
  sample from. Must have only numeric/integer features.

- `conditioning_set`:

  (`character` \| `NULL`) Default conditioning set to use in
  `$sample()`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ConditionalGaussianSampler$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(mlr3)
task = tgen("friedman1")$generate(n = 100)
sampler = ConditionalGaussianSampler$new(task)

# Sample x2, x3 conditioned on x1
test_data = task$data(rows = 1:5)
sampled = sampler$sample_newdata(
  feature = c("important2", "important3"),
  newdata = test_data,
  conditioning_set = "important1"
)
```

# Shapley Additive Global Importance (SAGE) Base Class

Base class for SAGE (Shapley Additive Global Importance) feature
importance based on Shapley values with marginalization. This is an
abstract class - use
[MarginalSAGE](https://mlr-org.github.io/xplainfi/reference/MarginalSAGE.md)
or
[ConditionalSAGE](https://mlr-org.github.io/xplainfi/reference/ConditionalSAGE.md).

## Details

SAGE uses Shapley values to fairly distribute the total prediction
performance among all features. Unlike perturbation-based methods, SAGE
marginalizes features by integrating over their distribution. This is
approximated by averaging predictions over a reference dataset.

SAGE values are reductions in the measure's score relative to the empty
coalition, `score(empty) - score(S)`, so that positive values mean the
feature improves performance. For measures that are maximized
(`measure$minimize = FALSE`, e.g. `classif.acc`) the scores are negated
internally, so the sign convention is the same for all measures.

**Standard Error Calculation**: The standard errors (SE) reported in
`$convergence_history` reflect the uncertainty in Shapley value
estimation across different random permutations within a single
resampling iteration. These SEs quantify the Monte Carlo sampling error
for a fixed trained model and are only valid for inference about the
importance of features for that specific model. They do not capture
broader uncertainty from model variability across different train/test
splits or resampling iterations.

**Estimators**: `estimator = "permutation"` (the default) is the
permutation-sampling estimator of Covert et al. (2020), budgeted by
`n_permutations`. `estimator = "kernel"` is the regression-based
estimator of Covert & Lee (2021), budgeted by `n_coalitions`: Shapley
values are the solution of a weighted least squares problem,
approximated from sampled coalitions (see below). `estimator = "exact"`
enumerates all `2^n_features` coalitions and computes the Shapley values
in closed form, so it has no coalition-sampling error and serves as a
ground-truth reference for the sampling estimators on small feature sets
(capped by `max_features`). "Exact" refers to coalition sampling only:
the marginalization error controlled by `n_samples` remains, and for
[ConditionalSAGE](https://mlr-org.github.io/xplainfi/reference/ConditionalSAGE.md)
the value function itself is a Monte Carlo estimate of the sampler.

**Kernel estimator**: This implements the unbiased KernelSHAP estimator
of Covert & Lee (2021, Eq. 9) for the stochastic cooperative game of
SAGE, mirroring the `KernelEstimator` of the reference Python `sage`
package. Coalitions are drawn from the Shapley kernel (size `k` with
probability proportional to `1 / (k (p - k))`, uniform within size)
together with their complement (paired sampling, their Section 4.2), and
each draw is paired with a single test observation drawn with
replacement, whose observation-wise loss is the value-function sample.
Consequently the kernel estimator requires a measure with an
observation-wise loss (`"obs_loss"` in `measure$properties`, e.g.
`regr.mse` or `classif.logloss`, but not `classif.auc`), and each
coalition evaluation costs `n_samples` model rows rather than
`n_test * n_samples` as for the other estimators, which evaluate every
coalition on the whole test set. For such measures the kernel estimator
targets the same SAGE values as the permutation and exact estimators
(the Shapley value is linear in the value function, and the test-set
loss is the mean of the observation-wise losses), so
`estimator = "exact"` remains the reference; `$budget$n_rows` compares
their costs in model rows. The kernel estimator is currently available
for
[MarginalSAGE](https://mlr-org.github.io/xplainfi/reference/MarginalSAGE.md)
only.

**Convergence and budget**: With `early_stopping = TRUE`, sampling stops
once the largest SE, relative to the spread of the SAGE values
(`max(se) / (max(phi) - min(phi))`), falls below `se_threshold`. This is
the criterion of the reference Python `sage` package. Early stopping is
currently available for the permutation estimator only. The budget
argument (`n_permutations`) then acts as an upper bound rather than a
planned cost: exhausting it without meeting the criterion returns the
values with a warning. `$budget` reports what was actually spent and
whether the criterion was met, and `$plot_convergence()` shows the
trajectory that led there. Under resampling, only the first iteration
runs the criterion and the remaining iterations reuse its budget, which
keeps them comparable and avoids re-deriving the standard errors in
every iteration.

## References

Covert I, Lundberg S, Lee S (2020). “Understanding Global Feature
Contributions With Additive Importance Measures.” In *Advances in Neural
Information Processing Systems*, volume 33, 17212–17223.
<https://proceedings.neurips.cc/paper/2020/hash/c7bf0b7c1a86d5eb3be2c722cf2cf746-Abstract.html>.

Covert I, Lee S (2021). “Improving KernelSHAP: Practical Shapley Value
Estimation Using Linear Regression.” In *Proceedings of the 24th
International Conference on Artificial Intelligence and Statistics*,
volume 130, 3457–3465.
<https://proceedings.mlr.press/v130/covert21a.html>.

## See also

[MarginalSAGE](https://mlr-org.github.io/xplainfi/reference/MarginalSAGE.md)
[ConditionalSAGE](https://mlr-org.github.io/xplainfi/reference/ConditionalSAGE.md)

## Super class

[`FeatureImportanceMethod`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.md)
-\> `SAGE`

## Public fields

- `convergence_history`:

  ([`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html))
  History of SAGE values during computation. Columns `budget` (sampling
  effort in the estimator's own units), `n_evals` (the corresponding
  number of evaluated coalitions), and `n_rows` (model rows predicted)
  index the checkpoints; see `$budget`.

- `converged`:

  (`logical(1)`) Whether the convergence criterion was met
  (`early_stopping = TRUE`). `NA` for the exact estimator, which
  enumerates all coalitions and has no criterion to meet.

## Active bindings

- `budget`:

  ([`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html))
  Read-only one-row summary of the sampling effort: the `estimator`, its
  `unit` of budget, the `requested` upper bound, the amount `used`
  (below the request only with early stopping), the resulting number of
  coalition evaluations `n_evals` (one empty-coalition baseline plus
  `n_features` per permutation; two anchors plus two per coalition draw
  for the kernel estimator; `2^n_features` for the exact estimator), the
  number of model rows predicted `n_rows`, and whether the computation
  `converged`. `n_evals` counts coalition evaluations, which differ in
  cost between estimators (the kernel estimator evaluates a coalition on
  one test observation, the others on the whole test set), so `n_rows`
  is the unit in which estimators are comparable. `used`, `n_evals`, and
  `n_rows` are `NA` before `$compute()`; `converged` is `NA` for the
  exact estimator, which has no criterion to meet. With multiple
  resampling iterations it describes the first iteration, whose budget
  the remaining ones reuse (see `early_stopping`).

- `n_permutations_used`:

  Defunct. Use `$budget` instead, which reports the effort spent
  alongside its unit and the implied number of coalition evaluations.

- `n_permutations`:

  (`integer(1)`) Deprecated. The permutation budget lives in the
  param_set; use `$param_set$values$n_permutations` instead. This alias
  is kept for backward compatibility with the field of the same name in
  earlier releases and warns on every access.

## Methods

### Public methods

- [`SAGE$new()`](#method-SAGE-initialize)

- [`SAGE$compute()`](#method-SAGE-compute)

- [`SAGE$reset()`](#method-SAGE-reset)

- [`SAGE$plot_convergence()`](#method-SAGE-plot_convergence)

- [`SAGE$clone()`](#method-SAGE-clone)

Inherited methods

- [`FeatureImportanceMethod$importance()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-importance)
- [`FeatureImportanceMethod$obs_loss()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-obs_loss)
- [`FeatureImportanceMethod$print()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-print)
- [`FeatureImportanceMethod$scores()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-scores)

------------------------------------------------------------------------

### `SAGE$new()`

Creates a new instance of the SAGE class.

#### Usage

    SAGE$new(
      task,
      learner,
      measure = NULL,
      resampling = NULL,
      features = NULL,
      estimator = c("permutation", "kernel", "exact"),
      n_permutations = NULL,
      n_coalitions = NULL,
      max_features = 12L,
      batch_size = 5000L,
      n_samples = 100L,
      early_stopping = FALSE,
      se_threshold = 0.025,
      min_permutations = 10L,
      check_interval = 1L
    )

#### Arguments

- `task, learner, measure, resampling, features`:

  Passed to FeatureImportanceMethod.

- `estimator`:

  (`character(1)`: `"permutation"`) Shapley-value estimator.
  `"permutation"` is the permutation-sampling estimator of Covert et al.
  (2020), budgeted by `n_permutations`; `"kernel"` is the
  regression-based estimator of Covert & Lee (2021), budgeted by
  `n_coalitions`; `"exact"` enumerates all `2^n_features` coalitions
  (capped by `max_features`) and takes no budget. All approximate the
  same SAGE values; setting the budget argument of a different estimator
  is an error. Their costs are comparable through `$budget$n_rows`, the
  number of model rows predicted; see Details for why the kernel
  estimator's coalition evaluations are cheaper than the others'.
  `$compute()` points out in a message (silenced by
  `xplain_opt(verbose = FALSE)`) when the sampling budget meets or
  exceeds the exact estimator's cost, since enumeration then removes the
  coalition-sampling error at no extra cost.

- `n_permutations`:

  (`integer(1)`: `NULL`) Number of permutations for
  `estimator = "permutation"`. Each permutation evaluates one coalition
  per feature, so the cost is `1 + n_permutations * n_features`
  evaluated coalitions. If unset, defaults to `10L`.

- `n_coalitions`:

  (`integer(1)`: `NULL`) Number of paired coalition draws for
  `estimator = "kernel"`. Each draw evaluates a coalition and its
  complement on one test observation, so the cost is
  `2 + 2 * n_coalitions` evaluated coalitions. If unset, defaults to
  `2048L`. Check whether the budget suffices with `$plot_convergence()`
  and increase it until the values settle.

- `max_features`:

  (`integer(1)`: `12L`) Cap on the number of features for
  `estimator = "exact"`, whose cost grows as `2^n_features`;
  construction aborts above it.

- `batch_size`:

  (`integer(1)`: `5000L`) Maximum number of observations to process in a
  single prediction call.

- `n_samples`:

  (`integer(1)`: `100L`) Number of samples to use for marginalizing
  out-of-coalition features. For
  [MarginalSAGE](https://mlr-org.github.io/xplainfi/reference/MarginalSAGE.md),
  this is the number of marginal data samples ("background data" in
  other implementations). For
  [ConditionalSAGE](https://mlr-org.github.io/xplainfi/reference/ConditionalSAGE.md),
  this is the number of conditional samples per test instance retrieved
  from `sampler`.

- `early_stopping`:

  (`logical(1)`: `FALSE`) Whether to stop once the convergence criterion
  is met, rather than spending the full budget. The budget then acts as
  an upper bound: if the criterion is not met within it, the values are
  returned with a warning and `$budget` reports `converged = FALSE`.

- `se_threshold`:

  (`numeric(1)`: `0.025`) Convergence threshold for relative standard
  error. Convergence is detected when the maximum relative SE across all
  features falls below this threshold. Relative SE is calculated as SE
  divided by the range of importance values (max - min), making it
  scale-invariant across different loss metrics. The default of `0.025`
  (convergence once the relative SE is below 2.5% of the importance
  range) is the default of the Python `sage` package; the examples in
  Covert et al. (2020) use `0.01` to `0.02`.

- `min_permutations`:

  (`integer(1)`: `10L`) Minimum permutations before checking for
  convergence. Convergence is judged based on the standard errors of the
  estimated SAGE values, which requires a sufficiently large number of
  samples (i.e., evaluated coalitions).

- `check_interval`:

  (`integer(1)`: `1L`) Check convergence every N permutations.

------------------------------------------------------------------------

### `SAGE$compute()`

Compute SAGE values.

#### Usage

    SAGE$compute(
      store_backends = TRUE,
      batch_size = NULL,
      early_stopping = NULL,
      se_threshold = NULL,
      min_permutations = NULL,
      check_interval = NULL
    )

#### Arguments

- `store_backends`:

  (`logical(1)`) Whether to store data backends.

- `batch_size`:

  (`integer(1)`: `5000L`) Maximum number of observations to process in a
  single prediction call.

- `early_stopping`:

  (`logical(1)`: `FALSE`) Whether to check for convergence and stop
  early.

- `se_threshold`:

  (`numeric(1)`: `0.025`) Convergence threshold for relative standard
  error. SE is normalized by the range of importance values (max - min)
  to make convergence detection scale-invariant. Default `0.025` means
  convergence when relative SE \< 2.5%.

- `min_permutations`:

  (`integer(1)`: `10L`) Minimum permutations before checking
  convergence.

- `check_interval`:

  (`integer(1)`: `1L`) Check convergence every N permutations. The
  convergence arguments only apply to `estimator = "permutation"`;
  passing them for another estimator is a warning.

------------------------------------------------------------------------

### `SAGE$reset()`

Resets all stored fields populated by `$compute()`, including the
convergence tracking (`$convergence_history`, `$converged`, `$budget`).

#### Usage

    SAGE$reset()

------------------------------------------------------------------------

### `SAGE$plot_convergence()`

Plot convergence history of SAGE values.

#### Usage

    SAGE$plot_convergence(features = NULL)

#### Arguments

- `features`:

  (`character` \| `NULL`) Features to plot. If NULL, plots all features.

#### Returns

A [ggplot2](https://ggplot2.tidyverse.org/reference/ggplot.html) object

------------------------------------------------------------------------

### `SAGE$clone()`

The objects of this class are cloneable with this method.

#### Usage

    SAGE$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

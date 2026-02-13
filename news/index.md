# Changelog

## xplainfi (development version)

### New features

- Use of a pre-trained `mlr3` learner is now supported in
  `PerturbationImportance` (`PFI`, `CFI`, `RFI`) and `SAGE` methods.
  - Requires the provided `Resampling` to be instantiated and consist of
    a single iteration, e.g. there must be only 1 test set.
  - Internally, a `ResampleResult` will be constructed from the given
    `learner`, `task`, and `resampling` arguments, which is then
    consistent with the previous default of performing
    [`resample()`](https://mlr3.mlr-org.com/reference/resample.html) to
    get trained learners for each resampling iteration.

### Minor user-facing changes

- Bump the defaults for `n_repeats` in favor of stability
  - For`PerturbationImportance` methods (`PFI`, `CFI`, `RFI`):
    `n_repeats` is now 30
  - `LOCO` and `WVIM`: `n_repeats` is now 30 as well.
  - Since the refitting methods will be more expensive than the
    perturbation-based methods, users will have to decrease this value
    if runtime becomes impractical, but now at least the package default
    is no longer `n_repeats = 1`, which is obviously too small.

### Testing improvements

- Replaced `ranger` with `rpart` in most tests where a flexible learner
  was unnecessary, reducing test runtime and removing conditional
  `skip_if_not_installed("ranger")` guards so these tests always run.
- Added omnibus `expect_method_output()` expectation that validates all
  three main outputs (`$importance()`, `$scores()`, `$obs_loss()`) of a
  computed method.
- Removed overly abstract test helper functions (`test_basic_workflow`,
  `test_with_resampling`, `test_custom_sampler`) and inlined their logic
  at call sites for better readability.
- Use `ConditionalGaussianSampler` instead of `ConditionalARFSampler` in
  tests that don’t specifically test ARF functionality.
- Set explicit `n_repeats` values in all tests (1L for functional, 5L
  for plausibility).

### Inference

- Parametric `ci_method`s (`"raw"`, `"nadeau_bengio"`) return `se`,
  `statistic`, `p.value`, `conf_lower`, and `conf_upper` columns. The
  `"quantile"` method returns only `conf_lower` and `conf_upper` (no
  `se`, `statistic`, or `p.value`).
- Parametric `ci_method`s support `alternative = "greater"` (one-sided,
  the default) or `alternative = "two.sided"` to test H0: importance \<=
  0 vs H1: importance \> 0, or H0: importance = 0 vs H1: importance !=
  0, respectively. For `"quantile"`, `alternative` controls whether the
  confidence interval is one-sided (`"greater"`: finite lower bound,
  `conf_upper = Inf`) or two-sided (both bounds finite).
- Improved documentation for all CI methods in
  `FeatureImportanceMethod`, explaining how p-values and confidence
  intervals are calculated for each method.
- CFI documentation distinguishes between CPI (knockoff-based inference,
  Watson & Wright 2021) and cARFi (ARF-based inference, Blesch et
  al. 2025).

## xplainfi 1.0.0 - Initial CRAN release

CRAN release: 2026-01-30

The major version bump is largely to mark the occasion that the package
is now considered “released”.

### Minor changes

- Removed the `fippy` comparison article since a more comprehensive
  comparison is now available in
  [xplainfi-benchmark](https://github.com/jemus42/xplainfi-benchmark).
- Clean up various documentation issues and other metadata.
- Adjusted the `min_permutations` default in `SAGE` methods to 10 rather
  than 3, since the previous value was found to lead to spurious early
  stopping.
- Fix `sim_dgp_ewald` lading to erroneous variances when compared to
  their settings.
- Reduce runtime of tests (mostly by using less ARF and mor Gaussian
  sampling)
- Remove `KnockoffSequentialSampler` as the `seqknockoff` package is not
  available on CRAN or R-universe. `KnockoffSampler` with the
  corresponding `knockoff_fun = seqknockoff::knockoffs_seq` still works.

## xplainfi 0.2.1

- Simplify `sim_dgp_confounded`, removing `x2` which doesn’t add
  anything interesting over `x1`.
- Ensure integers are preserved in Gaussian samplers
- Fix compatibility with mlr3 \>= 1.3.0 due to the change in the way
  `obs_loss()` is computed (see
  <https://github.com/mlr-org/mlr3/pull/1411>).
- Methods not allowing `measure` to be unspecified and falling back to a
  `task_type`-specific default measure

## xplainfi 0.2.0

### User-facing API improvements

#### Importance aggregation and confidence intervals

- `$importance()` gains `ci_method` parameter for variance estimation
  ([\#40](https://github.com/mlr-org/xplainfi/issues/40)):
  - `"none"` (default): Simple aggregation without confidence intervals
  - `"raw"`: Uncorrected variance estimates (informative only, CIs too
    narrow)
  - `"nadeau_bengio"`: Variance correction by Nadeau & Bengio (2003) as
    recommended by Molnar et al. (2023)
  - `"quantile"`: Empirical quantile-based confidence intervals
  - `"cpi"`: Conditional Predictive Impact for perturbation methods
    (PFI/CFI/RFI), supporting t-, Wilcoxon-, Fisher-, and binomial tests
- CPI is now properly scoped to `PerturbationImportance` methods only
  (not available for WVIM/LOCO or SAGE)
- `$importance()` gains `standardize` parameter to normalize scores to
  \[-1, 1\] range
- `$importance()` and `$scores()` gain `relation` parameter (default:
  `"difference"`) to compute importances as difference or ratio of
  baseline and post-modification loss
  - Moved from `$compute()` to avoid recomputing predictions/refits when
    changing aggregation method

#### Data simulation helpers

- Add focused simulation DGPs for testing importance methods:
  - [`sim_dgp_independent()`](https://mlr-org.github.io/xplainfi/reference/sim_dgp_scenarios.md):
    Baseline with additive independent effects
  - [`sim_dgp_correlated()`](https://mlr-org.github.io/xplainfi/reference/sim_dgp_scenarios.md):
    Highly correlated features (PFI fails, CFI succeeds)
  - [`sim_dgp_mediated()`](https://mlr-org.github.io/xplainfi/reference/sim_dgp_scenarios.md):
    Mediation structure (total vs direct effects)
  - [`sim_dgp_confounded()`](https://mlr-org.github.io/xplainfi/reference/sim_dgp_scenarios.md):
    Confounding structure
  - [`sim_dgp_interactions()`](https://mlr-org.github.io/xplainfi/reference/sim_dgp_scenarios.md):
    Interaction effects between features
- Each DGP illustrates specific methodological challenges for importance
  methods

#### Observation-wise losses and predictions

- `$obs_loss()` computes observation-wise importance scores when
  `measure` has a `Measure$obs_loss()` method
- `$predictions` field stores prediction objects for further analysis

#### Grouped feature importance

- `PerturbationImportance` and `WVIM` methods support `groups` parameter
  for grouped feature importance:
  - Example:
    `groups = list(effects = c("x1", "x2", "x3"), noise = c("noise1", "noise2"))`
  - In output, `feature` column contains group names instead of
    individual features
  - Allows measuring importance of feature sets rather than individual
    features

### Method-specific improvements

#### WVIM (Williamson’s Variable Importance Measure)

- Generalizes LOCO (Leave-One-Covariate-Out) and LOCI
  (Leave-One-Covariate-In)
- Implemented using `mlr3fselect` for cleaner internals
- Parameter renamed: `iters_refit` → `n_repeats` for consistency

#### PerturbationImportance (PFI, CFI, RFI)

- **Performance improvements**:
  - Uses `learner$predict_newdata_fast()` for faster predictions
    (requires mlr3 \>= 1.1.0)
  - Batches permutation iterations internally to reduce
    `sampler$sample()` calls
  - New `batch_size` parameter to control memory usage with large
    datasets
- **Parallelization support**:
  - Parallel execution via `mirai` or `future` backends
  - Set up with
    [`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html)
    or
    [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  - Parallelizes across features within each resampling iteration
- Parameter renamed: `iters_perm` → `n_repeats` for consistency

#### Feature Samplers

- **Breaking changes**:
  - Refactored API separates task-based vs external data sampling
    ([\#49](https://github.com/mlr-org/xplainfi/issues/49)):
    - `$sample(feature, row_ids)`: Samples from stored task using row
      IDs
    - `$sample_newdata(feature, newdata)`: Samples from external data
  - Renamed sampler classes for hierarchical consistency:
    - `PermutationSampler` → `MarginalPermutationSampler`
    - `ARFSampler` → `ConditionalARFSampler`
    - `GaussianConditionalSampler` → `ConditionalGaussianSampler`
    - `KNNConditionalSampler` → `ConditionalKNNSampler`
    - `CtreeConditionalSampler` → `ConditionalCtreeSampler`
  - Standardized parameter name: `conditioning_set` for features to
    condition on
- **New samplers**:
  - `MarginalSampler`: Base class for marginal sampling methods
  - `MarginalReferenceSampler`: Samples complete rows from reference
    data (for SAGE)
  - `KnockoffSampler`: Knockoff-based sampling
    ([\#16](https://github.com/mlr-org/xplainfi/issues/16) via
    [@mnwright](https://github.com/mnwright))
    - Convenience wrappers: `KnockoffGaussianSampler`,
      `KnockoffSequentialSampler`
    - Supports `row_ids`-based sampling
    - `iters` parameter for multiple knockoff iterations
    - Compatible with CFI (not RFI/SAGE)

#### SAGE (Shapley Additive Global Importance)

- **Bug fix**: `ConditionalSAGE` now properly uses conditional sampling
  (was accidentally using marginal sampling)

- **Performance improvements**:

  - Uses `learner$predict_newdata_fast()` for faster predictions
  - `batch_size` parameter controls memory usage for large coalitions

- **Convergence tracking**
  ([\#29](https://github.com/mlr-org/xplainfi/issues/29),
  [\#33](https://github.com/mlr-org/xplainfi/issues/33)):

  - Enable with `early_stopping = TRUE`
  - Stops when relative standard error falls below `se_threshold`
    (default: 0.01)
  - Requires at least `min_permutations` (default: 3)
  - Checks convergence every `check_interval` permutations (default: 1)
  - New fields:
    - `$converged`: Boolean indicating if convergence was reached
    - `$n_permutations_used`: Actual permutations used (may be less than
      requested)
    - `$convergence_history`: Per-feature importance and SE over
      permutations
  - `$plot_convergence()`: Visualize convergence curves
  - Convergence tracked for first resampling iteration only

## xplainfi 0.1.0

- Initial prototype with
  - PFI
  - CFI and RFI (via `arf`-powered conditional sampling)
  - SAGE (marginal and conditional, the latter via `arf`)
  - LOCO and LOCI
- Includes comparison to reference implementation in Python via `fippy`

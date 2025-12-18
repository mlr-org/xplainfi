# xplainfi 0.2.1

- Simplify `sim_dgp_confounded`, removing `x2` which doesn't add anything interesting over `x1`.
- Ensure integers are preserved in Gaussian samplers
- Fix compatibility with mlr3 >= 1.3.0 due to the change in the way `obs_loss()` is computed (see https://github.com/mlr-org/mlr3/pull/1411).
- Methods not allowing `measure` to be unspecified and falling back to a `task_type`-specific default measure

# xplainfi 0.2.0

## User-facing API improvements

### Importance aggregation and confidence intervals

- `$importance()` gains `ci_method` parameter for variance estimation (#40):
  - `"none"` (default): Simple aggregation without confidence intervals
  - `"raw"`: Uncorrected variance estimates (informative only, CIs too narrow)
  - `"nadeau_bengio"`: Variance correction by Nadeau & Bengio (2003) as recommended by Molnar et al. (2023)
  - `"quantile"`: Empirical quantile-based confidence intervals
  - `"cpi"`: Conditional Predictive Impact for perturbation methods (PFI/CFI/RFI), supporting t-, Wilcoxon-, Fisher-, and binomial tests
- CPI is now properly scoped to `PerturbationImportance` methods only (not available for WVIM/LOCO or SAGE)
- `$importance()` gains `standardize` parameter to normalize scores to [-1, 1] range
- `$importance()` and `$scores()` gain `relation` parameter (default: `"difference"`) to compute importances as difference or ratio of baseline and post-modification loss
  - Moved from `$compute()` to avoid recomputing predictions/refits when changing aggregation method

### Data simulation helpers

- Add focused simulation DGPs for testing importance methods:
  - `sim_dgp_independent()`: Baseline with additive independent effects
  - `sim_dgp_correlated()`: Highly correlated features (PFI fails, CFI succeeds)
  - `sim_dgp_mediated()`: Mediation structure (total vs direct effects)
  - `sim_dgp_confounded()`: Confounding structure
  - `sim_dgp_interactions()`: Interaction effects between features
- Each DGP illustrates specific methodological challenges for importance methods

### Observation-wise losses and predictions

- `$obs_loss()` computes observation-wise importance scores when `measure` has a `Measure$obs_loss()` method
- `$predictions` field stores prediction objects for further analysis

### Grouped feature importance

- `PerturbationImportance` and `WVIM` methods support `groups` parameter for grouped feature importance:
  - Example: `groups = list(effects = c("x1", "x2", "x3"), noise = c("noise1", "noise2"))`
  - In output, `feature` column contains group names instead of individual features
  - Allows measuring importance of feature sets rather than individual features

## Method-specific improvements

### WVIM (Williamson's Variable Importance Measure)

- Generalizes LOCO (Leave-One-Covariate-Out) and LOCI (Leave-One-Covariate-In)
- Implemented using `mlr3fselect` for cleaner internals
- Parameter renamed: `iters_refit` → `n_repeats` for consistency

### PerturbationImportance (PFI, CFI, RFI)

- **Performance improvements**:
  - Uses `learner$predict_newdata_fast()` for faster predictions (requires mlr3 >= 1.1.0)
  - Batches permutation iterations internally to reduce `sampler$sample()` calls
  - New `batch_size` parameter to control memory usage with large datasets

- **Parallelization support**:
  - Parallel execution via `mirai` or `future` backends
  - Set up with `mirai::daemons()` or `future::plan()`
  - Parallelizes across features within each resampling iteration

- Parameter renamed: `iters_perm` → `n_repeats` for consistency

### Feature Samplers

- **Breaking changes**:
  - Refactored API separates task-based vs external data sampling (#49):
    - `$sample(feature, row_ids)`: Samples from stored task using row IDs
    - `$sample_newdata(feature, newdata)`: Samples from external data
  - Renamed sampler classes for hierarchical consistency:
    - `PermutationSampler` → `MarginalPermutationSampler`
    - `ARFSampler` → `ConditionalARFSampler`
    - `GaussianConditionalSampler` → `ConditionalGaussianSampler`
    - `KNNConditionalSampler` → `ConditionalKNNSampler`
    - `CtreeConditionalSampler` → `ConditionalCtreeSampler`
  - Standardized parameter name: `conditioning_set` for features to condition on

- **New samplers**:
  - `MarginalSampler`: Base class for marginal sampling methods
  - `MarginalReferenceSampler`: Samples complete rows from reference data (for SAGE)
  - `KnockoffSampler`: Knockoff-based sampling (#16 via @mnwright)
    - Convenience wrappers: `KnockoffGaussianSampler`, `KnockoffSequentialSampler`
    - Supports `row_ids`-based sampling
    - `iters` parameter for multiple knockoff iterations
    - Compatible with CFI (not RFI/SAGE)

### SAGE (Shapley Additive Global Importance)

- **Bug fix**: `ConditionalSAGE` now properly uses conditional sampling (was accidentally using marginal sampling)

- **Performance improvements**:
  - Uses `learner$predict_newdata_fast()` for faster predictions
  - `batch_size` parameter controls memory usage for large coalitions

- **Convergence tracking** (#29, #33):
  - Enable with `early_stopping = TRUE`
  - Stops when relative standard error falls below `se_threshold` (default: 0.01)
  - Requires at least `min_permutations` (default: 3)
  - Checks convergence every `check_interval` permutations (default: 1)
  - New fields:
    - `$converged`: Boolean indicating if convergence was reached
    - `$n_permutations_used`: Actual permutations used (may be less than requested)
    - `$convergence_history`: Per-feature importance and SE over permutations
  - `$plot_convergence()`: Visualize convergence curves
  - Convergence tracked for first resampling iteration only


# xplainfi 0.1.0

- Initial prototype with 
	- PFI
	- CFI and RFI (via `arf`-powered conditional sampling)
	- SAGE (marginal and conditional, the latter via `arf`)
	- LOCO and LOCI
- Includes comparison to reference implementation in Python via `fippy`

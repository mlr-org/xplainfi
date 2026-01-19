# Package index

## Importance Measures

Base class

- [`FeatureImportanceMethod`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.md)
  : Feature Importance Method Class

### Perturbation-Based Importance Measures

Methods which perturb features of interest either marginally (PFI) or
conditionally on all (CFI) or a subset of remaining features (RFI)

- [`PerturbationImportance`](https://mlr-org.github.io/xplainfi/reference/PerturbationImportance.md)
  : Perturbation Feature Importance Base Class
- [`PFI`](https://mlr-org.github.io/xplainfi/reference/PFI.md) :
  Permutation Feature Importance
- [`CFI`](https://mlr-org.github.io/xplainfi/reference/CFI.md) :
  Conditional Feature Importance
- [`RFI`](https://mlr-org.github.io/xplainfi/reference/RFI.md) :
  Relative Feature Importance

### Model Refitting Measures

Methods which refit models with one (or more) features omitted (LOCO) or
included (LOCI).

- [`WVIM`](https://mlr-org.github.io/xplainfi/reference/WVIM.md) :
  Williamson's Variable Importance Measure (WVIM)
- [`LOCO`](https://mlr-org.github.io/xplainfi/reference/LOCO.md) :
  Leave-One-Covariate-Out (LOCO)

### Shapley-Based Approaches

Shapley Additive Global Importance (SAGE) in marginal and conditional
variants

- [`SAGE`](https://mlr-org.github.io/xplainfi/reference/SAGE.md) :
  Shapley Additive Global Importance (SAGE) Base Class
- [`MarginalSAGE`](https://mlr-org.github.io/xplainfi/reference/MarginalSAGE.md)
  : Marginal SAGE
- [`ConditionalSAGE`](https://mlr-org.github.io/xplainfi/reference/ConditionalSAGE.md)
  : Conditional SAGE

## Sampling Infrastructure

Base class and three families of samplers (Marginal, Conditional,
Knockoff)

### Base classes

Abstract base classes for the three sampler families. FeatureSampler is
the top-level base class, MarginalSampler is for marginal sampling
methods, and ConditionalSampler is for conditional sampling methods.

- [`FeatureSampler`](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.md)
  : Feature Sampler Class
- [`MarginalSampler`](https://mlr-org.github.io/xplainfi/reference/MarginalSampler.md)
  : Marginal Sampler Base Class
- [`ConditionalSampler`](https://mlr-org.github.io/xplainfi/reference/ConditionalSampler.md)
  : Conditional Feature Sampler

### Marginal sampling (no conditioning)

Samplers that draw from the marginal distribution P(X_S) without
conditioning on other features.

- [`MarginalPermutationSampler`](https://mlr-org.github.io/xplainfi/reference/MarginalPermutationSampler.md)
  : Marginal Permutation Sampler
- [`MarginalReferenceSampler`](https://mlr-org.github.io/xplainfi/reference/MarginalReferenceSampler.md)
  : Marginal Reference Sampler

### Conditional sampling (with conditioning)

Samplers that draw from the conditional distribution P(X_S \| X_C) where
X_C is an arbitrary conditioning set. The conditioning set can be
specified via the `conditioning_set` parameter.

- [`ConditionalARFSampler`](https://mlr-org.github.io/xplainfi/reference/ConditionalARFSampler.md)
  : ARF-based Conditional Sampler
- [`ConditionalGaussianSampler`](https://mlr-org.github.io/xplainfi/reference/ConditionalGaussianSampler.md)
  : Gaussian Conditional Sampler
- [`ConditionalKNNSampler`](https://mlr-org.github.io/xplainfi/reference/ConditionalKNNSampler.md)
  : k-Nearest Neighbors Conditional Sampler
- [`ConditionalCtreeSampler`](https://mlr-org.github.io/xplainfi/reference/ConditionalCtreeSampler.md)
  : (experimental) Conditional Inference Tree Conditional Sampler

### Knockoff sampling

Knockoffs satisfy certain theoretical properties that exceed those of
the conditional samplers. They generate one (or more) knockoff matrix on
construction, and sampling is always performed in reference to these.
Unlike other samplers, they do not allow sampling from new data with
`$sample_newdata()`, as that would require re-creating a knockoff matrix
on the fly.

- [`KnockoffSampler`](https://mlr-org.github.io/xplainfi/reference/KnockoffSampler.md)
  : Knockoff-based Conditional Sampler
- [`KnockoffGaussianSampler`](https://mlr-org.github.io/xplainfi/reference/KnockoffGaussianSampler.md)
  : Gaussian Knockoff Conditional Sampler
- [`KnockoffSequentialSampler`](https://mlr-org.github.io/xplainfi/reference/KnockoffSequentialSampler.md)
  : Sequential Knockoff Conditional Sampler

## Utilities

- [`wvim_design_matrix()`](https://mlr-org.github.io/xplainfi/reference/wvim_design_matrix.md)
  : Create Feature Selection Design Matrix

- [`check_groups()`](https://mlr-org.github.io/xplainfi/reference/check_groups.md)
  : Check group specification

- [`` `%||%` ``](https://mlr-org.github.io/xplainfi/reference/op-null-default.md)
  :

  Default value for `NULL`

- [`xplain_opt()`](https://mlr-org.github.io/xplainfi/reference/xplain_opt.md)
  : xplainfi Package Options

## Data simulation

- [`sim_dgp_ewald()`](https://mlr-org.github.io/xplainfi/reference/sim_dgp_ewald.md)
  : Simulate data as in Ewald et al. (2024)
- [`sim_dgp_correlated()`](https://mlr-org.github.io/xplainfi/reference/sim_dgp_scenarios.md)
  [`sim_dgp_mediated()`](https://mlr-org.github.io/xplainfi/reference/sim_dgp_scenarios.md)
  [`sim_dgp_confounded()`](https://mlr-org.github.io/xplainfi/reference/sim_dgp_scenarios.md)
  [`sim_dgp_interactions()`](https://mlr-org.github.io/xplainfi/reference/sim_dgp_scenarios.md)
  [`sim_dgp_independent()`](https://mlr-org.github.io/xplainfi/reference/sim_dgp_scenarios.md)
  : Simulation DGPs for Feature Importance Method Comparison

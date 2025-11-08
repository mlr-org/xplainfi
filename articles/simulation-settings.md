# Simulation Settings for Feature Importance Methods

``` r
library(xplainfi)
library(DiagrammeR)
library(mlr3learners)
#> Loading required package: mlr3
set.seed(123)
```

## Introduction

The `xplainfi` package provides several data generating processes (DGPs)
designed to illustrate specific strengths and weaknesses of different
feature importance methods. Each DGP focuses on one primary challenge to
make the differences between methods clear.

This article provides a comprehensive overview of all simulation
settings, including their mathematical formulations and causal
structures visualized as directed acyclic graphs (DAGs).

## Overview of Simulation Settings

| DGP                  | Challenge                | PFI Behavior          | CFI Behavior                 |
|:---------------------|:-------------------------|:----------------------|:-----------------------------|
| sim_dgp_correlated   | Spurious correlation     | High for spurious x2  | Low for spurious x2          |
| sim_dgp_mediated     | Mediation effects        | Shows total effects   | Shows direct effects         |
| sim_dgp_confounded   | Confounding              | Biased upward         | Less biased                  |
| sim_dgp_interactions | Interaction effects      | Low (no main effects) | High (captures interactions) |
| sim_dgp_independent  | Baseline (no challenges) | Accurate              | Accurate                     |
| sim_dgp_ewald        | Mixed effects            | Mixed                 | Mixed                        |

Overview of simulation settings and expected method behavior

## 1. Correlated Features DGP

This DGP creates a highly correlated spurious predictor to illustrate
the fundamental difference between marginal and conditional importance
methods.

### Mathematical Model

\\(X_1, X_2)^T \sim \text{MVN}(0, \Sigma)\\

where \\\Sigma\\ is a 2×2 covariance matrix with 1 on the diagonal and
correlation \\r\\ (default 0.9) on the off-diagonal.

\\X_3 \sim N(0,1), \quad X_4 \sim N(0,1)\\ \\Y = 2 \cdot X_1 + X_3 +
\varepsilon\\

where \\\varepsilon \sim N(0, 0.2^2)\\.

### Causal Structure

DAG for correlated features DGP

### Usage Example

``` r
set.seed(123)
task <- sim_dgp_correlated(n = 500)

# Check correlation between X1 and X2
cor(task$data()[, c("x1", "x2")])
#>          x1       x2
#> x1 1.000000 0.885718
#> x2 0.885718 1.000000

# True coefficients: x1=2.0, x2=0, x3=1.0, x4=0
# Note: x2 is highly correlated with x1 but has NO causal effect!
```

### Expected Behavior

- **Marginal methods (PFI, Marginal SAGE)**: Will falsely assign high
  importance to x2 because permuting it breaks the correlation with x1,
  creating unrealistic data that confuses the model
- **Conditional methods (CFI, Conditional SAGE)**: Should correctly
  assign near-zero importance to x2 because conditional sampling
  preserves the correlation, revealing that x2 adds no information
  beyond what x1 provides
- **Key insight**: x2 is a spurious predictor - it appears predictive
  due to correlation with x1 but has no causal effect on y

## 2. Mediated Effects DGP

This DGP demonstrates the difference between total and direct causal
effects. Some features affect the outcome only through mediators.

### Mathematical Model

\\\text{exposure} \sim N(0,1), \quad \text{direct} \sim N(0,1)\\
\\\text{mediator} = 0.8 \cdot \text{exposure} + 0.6 \cdot
\text{direct} + \varepsilon_m\\ \\Y = 1.5 \cdot \text{mediator} + 0.5
\cdot \text{direct} + \varepsilon\\

where \\\varepsilon_m \sim N(0, 0.3^2)\\ and \\\varepsilon \sim N(0,
0.2^2)\\.

### Causal Structure

DAG for mediated effects DGP

### Usage Example

``` r
set.seed(123)
task <- sim_dgp_mediated(n = 500)

# Calculate total effect of exposure
# Total effect = 0.8 * 1.5 = 1.2 (through mediator)
# Direct effect = 0 (no direct path to Y)
```

### Expected Behavior

- **PFI**: Shows total effects (exposure appears important with effect ≈
  1.2)
- **CFI**: Shows direct effects (exposure appears unimportant when
  conditioning on mediator)
- **RFI with mediator**: Should show direct effects similar to CFI

## 3. Confounding DGP

This DGP includes a confounder that affects both features and the
outcome.

### Mathematical Model

\\H \sim N(0,1) \quad \text{(confounder)}\\ \\X_1 = H + \varepsilon_1,
\quad X_2 = H + \varepsilon_2\\ \\\text{proxy} = H + \varepsilon_p,
\quad \text{independent} \sim N(0,1)\\ \\Y = H + 0.5 \cdot X_1 + 0.5
\cdot X_2 + \text{independent} + \varepsilon\\

where all \\\varepsilon \sim N(0, 0.5^2)\\ independently.

### Causal Structure

DAG for confounding DGP

### Usage Example

``` r
set.seed(123)
# Hidden confounder scenario (default)
task_hidden <- sim_dgp_confounded(n = 500, hidden = TRUE)
task_hidden$feature_names # proxy available but not confounder
#> [1] "independent" "proxy"       "x1"          "x2"

# Observable confounder scenario
task_observed <- sim_dgp_confounded(n = 500, hidden = FALSE)
task_observed$feature_names # both confounder and proxy available
#> [1] "confounder"  "independent" "proxy"       "x1"          "x2"
```

### Expected Behavior

- **PFI**: Will show inflated importance for x1 and x2 due to
  confounding
- **CFI**: Should partially account for confounding through conditional
  sampling
- **RFI conditioning on proxy**: Should reduce confounding bias

## 4. Interaction Effects DGP

This DGP demonstrates a pure interaction effect where features have no
main effects.

### Mathematical Model

\\Y = 2 \cdot X_1 \cdot X_2 + X_3 + \varepsilon\\

where \\X_j \sim N(0,1)\\ independently and \\\varepsilon \sim N(0,
0.5^2)\\.

### Causal Structure

DAG for interaction effects DGP

### Usage Example

``` r
set.seed(123)
task <- sim_dgp_interactions(n = 500)

# Note: X1 and X2 have NO main effects
# Their importance comes ONLY through their interaction
```

### Expected Behavior

- **PFI**: Should assign near-zero importance to x1 and x2 (no marginal
  effect)
- **CFI**: Should capture the interaction and assign high importance to
  x1 and x2
- **LOCO**: May show high importance for x1 and x2 (removing either
  breaks the interaction)
- **LOCI**: Should show near-zero importance for x1 and x2 (individually
  useless)

## 5. Independent Features DGP (Baseline)

This is a baseline scenario where all features are independent and their
effects are additive. All importance methods should give similar
results.

### Mathematical Model

\\Y = 2.0 \cdot X_1 + 1.0 \cdot X_2 + 0.5 \cdot X_3 + \varepsilon\\

where \\X_j \sim N(0,1)\\ independently and \\\varepsilon \sim N(0,
0.2^2)\\.

### Causal Structure

DAG for independent features DGP

### Usage Example

``` r
set.seed(123)
task <- sim_dgp_independent(n = 500)

# All methods should rank features consistently:
# important1 > important2 > important3 > unimportant1,2 ≈ 0
```

### Expected Behavior

- **All methods**: Should rank features consistently by their true
  effect sizes
- **Ground truth**: important1 (2.0) \> important2 (1.0) \> important3
  (0.5) \> unimportant1,2 (0)

## 6. Ewald et al. (2024) DGP

Reproduces the data generating process from Ewald et al. (2024) for
benchmarking feature importance methods. Includes correlated features
and interaction effects.

### Mathematical Model

\\X_1, X_3, X_5 \sim \text{Uniform}(0,1)\\ \\X_2 = X_1 + \varepsilon_2,
\quad \varepsilon_2 \sim N(0, 0.001)\\ \\X_4 = X_3 + \varepsilon_4,
\quad \varepsilon_4 \sim N(0, 0.1)\\ \\Y = X_4 + X_5 + X_4 \cdot X_5 +
\varepsilon, \quad \varepsilon \sim N(0, 0.1)\\

### Causal Structure

DAG for Ewald et al. (2024) DGP

### Usage Example

``` r
sim_dgp_ewald(n = 500)
#> 
#> ── <TaskRegr> (500x6) ──────────────────────────────────────────────────────────
#> • Target: y
#> • Properties: -
#> • Features (5):
#>   • dbl (5): x1, x2, x3, x4, x5
```

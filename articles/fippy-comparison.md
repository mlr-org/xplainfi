# Comparison with fippy (Python Implementation)

``` r
library(data.table)
library(ggplot2)
library(jsonlite)
library(knitr)
library(glue)
library(here)

# Set seed for reproducibility
set.seed(123)
```

## Overview

This article compares xplainfi’s feature importance implementations with
those from [fippy](https://github.com/gcskoenig/fippy), a Python package
implementing similar methods. This comparison serves as a regression
test to ensure methodological consistency across language
implementations.

The comparison includes:

- **PFI** (Permutation Feature Importance)
- **CFI** (Conditional Feature Importance)
- **RFI** (Relative Feature Importance)
- **SAGE** (Shapley Additive Global Importance) - both marginal and
  conditional variants

## Methodology

Both implementations use:

- **Dataset**: Ewald et al. simulation with 5000 observations
  ([`sim_dgp_ewald()`](https://jemus42.github.io/xplainfi/reference/sim_dgp_ewald.md))
- **Evaluation**: Same train/test data (70% train)
- **Metrics**: Mean Squared Error for importance calculations

The Ewald simulation provides an interpretable test case where we can
better understand expected feature importance patterns, particularly for
conditional methods that account for feature dependencies.

Due to the difference in underlying conditional samplers (ARF for
xplainfi vs Gaussian samplers in fippy) we expect conditional methods to
show more variation than marginal ones.

## Setup and Execution

The comparison uses separate calculation scripts:

``` bash
# 1. Calculate xplainfi results
cd vignettes/articles/fippy-comparison
Rscript calculate_xplainfi.R

# 2. Calculate fippy results (portable with uv - automatically installs dependencies)
./calculate_fippy.py
```

Both scripts generate JSON files with results that are loaded below for
comparison.

### Expected Feature Importance Patterns

The Ewald simulation (`sim_dgp_ewald`) generates a regression task with
5 features (x1-x5) where:

- All features contribute to the target variable, but with different
  weights
- Some features are correlated, making conditional methods particularly
  interesting
- The known data generating process allows us to validate whether the
  methods identify sensible patterns

See [the article for more details on the
DGP](https://jemus42.github.io/xplainfi/articles/simulation-settings.html#ewald-et-al--2024-dgp)

## Load Results

``` r
# Check that both result files exist
# Look in the fippy-comparison subdirectory
base_dir <- here::here("vignettes", "articles", "fippy-comparison")
xplainfi_results_path <- file.path(base_dir, "xplainfi_results.json")
fippy_results_path <- file.path(base_dir, "fippy_results.json")

if (!file.exists(xplainfi_results_path)) {
    stop("xplainfi_results.json not found. Please run calculate_xplainfi.R first.")
}

if (!file.exists(fippy_results_path)) {
    stop("fippy_results.json not found. Please run calculate_fippy.py first.")
}

# Load results from both implementations
xplainfi_results <- fromJSON(xplainfi_results_path)
fippy_results <- fromJSON(fippy_results_path)
```

## Model Performance Comparison

``` r
performance_comparison <- data.table(
    Implementation = c("xplainfi (R)", "fippy (Python)"),
    R_squared = c(
        round(xplainfi_results$model_performance$r_squared, 4),
        round(fippy_results$model_performance$r_squared, 4)
    )
)

kable(performance_comparison, caption = "Model Performance Comparison")
```

| Implementation | R_squared |
|:---------------|----------:|
| xplainfi (R)   |    0.8409 |
| fippy (Python) |    0.8361 |

Model Performance Comparison

## Method Comparisons

``` r
compare_method <- function(method_name, xplainfi_result, fippy_result) {
    # Both implementations available
    method_dt <- data.table(
        feature = xplainfi_result$feature,
        xplainfi = xplainfi_result$importance,
        fippy = fippy_result$importance
    )

    # Return table and correlation for display
    correlation <- cor(method_dt$xplainfi, method_dt$fippy)
    correlation_spearman = cor(method_dt$xplainfi, method_dt$fippy, method = "spearman")

    list(
        method = method_name,
        table = kable(
            method_dt[order(-xplainfi)],
            caption = glue("{method_name} Results Comparison"),
            digits = 4
        ),
        correlation = correlation,
        correlation_spearman = correlation_spearman
    )
}
```

### PFI (Permutation Feature Importance)

``` r
pfi_result <- compare_method("PFI", xplainfi_results$PFI, fippy_results$PFI)
pfi_result$table
```

| feature | xplainfi |   fippy |
|:--------|---------:|--------:|
| x4      |   0.6955 |  0.8276 |
| x5      |   0.3754 |  0.3958 |
| x3      |   0.0109 |  0.0024 |
| x2      |  -0.0003 | -0.0006 |
| x1      |  -0.0008 | -0.0004 |

PFI Results Comparison

### CFI (Conditional Feature Importance)

``` r
cfi_result <- compare_method("CFI", xplainfi_results$CFI, fippy_results$CFI)
cfi_result$table
```

| feature | xplainfi |   fippy |
|:--------|---------:|--------:|
| x4      |   0.4104 |  0.4496 |
| x5      |   0.3427 |  0.3548 |
| x3      |   0.0011 |  0.0012 |
| x2      |   0.0003 |  0.0001 |
| x1      |  -0.0003 | -0.0002 |

CFI Results Comparison

### RFI (Relative Feature Importance)

``` r
rfi_result <- compare_method("RFI", xplainfi_results$RFI, fippy_results$RFI)
rfi_result$table
```

| feature | xplainfi |   fippy |
|:--------|---------:|--------:|
| x4      |   0.4112 |  0.4460 |
| x5      |   0.3434 |  0.3552 |
| x3      |   0.0000 |  0.0000 |
| x2      |  -0.0003 | -0.0006 |
| x1      |  -0.0005 | -0.0003 |

RFI Results Comparison

``` r
glue("xplainfi conditioning set: {paste(xplainfi_results$RFI$conditioning_set, collapse = ', ')}")
```

    ## xplainfi conditioning set: x3

``` r
glue("fippy conditioning set: {paste(fippy_results$RFI$conditioning_set, collapse = ', ')}")
```

    ## fippy conditioning set: x3

### SAGE Methods

#### Marginal SAGE

``` r
sage_marginal_result <- compare_method(
    "Marginal SAGE",
    xplainfi_results$SAGE_Marginal,
    fippy_results$SAGE_Marginal
)
sage_marginal_result$table
```

| feature | xplainfi |   fippy |
|:--------|---------:|--------:|
| x4      |   0.3770 |  0.4386 |
| x5      |   0.1817 |  0.1963 |
| x3      |   0.0188 | -0.0007 |
| x1      |   0.0002 |  0.0006 |
| x2      |   0.0000 |  0.0001 |

Marginal SAGE Results Comparison

#### Conditional SAGE

``` r
sage_conditional_result <- compare_method(
    "Conditional SAGE",
    xplainfi_results$SAGE_Conditional,
    fippy_results$SAGE_Conditional
)
sage_conditional_result$table
```

| feature | xplainfi |  fippy |
|:--------|---------:|-------:|
| x4      |   0.2869 | 0.3331 |
| x5      |   0.1804 | 0.1947 |
| x3      |   0.1083 | 0.1038 |
| x2      |   0.0004 | 0.0013 |
| x1      |  -0.0001 | 0.0017 |

Conditional SAGE Results Comparison

## Correlation Summary

``` r
correlations <- rbindlist(list(
    pfi_result[c("method", "correlation", "correlation_spearman")],
    cfi_result[c("method", "correlation", "correlation_spearman")],
    rfi_result[c("method", "correlation", "correlation_spearman")],
    sage_marginal_result[c("method", "correlation", "correlation_spearman")],
    sage_conditional_result[c("method", "correlation", "correlation_spearman")]
))

kable(
    correlations,
    digits = 4,
    caption = "Pearson and Spearman Correlations between xplainfi and fippy",
    col.names = c("Method", "Pearson Corr.", "Spearman Corr.")
)
```

| Method           | Pearson Corr. | Spearman Corr. |
|:-----------------|--------------:|---------------:|
| PFI              |        0.9983 |            0.9 |
| CFI              |        0.9994 |            1.0 |
| RFI              |        0.9996 |            0.9 |
| Marginal SAGE    |        0.9985 |            0.7 |
| Conditional SAGE |        0.9971 |            0.9 |

Pearson and Spearman Correlations between xplainfi and fippy

``` r
melt(correlations, id.vars = "method") |>
    ggplot(aes(x = reorder(method, value), y = value)) +
    facet_wrap(
        vars(variable),
        ncol = 1,
        labeller = as_labeller(c(correlation = "Pearson's", correlation_spearman = "Spearman's"))
    ) +
    geom_col(fill = "steelblue", alpha = 0.7) +
    geom_hline(yintercept = c(0.5, 1), linetype = "dashed", color = "red", alpha = 0.7) +
    coord_flip() +
    labs(
        title = "Implementation Correlations",
        subtitle = "xplainfi (R) vs fippy (Python)",
        x = "Method",
        y = "Correlation"
    ) +
    theme_minimal(base_size = 14) +
    theme(
        plot.title.position = "plot"
    )
```

![](fippy-comparison_files/figure-html/correlation-summary-1.png)

## Interpretation in Context of Ewald Simulation

The Ewald simulation provides interpretable feature importance patterns
that help validate both implementations:

![](fippy-comparison_files/figure-html/importancer-plot-1.png)

### Method-Specific Insights

**Marginal vs Conditional Methods:**

- **PFI** and **Marginal SAGE** ignore feature correlations
- **CFI**, **RFI**, and **Conditional SAGE** account for feature
  dependencies
- Differences between marginal and conditional variants reveal the
  impact of feature correlations

**RFI Conditioning:** Both implementations use `{x3}` as the
conditioning set.

**Cross-Implementation Consistency:** High correlations indicate that
both xplainfi and fippy identify similar underlying feature importance
patterns despite using different:

- Programming languages (R vs Python)
- Conditional sampling approaches (ARF vs Gaussian)
- Implementation details

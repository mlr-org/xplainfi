# Check group specification

Check group specification

## Usage

``` r
check_groups(groups, all_features)
```

## Arguments

- groups:

  (`list`) A (named) list of groups

- all_features:

  (character()) All available feature names from the task.

## Value

`group`, with each element now named.

## Examples

``` r
task <- sim_dgp_interactions(n = 100)
task$feature_names
#> [1] "noise1" "noise2" "x1"     "x2"     "x3"    

# Intended use
groups1 = list(effects = c("x1", "x2", "x3"), noise = c("noise1", "noise2"))
check_groups(groups1, task$feature_names)
#> $effects
#> [1] "x1" "x2" "x3"
#> 
#> $noise
#> [1] "noise1" "noise2"
#> 

# Names are auto-generated where needed
check_groups(list(a = "x1",  c("x2", "x1")), task$feature_names)
#> ! Feature is specified in multiple groups: "x1"
#> Not all groups are named
#> ℹ Group "2" is named automatically
#> $a
#> [1] "x1"
#> 
#> $GroupB
#> [1] "x2" "x1"
#> 

if (FALSE) { # \dontrun{
# Unexpected features
groups2 = list(effects = c("x1", "foo", "bar", "x1"))
check_groups(groupos1, task$feature_names)
# Too deeply nested
groups3 = list(effects = c("x1", "x2", "x3"), noise = c("noise1", list(c("noise2"))))
check_groups(groupos1, task$feature_names)
} # }
```

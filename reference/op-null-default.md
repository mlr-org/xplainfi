# Default value for `NULL`

A backport of `%||%` available in R versions from 4.4.0.

## Usage

``` r
x %||% y
```

## Arguments

- x, y:

  If `x` is NULL or length 0, will return `y`; otherwise returns `x`.

## Examples

``` r
1 %||% 2
#> [1] 1
NULL %||% 2
#> [1] 2
```

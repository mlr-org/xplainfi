# xplainfi Package Options

Get or set package-level options for xplainfi.

## Usage

``` r
xplain_opt(...)
```

## Arguments

- ...:

  Option names to retrieve (as character strings) or options to set (as
  named arguments).

  - To **get** an option: `xplain_opt("verbose")` returns the current
    value

  - To **set** an option: `xplain_opt(verbose = FALSE)` sets the value

  - To **get all** options: `xplain_opt()` returns a named list of all
    options

## Value

- When **getting** a single option: the option value (logical)

- When **getting** multiple options: a named list of option values

- When **setting** options: the previous values (invisibly)

## Details

Options can be set in three ways (in order of precedence):

1.  Using `xplain_opt(option_name = value)` (recommended)

2.  Using `options("xplain.option_name" = value)`

3.  Using environment variables `XPLAIN_OPTION_NAME=value`

### Available Options

|              |         |                                                                              |
|--------------|---------|------------------------------------------------------------------------------|
| Option       | Default | Description                                                                  |
| `verbose`    | `TRUE`  | Show informational messages (e.g., when using default measure or resampling) |
| `progress`   | `FALSE` | Show progress bars during computation                                        |
| `sequential` | `FALSE` | Force sequential execution (disable parallelization)                         |
| `debug`      | `FALSE` | Enable debug output for development and troubleshooting                      |

## Examples

``` r
# Get current value of an option
xplain_opt("verbose")
#> [1] TRUE

# Get all options
xplain_opt()
#> $verbose
#> [1] TRUE
#> 
#> $progress
#> [1] FALSE
#> 
#> $sequential
#> [1] FALSE
#> 
#> $debug
#> [1] FALSE
#> 

# Set an option (returns previous value invisibly)
old <- xplain_opt(verbose = FALSE)
xplain_opt("verbose")  # Now FALSE
#> [1] FALSE

# Restore previous value
xplain_opt(verbose = old$verbose)

# Temporary option change with withr
if (requireNamespace("withr", quietly = TRUE)) {
  withr::with_options(
    list("xplain.verbose" = FALSE),
    {
      # Code here runs with verbose = FALSE
      xplain_opt("verbose")
    }
  )
}
#> [1] FALSE
```

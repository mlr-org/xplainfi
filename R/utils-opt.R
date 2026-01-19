#' xplainfi Package Options
#'
#' Get or set package-level options for xplainfi.
#'
#' @param ... Option names to retrieve (as character strings) or options to set (as named arguments).
#'   - To **get** an option: `xplain_opt("verbose")` returns the current value
#'   - To **set** an option: `xplain_opt(verbose = FALSE)` sets the value
#'   - To **get all** options: `xplain_opt()` returns a named list of all options
#'
#' @details
#' Options can be set in three ways (in order of precedence):
#' 1. Using `xplain_opt(option_name = value)` (recommended)
#' 2. Using `options("xplain.option_name" = value)`
#' 3. Using environment variables `XPLAIN_OPTION_NAME=value`
#'
#' ## Available Options
#'
#' | Option | Default | Description |
#' |--------|---------|-------------|
#' | `verbose` | `TRUE` | Show informational messages (e.g., when using default measure or resampling) |
#' | `progress` | `FALSE` | Show progress bars during computation |
#' | `sequential` | `FALSE` | Force sequential execution (disable parallelization) |
#' | `debug` | `FALSE` | Enable debug output for development and troubleshooting |
#'
#' @return
#' - When **getting** a single option: the option value (logical)
#' - When **getting** multiple options: a named list of option values
#' - When **setting** options: the previous values (invisibly)
#'
#' @examples
#' # Get current value of an option
#' xplain_opt("verbose")
#'
#' # Get all options
#' xplain_opt()
#'
#' # Set an option (returns previous value invisibly)
#' old <- xplain_opt(verbose = FALSE)
#' xplain_opt("verbose")  # Now FALSE
#'
#' # Restore previous value
#' xplain_opt(verbose = old$verbose)
#'
#' # Temporary option change with withr
#' if (requireNamespace("withr", quietly = TRUE)) {
#'   withr::with_options(
#'     list("xplain.verbose" = FALSE),
#'     {
#'       # Code here runs with verbose = FALSE
#'       xplain_opt("verbose")
#'     }
#'   )
#' }
#'
#' @export
xplain_opt <- function(...) {
	# Define available options and their defaults
	option_defaults <- list(
		verbose = TRUE,
		progress = FALSE,
		sequential = FALSE,
		debug = FALSE
	)

	args <- list(...)

	# No arguments: return all options with current values
	if (length(args) == 0) {
		result <- lapply(names(option_defaults), function(opt) {
			get_option_value(opt, option_defaults[[opt]])
		})
		names(result) <- names(option_defaults)
		return(result)
	}

	# Check if we're getting or setting
	arg_names <- names(args)

	if (is.null(arg_names) || all(arg_names == "")) {
		# All unnamed arguments: getting values
		# xplain_opt("verbose") or xplain_opt("verbose", "progress")
		opts_to_get <- unlist(args)
		checkmate::assert_character(opts_to_get, min.len = 1)
		checkmate::assert_subset(opts_to_get, names(option_defaults))

		if (length(opts_to_get) == 1) {
			return(get_option_value(opts_to_get, option_defaults[[opts_to_get]]))
		} else {
			result <- lapply(opts_to_get, function(opt) {
				get_option_value(opt, option_defaults[[opt]])
			})
			names(result) <- opts_to_get
			return(result)
		}
	} else {
		# Named arguments: setting values
		# xplain_opt(verbose = FALSE, progress = TRUE)
		checkmate::assert_subset(arg_names, names(option_defaults))

		# Store old values to return
		old_values <- lapply(arg_names, function(opt) {
			get_option_value(opt, option_defaults[[opt]])
		})
		names(old_values) <- arg_names

		# Set new values
		for (opt in arg_names) {
			opt_list <- list(args[[opt]])
			names(opt_list) <- paste0("xplain.", tolower(opt))
			options(opt_list)
		}

		return(invisible(old_values))
	}
}

#' Get option value with precedence: R option > env var > default
#' @noRd
#' @keywords internal
get_option_value <- function(name, default) {
	opt <- getOption(paste0("xplain.", tolower(name)), default = NA)
	envvar <- Sys.getenv(toupper(paste0("xplain_", name)), unset = NA)

	opt <- as.logical(opt)
	if (is.na(opt)) {
		opt <- NULL
	}

	envvar <- as.logical(envvar)
	if (is.na(envvar)) {
		envvar <- NULL
	}

	# R option > env var > default
	opt %||% envvar %||% default
}

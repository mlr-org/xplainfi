test_that("xplain_opt() returns all options when called without arguments", {
	result <- xplain_opt()
	expect_type(result, "list")
	expect_true(all(c("verbose", "progress") %in% names(result)))
})

test_that("xplain_opt() gets single option value", {
	# verbose defaults to TRUE
	options("xplain.verbose" = NULL)
	Sys.unsetenv("XPLAIN_VERBOSE")
	expect_true(xplain_opt("verbose"))

	# progress defaults to FALSE
	options("xplain.progress" = NULL)
	Sys.unsetenv("XPLAIN_PROGRESS")
	expect_false(xplain_opt("progress"))
})

test_that("xplain_opt() gets multiple option values", {
	result <- xplain_opt("verbose", "progress")
	expect_type(result, "list")
	expect_length(result, 2)
	expect_named(result, c("verbose", "progress"))
})

test_that("xplain_opt() sets options and returns old values", {
	# Store current values
	old_verbose <- xplain_opt("verbose")

	# Set new value
	returned <- xplain_opt(verbose = FALSE)
	expect_type(returned, "list")
	expect_length(returned, 1)
	expect_equal(returned$verbose, old_verbose)

	# Verify it was set
	expect_false(xplain_opt("verbose"))

	# Restore
	xplain_opt(verbose = old_verbose)
	expect_equal(xplain_opt("verbose"), old_verbose)
})

test_that("xplain_opt() sets multiple options", {
	# Store current values
	old <- xplain_opt()

	# Set multiple
	xplain_opt(verbose = FALSE, progress = TRUE)

	expect_false(xplain_opt("verbose"))
	expect_true(xplain_opt("progress"))

	# Restore
	xplain_opt(verbose = old$verbose, progress = old$progress)
})

test_that("R option takes precedence over environment variable", {
	# Store current
	old_verbose <- xplain_opt("verbose")

	# Set env to TRUE, option to FALSE
	Sys.setenv("XPLAIN_VERBOSE" = "TRUE")
	options("xplain.verbose" = FALSE)

	expect_false(xplain_opt("verbose"))

	# Set env to FALSE, option to TRUE
	Sys.setenv("XPLAIN_VERBOSE" = "FALSE")
	options("xplain.verbose" = TRUE)

	expect_true(xplain_opt("verbose"))

	# Cleanup
	Sys.unsetenv("XPLAIN_VERBOSE")
	options("xplain.verbose" = old_verbose)
})

test_that("environment variable is used when R option is not set", {
	# Store current
	old_verbose <- xplain_opt("verbose")

	# Clear R option, set env
	options("xplain.verbose" = NULL)
	Sys.setenv("XPLAIN_VERBOSE" = "FALSE")

	expect_false(xplain_opt("verbose"))

	Sys.setenv("XPLAIN_VERBOSE" = "TRUE")
	expect_true(xplain_opt("verbose"))

	# Cleanup
	Sys.unsetenv("XPLAIN_VERBOSE")
	options("xplain.verbose" = old_verbose)
})

test_that("xplain_opt() errors on unknown options", {
	expect_error(xplain_opt("nonexistent"), "subset")
	expect_error(xplain_opt(nonexistent = TRUE), "subset")
})

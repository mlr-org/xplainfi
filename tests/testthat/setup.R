# https://testthat.r-lib.org/articles/special-files.html#setup-files
# Get current log threshold
old_threshold <- logger$threshold

# Reduce to warnings only for tests
lgr::get_logger("mlr3")$set_threshold("warn")

# Restore after tests
withr::defer(
	{
		lgr::get_logger("mlr3")$set_threshold(old_threshold)
	},
	teardown_env()
)

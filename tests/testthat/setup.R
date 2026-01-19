# https://testthat.r-lib.org/articles/special-files.html#setup-files

# Load mlr3 to avoid mlr3:: namespace clutter in tests
library(mlr3)

# Get current log threshold
logger = lgr::get_logger("mlr3")
old_threshold <- logger$threshold

# Reduce to warnings only for tests
logger$set_threshold("warn")

# Suppress xplainfi messages and progress during tests
old_opts <- xplain_opt(verbose = FALSE, progress = FALSE)

# Restore after tests
withr::defer(
	{
		lgr::get_logger("mlr3")$set_threshold(old_threshold)
		xplain_opt(verbose = old_opts$verbose, progress = old_opts$progress)
	},
	teardown_env()
)

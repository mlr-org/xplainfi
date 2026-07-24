# https://testthat.r-lib.org/articles/special-files.html#setup-files

# Load mlr3 to avoid mlr3:: namespace clutter in tests
library(mlr3)

# CRAN policy: at most 2 cores in tests. data.table (OpenMP) and ranger (own thread pool via arf) need capping;
# ranger uses std::thread, so OMP_THREAD_LIMIT in testthat.R does not cover it. Restored in teardown.
old_dt_threads = data.table::getDTthreads()
old_ranger_threads = getOption("ranger.num.threads")
data.table::setDTthreads(2)
options(ranger.num.threads = 2)

# Get current log threshold
logger = lgr::get_logger("mlr3")
old_threshold = logger$threshold

# Reduce to warnings only for tests
logger$set_threshold("warn")

# Suppress xplainfi messages and progress during tests
old_opts = xplain_opt(verbose = FALSE, progress = FALSE)

# Restore after tests
withr::defer(
  {
    lgr::get_logger("mlr3")$set_threshold(old_threshold)
    xplain_opt(verbose = old_opts$verbose, progress = old_opts$progress)
    data.table::setDTthreads(old_dt_threads)
    options(ranger.num.threads = old_ranger_threads)
  },
  teardown_env()
)

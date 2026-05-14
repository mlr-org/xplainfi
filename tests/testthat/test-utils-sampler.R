test_that(".replicate_evidence preserves data unchanged when samples_per_row == 1", {
	dt = data.table::data.table(x = 1:3, y = letters[1:3])
	out = xplainfi:::.replicate_evidence(dt, samples_per_row = 1L)
	expect_identical(out, dt)
})

test_that(".replicate_evidence produces draw-major output for samples_per_row > 1", {
	dt = data.table::data.table(x = 1:3, y = letters[1:3])
	out = xplainfi:::.replicate_evidence(dt, samples_per_row = 4L)
	expect_equal(nrow(out), 12L)
	# Draw-major: rows 1-3 = draw1 (x = 1:3), rows 4-6 = draw2 (x = 1:3), etc.
	expect_identical(out$x, rep(1:3, times = 4L))
	expect_identical(out$y, rep(letters[1:3], times = 4L))
})

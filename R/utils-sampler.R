# Replicate a data.table for use by samplers that don't natively support
# `samples_per_row > 1`. Returns rows in draw-major order:
#   if `data` has rows r1, r2, r3 and `samples_per_row = 2`, returns
#   r1, r2, r3, r1, r2, r3.
# Intended as an opt-in convenience for user-implemented samplers.
#
# @param data (`data.table`) Data to replicate.
# @param samples_per_row (`integer(1)`) Number of draw blocks.
# @return data.table with `nrow(data) * samples_per_row` rows.
# @noRd
.replicate_evidence = function(data, samples_per_row) {
	checkmate::assert_data_table(data)
	checkmate::assert_count(samples_per_row, positive = TRUE)
	if (samples_per_row == 1L) return(data)
	data[rep.int(seq_len(.N), times = samples_per_row)]
}

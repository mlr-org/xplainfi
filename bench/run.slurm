#!/bin/bash -l
#SBATCH --job-name=xplainfi-bench
#SBATCH --partition=compute
#SBATCH --cpus-per-task=4
#SBATCH --mem=8G
#SBATCH --time=01:00:00
#SBATCH --output=bench/slurm-%j.out
#SBATCH --error=bench/slurm-%j.err

# Offline runtime comparison of the samples_per_row / n_synth refactor.
#
# Submit from the package root:
#   sbatch bench/run.slurm
#
# Builds the pre-refactor commit and current HEAD with `R CMD INSTALL`
# (no network: dependencies come from the shared user library), then times
# CFI and ConditionalSAGE compute() for each build via bench/timing.R.
#
# `#!/bin/bash -l` makes this a login shell so `module` is defined
# (see the BIPS-cluster Slurm Usage notes for the rationale).

set -euo pipefail

cd "${SLURM_SUBMIT_DIR:-$(pwd)}"
module load R/4.6.0

# Commit before the samples_per_row plumbing started (CFI + ConditionalSAGE
# both still replicate evidence and call arf::forge(n_synth = 1)).
REF_PRE=aa88c624
REF_POST=$(git rev-parse HEAD)

SCRATCH=$(mktemp -d)
cleanup() {
	git worktree remove --force "$SCRATCH/wt-pre" 2>/dev/null || true
	git worktree remove --force "$SCRATCH/wt-post" 2>/dev/null || true
	rm -rf "$SCRATCH"
}
trap cleanup EXIT

LIB_PRE="$SCRATCH/lib-pre"
LIB_POST="$SCRATCH/lib-post"
mkdir -p "$LIB_PRE" "$LIB_POST"

echo "=== xplainfi bench ==="
echo "host:      $(hostname)"
echo "node:      ${SLURMD_NODENAME:-?}"
echo "cpus:      ${SLURM_CPUS_PER_TASK:-?}"
echo "branch:    $(git rev-parse --abbrev-ref HEAD)"
echo "REF_PRE:   $REF_PRE"
echo "REF_POST:  $REF_POST"
echo "R:         $(Rscript --version 2>&1)"
echo "date:      $(date --iso-8601=seconds)"
echo

echo "--- building pre ($REF_PRE) ---"
git worktree add --detach "$SCRATCH/wt-pre" "$REF_PRE"
R CMD INSTALL --library="$LIB_PRE" "$SCRATCH/wt-pre"

echo "--- building post ($REF_POST) ---"
git worktree add --detach "$SCRATCH/wt-post" "$REF_POST"
R CMD INSTALL --library="$LIB_POST" "$SCRATCH/wt-post"

mkdir -p bench/.results
rm -f bench/.results/*.rds

for m in cfi sage; do
	echo "--- timing $m (pre) ---"
	Rscript bench/timing.R "$LIB_PRE" "$m" pre
	echo "--- timing $m (post) ---"
	Rscript bench/timing.R "$LIB_POST" "$m" post
done

echo
echo "=== summary ==="
Rscript -e '
files = list.files("bench/.results", "\\.rds$", full.names = TRUE)
df = do.call(rbind, lapply(files, readRDS))
df = df[order(df$method, df$label), ]
print(df, row.names = FALSE)
'

echo
echo "=== done ==="

#!/bin/bash -l
#SBATCH --job-name=xplainfi-bench
#SBATCH --partition=compute
#SBATCH --cpus-per-task=4
#SBATCH --mem=8G
#SBATCH --time=01:00:00
#SBATCH --output=bench/slurm-%j.out
#SBATCH --error=bench/slurm-%j.err

# Submit from the package root:
#   sbatch bench/run.slurm
#
# Runs both `samples_per_row`-refactor benchmarks via cross::bench_branches().
# Output lands in bench/slurm-<jobid>.{out,err} (gitignored).
#
# Note: `#!/bin/bash -l` makes this a login shell so `module` is defined.
# See the BIPS-cluster Slurm Usage notes for the rationale.

set -euo pipefail

cd "${SLURM_SUBMIT_DIR:-$(pwd)}"

module load R/4.6.0

echo "=== xplainfi bench ==="
echo "host:       $(hostname)"
echo "node:       ${SLURMD_NODENAME:-?}"
echo "cpus:       ${SLURM_CPUS_PER_TASK:-?}"
echo "submit dir: ${SLURM_SUBMIT_DIR:-?}"
echo "branch:     $(git rev-parse --abbrev-ref HEAD)"
echo "head:       $(git rev-parse --short HEAD)"
echo "R:          $(Rscript --version 2>&1)"
echo "date:       $(date --iso-8601=seconds)"
echo

echo "--- CFI + ConditionalARFSampler ---"
Rscript bench/cfi-arf-scaling.R
echo

echo "--- ConditionalSAGE + ConditionalARFSampler ---"
Rscript bench/sage-arf-scaling.R
echo

echo "=== done ==="

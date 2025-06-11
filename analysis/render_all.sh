#! /bin/sh
#
# Override the CACHE environment variable if you do not want to use the embedded cached models.
: "${CACHE:=../.cache}"
# Set RELOO environment to TRUE if you want to run reloo (but beware that this will take days, if you run all models with RELOO=TRUE)
: "${RELOO:=FALSE}"
# CORES determines how many (up to CHAINS, default 4) parallel models to run
: "${CORES:=2}"
# THREADS determines how many threads to run in each model (default 4).
# As brms and Stan are quite CPU-bound, CORES*THREADS should not exceed the number of physical CPU cores available.
: "${THREADS:=4}"

SOURCE=analysis
OUTPUT=observability/output

cat<<!

        Learning Observability Tracing - Replication Package

Will render the Rmd files (possibly restricted via glob prefix PREFIX) in the
$SOURCE subdirectory, and place the corresponding output files in $OUTPUT.

Running the models without reloo active should be quite fast (finish within 15 minutes).
If you run with reloo, it might take up to an hour to complete.
See README.md for details on how to check status of model building.
!

for f in ${SOURCE}/${PREFIX}*.Rmd; do
    echo "============> Starting to generate file ${f} at:" $(date -Iseconds)
    R -e "rmarkdown::render(\"${f}\", params=list(cache=\"${CACHE}\", reloo=${RELOO}, cores=${CORES}, threads=${THREADS}), output_dir=\"${OUTPUT}\")" || echo "FAILED - please check your environment or settings"
    echo "============> Finished file ${f} at:" $(date -Iseconds)
done

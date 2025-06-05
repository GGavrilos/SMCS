#!/bin/bash
# First job - no dependencies
# option 'parsable' formats job id to number (required!) 
jid1=$(sbatch --job-name="job1" --parsable job1.sh)

# Second job collecting the results of first
sbatch --job-name="job2" --dependency=afterok:$jid1 job2.sh
#!/bin/bash
#SBATCH --mail-user=georgios.gavrilopoulos@stat.math.ethz.ch
#SBATCH --mail-type=end,fail
#SBATCH --job-name="Collect MSE Simulation"
#SBATCH --time=01:30:00
#SBATCH --mem-per-cpu=50000M
#SBATCH --partition=bdw



#### Your shell commands below this line ####
module load r
R CMD BATCH --no-save --no-restore Collect.R

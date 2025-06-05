#!/bin/bash
#SBATCH --mail-user=georgios.gavrilopoulos@stat.math.ethz.ch
#SBATCH --mail-type=end,fail
#SBATCH --job-name="MSE Simulation" 
#SBATCH --time=5:30:00
#SBATCH --mem-per-cpu=5000M
#SBATCH --partition=bdw
#SBATCH --array=1-1000


#### Your shell commands below this line ####
module load r
R CMD BATCH --no-save --no-restore Sim_squared_error.R
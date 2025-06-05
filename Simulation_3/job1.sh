#!/bin/bash
#SBATCH --mail-user=sebastian.arnold@stat.unibe.ch
#SBATCH --mail-type=end,fail
#SBATCH --job-name="Simulation_Study"
#SBATCH --time=00:10:00
#SBATCH --mem-per-cpu=50000M
#SBATCH --partition=epyc2
#SBATCH --array=1-100


#### Your shell commands below this line ####
module load R
R CMD BATCH --no-save --no-restore Sim3_repeated.R
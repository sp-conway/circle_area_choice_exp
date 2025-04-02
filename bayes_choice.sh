#!/bin/bash 
#SBATCH -c 16  # Number of Cores per Task
#SBATCH --mem=100g  # Requested Memory
#SBATCH --partition=cpu
#SBATCH --account=pi_alc_umass_edu
#SBATCH -t 15:00:00  # Job time limit
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
module load r-rocker-ml-verse/4.4.0+apptainer
Rscript analysis/bayes_choice_dm_partial_pooling_weighted.R
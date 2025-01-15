#!/bin/bash 
#SBATCH -c 50  # Number of Cores per Task
#SBATCH --mem=200g  # Requested Memory
#SBATCH --partition=cpu
#SBATCH --account=pi_alc_umass_edu
#SBATCH -t 24:00:00  # Job time limit
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
module load r-rocker-ml-verse/4.2.3+apptainer
Rscript bayes_circle_area.R
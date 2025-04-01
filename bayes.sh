#!/bin/bash 
#SBATCH -c 100  # Number of Cores per Task
#SBATCH --mem=400g  # Requested Memory
#SBATCH --partition=cpu
#SBATCH --account=pi_alc_umass_edu
#SBATCH -t 48:00:00  # Job time limit
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
module load r-rocker-ml-verse/4.4.0+apptainer
Rscript analysis/bayes_circle_area.R 
Rscript analysis/bayes_circle_area_analyze_model.R 
Rscript analysis/bayes_circle_area_analyze_mu.R 
Rscript analysis/bayes_circle_area_plot_omega.R
Rscript analysis/bayes_circle_area_check_mu.R 
Rscript analysis/bayes_circle_area_plot_mu.R
Rscript analysis/bayes_circle_area_sim_choice.R 
Rscript analysis/bayes_circle_area_probe_omega_sigma_constant.R 
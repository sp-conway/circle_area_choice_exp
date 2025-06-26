# setup =================================================================================
rm(list=ls())
library(here)
library(tidyverse)
library(fs)
library(glue)
library(latex2exp)
library(posterior)
library(bayesplot)
library(patchwork)

# Control parameters
outliers_removed <- T
which_cond <- "triangle"
which_model <- "sigma_constant_comp_effect"
  
# Output directory
results_dir <- here("analysis","bayes",glue("{which_model}/{which_cond}/{tmp}",tmp=ifelse(outliers_removed,"no_outliers","with_outliers")))
load(path(results_dir,"fit_summary.RData"))
fit_summary <- select(fit_summary,-median)
print(fit_summary,n=nrow(fit_summary))

rm(list=ls())
library(tidyverse)
library(here)
library(glue)
library(fs)
library(qs)
library(posterior)
library(bayesplot)
library(latex2exp)

# Control parameters
outliers_removed <- T
for(which_cond in c("triangle","horizontal")){
  results_dir <- here("bayes",glue("sigma_constant/{which_cond}/{tmp}",tmp=ifelse(outliers_removed,"no_outliers","with_outliers")))
  
  omega <- qread(path(results_dir,"cor.qs")) %>%
    as_draws_array()
  mcmc_areas(omega,pars=c("x[1,2]",
                          "x[1,3]",
                          "x[2,3]"),prob = .95, prob_outer = .9995)+
    scale_y_discrete(labels=c(
      TeX("$\\rho_{tc}$"),
      TeX("$\\rho_{td}$"),
      TeX("$\\rho_{cd}$")))+
    labs(x="estimate",
         y="parameter",
         title = glue("{which_cond} condition"))+
    scale_x_continuous(limits = c(.5,.75))+
    theme(plot.title = element_text(hjust=0.5),
          plot.subtitle = element_text(hjust=0.5),
          text = element_text(size=32))
  ggsave(path(results_dir,"omega_dens_plot.jpeg"),width=8,height=6)
  
}

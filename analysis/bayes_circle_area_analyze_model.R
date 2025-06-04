# setup =================================================================================
rm(list=ls())
gc()
library(here)
library(tidyverse)
library(fs)
library(glue)
library(latex2exp)
library(posterior)
library(bayesplot)
library(cmdstanr)
library(patchwork)
set_cmdstan_path(here("cmdstan-2.36.0"))

# Control parameters
outliers_removed <- T

# looping through conditions so I can just run all at once on unity
for(which_cond in c("triangle","horizontal")){
  
  for(which_model in c("sigma_constant","sigma_constant_comp_effect","sigma_constant_target_effect")){
    
  # Output directory
  results_dir <- here("analysis","bayes",glue("{which_model}/{which_cond}/{tmp}",tmp=ifelse(outliers_removed,"no_outliers","with_outliers")))
  
  # load in fit
  fit <- readRDS(path(results_dir,"fit.RDS"))
  load(path(results_dir,"dat_for_model.RDS"))
  
  # summarize fit
  if(which_model=="sigma_constant_target_effect"){
    fit_summary <- fit$summary(variables=c("cor","b0","btar","bw","bdiag2",
                                           "bdiag3","bdist5","bdist9",
                                           "bdist14","bdist2d",
                                           "bdist5d","bdist9d",
                                           "bdist14d","sigma_b0_s","s"),
                               mean, median, sd,
                               ~quantile(.x, probs=c(.025,.975)))
  }else if(which_model=="sigma_constant_comp_effect"){
    fit_summary <- fit$summary(variables=c("cor","b0","bcomp","bw","bdiag2",
                                           "bdiag3","bdist5","bdist9",
                                           "bdist14","bdist2d",
                                           "bdist5d","bdist9d",
                                           "bdist14d","sigma_b0_s","s"),
                               mean, median, sd,
                               ~quantile(.x, probs=c(.025,.975)))
  }else{
    fit_summary <- fit$summary(variables=c("cor","b0","bw","bdiag2",
                                           "bdiag3","bdist5","bdist9",
                                           "bdist14","bdist2d",
                                           "bdist5d","bdist9d",
                                           "bdist14d","sigma_b0_s","s"),
                               mean, median, sd,
                               ~quantile(.x, probs=c(.025,.975)))
  }
  
  
  # save fit summary
  save(fit_summary, file=path(results_dir,"fit_summary.RData"))
  
  # get posterioer draws of mu and save
  mu <- fit$draws(variables="mu")
  save(mu,file=path(results_dir,"mu.RData"))
  
  # remove move, clear memory
  rm(mu)
  gc()
  
  # posterior draws of omega, save
  cors <- fit$draws(variables=c("cor[1,3]","cor[2,3]","cor[1,2]"))
  save(cors,file=path(results_dir,"cors.RData"))
  rm(cors)
  gc() # remove, clear memory
  
  # all posterior draws
  draws <- fit$draws(inc_warmup = F)
  draws_array <- as.array(draws)
  
  # no longer need fit object, clear memory
  rm(fit)
  gc()
  
  # set color scheme to red here
  color_scheme_set("red")
  
  # Traceplots of all parameters of interest
  p <- mcmc_trace(draws_array,"b0")
  ggsave(p, filename=path(results_dir,"b0_trace.jpeg"),width=4,height=4)
  rm(p)
  
  # Traceplots of all parameters of interest
  try({p <- mcmc_trace(draws_array,"btar")
  ggsave(p, filename=path(results_dir,"btar_trace.jpeg"),width=4,height=4)
  rm(p)
  })
  # Traceplots of all parameters of interest
  try({p <- mcmc_trace(draws_array,"bcomp")
  ggsave(p, filename=path(results_dir,"bcomp_trace.jpeg"),width=4,height=4)
  rm(p)
  })
  p <- mcmc_trace(draws_array,c("bdiag2","bdiag3"))
  ggsave(p, filename=path(results_dir,"bdiag_trace.jpeg"),width=4,height=4)
  rm(p)
  p <- mcmc_trace(draws_array,c("bdist5","bdist9","bdist14"))
  ggsave(p, filename=path(results_dir,"bdist_trace.jpeg"),width=4,height=4)
  rm(p)
  p <- mcmc_trace(draws_array,c("bdist2d","bdist5d","bdist9d","bdist14d"))
  ggsave(p, filename=path(results_dir,"bdistd_trace.jpeg"),width=4,height=4)
  rm(p)
  p <- mcmc_trace(draws_array,c("cor[1,2]","cor[1,3]","cor[2,3]"))
  ggsave(p, filename=path(results_dir,"cor_trace.jpeg"),width=4,height=4)
  rm(p)
  p <- mcmc_trace(draws_array,c("sigma_b0_s"))
  ggsave(p, filename=path(results_dir,"sigma_b0trace.jpeg"),width=4,height=4)
  rm(p)
  p <- mcmc_trace(draws_array,c("s[1]","s[2]","s[3]"))
  ggsave(p, filename=path(results_dir,"s_trace.jpeg"),width=4,height=4)
  rm(p)
  
  # histograms of all parameters of interest
  p <- mcmc_hist(draws_array,"b0")
  ggsave(p, filename=path(results_dir,"b0_hist.jpeg"),width=4,height=4)
  rm(p)
  try({p <- mcmc_hist(draws_array,"btar")
  ggsave(p, filename=path(results_dir,"btar_hist.jpeg"),width=4,height=4)
  rm(p)
  })
  try({p <- mcmc_hist(draws_array,"bcomp")
  ggsave(p, filename=path(results_dir,"bcomp_hist.jpeg"),width=4,height=4)
  rm(p)
  })
  p <- mcmc_hist(draws_array,c("bdiag2","bdiag3"))
  ggsave(p, filename=path(results_dir,"bdiag_hist.jpeg"),width=4,height=4)
  rm(p)
  p <- mcmc_hist(draws_array,c("bdist5","bdist9","bdist14"))
  ggsave(p, filename=path(results_dir,"bdist_hist.jpeg"),width=4,height=4)
  rm(p)
  p <- mcmc_hist(draws_array,c("bdist2d","bdist5d","bdist9d","bdist14d"))
  ggsave(p, filename=path(results_dir,"bdistd_hist.jpeg"),width=4,height=4)
  rm(p)
  p <- mcmc_hist(draws_array,c("sigma_b0_s"))
  ggsave(p, filename=path(results_dir,"sigma_b0hist.jpeg"),width=4,height=4)
  rm(p)
  p <- mcmc_hist(draws_array,c("s[1]","s[2]","s[3]"))
  ggsave(p, filename=path(results_dir,"s_hist.jpeg"),width=4,height=4)
  rm(p)
  
  # density plots of all parameters of interest
  p <- mcmc_dens(draws_array,"b0")
  ggsave(p, filename=path(results_dir,"b0_dens.jpeg"),width=4,height=4)
  rm(p)
  try({p <- mcmc_dens(draws_array,"btar")
  ggsave(p, filename=path(results_dir,"btar_dens.jpeg"),width=4,height=4)
  rm(p)
  })
  try({p <- mcmc_dens(draws_array,"bcomp")
  ggsave(p, filename=path(results_dir,"bcomp_dens.jpeg"),width=4,height=4)
  rm(p)
  })
  p <- mcmc_dens(draws_array,c("bdiag2","bdiag3"))
  ggsave(p, filename=path(results_dir,"bdiag_dens.jpeg"),width=4,height=4)
  rm(p)
  p <- mcmc_dens(draws_array,c("bdist5","bdist9","bdist14"))
  ggsave(p, filename=path(results_dir,"bdist_dens.jpeg"),width=4,height=4)
  rm(p)
  p <- mcmc_dens(draws_array,c("bdist2d","bdist5d","bdist9d","bdist14d"))
  ggsave(p, filename=path(results_dir,"bdistd_dens.jpeg"),width=4,height=4)
  rm(p)
  p <- mcmc_dens(draws_array,c("cor[1,2]","cor[1,3]","cor[2,3]"))
  ggsave(p, filename=path(results_dir,"cor_dens.jpeg"),width=4,height=4)
  rm(p)
  p <- mcmc_dens(draws_array,c("sigma_b0_s"))
  ggsave(p, filename=path(results_dir,"sigma_b0dens.jpeg"),width=4,height=4)
  rm(p)
  p <- mcmc_dens(draws_array,c("s[1]","s[2]","s[3]"))
  ggsave(p, filename=path(results_dir,"s_dens.jpeg"),width=4,height=4)
  rm(p)
  
  # density plots by chains
  p <- mcmc_dens_chains(draws_array,"b0")
  ggsave(p, filename=path(results_dir,"b0_dens_chains.jpeg"),width=4,height=4)
  rm(p)
  try({p <- mcmc_dens_chains(draws_array,"btar")
  ggsave(p, filename=path(results_dir,"btar_dens_chains.jpeg"),width=4,height=4)
  rm(p)
  })
  try({p <- mcmc_dens_chains(draws_array,"bcomp")
  ggsave(p, filename=path(results_dir,"bcomp_dens_chains.jpeg"),width=4,height=4)
  rm(p)
  })
  p <- mcmc_dens_chains(draws_array,c("bdiag2","bdiag3"))
  ggsave(p, filename=path(results_dir,"bdiag_dens_chains.jpeg"),width=4,height=4)
  rm(p)
  p <- mcmc_dens_chains(draws_array,c("bdist5","bdist9","bdist14"))
  ggsave(p, filename=path(results_dir,"bdist_dens_chains.jpeg"),width=4,height=4)
  rm(p)
  p <- mcmc_dens_chains(draws_array,c("bdist2d","bdist5d","bdist9d","bdist14d"))
  ggsave(p, filename=path(results_dir,"bdistd_dens_chains.jpeg"),width=4,height=4)
  rm(p)
  p <- mcmc_dens_chains(draws_array,c("cor[1,2]","cor[1,3]","cor[2,3]"))
  ggsave(p, filename=path(results_dir,"cor_dens_chains.jpeg"),width=4,height=4)
  rm(p)
  p <- mcmc_dens_chains(draws_array,c("sigma_b0_s"))
  ggsave(p, filename=path(results_dir,"sigma_b0dens_chains.jpeg"),width=4,height=4)
  rm(p)
  p <- mcmc_dens_chains(draws_array,c("s[1]","s[2]","s[3]"))
  ggsave(p, filename=path(results_dir,"s_dens_chains.jpeg"),width=4,height=4)
  rm(p)
  
  # ridge plot of correlations
  p <- mcmc_areas_ridges(draws_array,pars=c("cor[1,2]",
                                            "cor[1,3]",
                                            "cor[2,3]"),prob = .95, prob_outer = .9995)+
    scale_y_discrete(labels=c(
      TeX("$\\rho_{tc}$"),
      TeX("$\\rho_{td}$"),
      TeX("$\\rho_{cd}$")))+
    labs(x="estimate",
         y="parameter",
         title=glue("{which_cond} condition"))+
    scale_x_continuous(limits = c(.5,.75))+
    theme(plot.title = element_text(hjust=0.5),
          plot.subtitle = element_text(hjust=0.5),
          text = element_text(size=16))
  p
  ggsave(p, filename=path(results_dir,"cor_areas_plot.jpeg"),width=7,height=6)
  rm(p)
  
  # set color scheme to gray for paper figurehere
  color_scheme_set("gray")
  # INTERVAL PLOT OF CORRELATIONS FOR PAPER
  p <- mcmc_intervals(draws_array,pars=c("cor[1,2]",
                                         "cor[1,3]",
                                         "cor[2,3]"),
                      prob = .95, prob_outer = 1,point_size=2)+
    scale_y_discrete(labels=c(
      TeX("$\\rho_{\\; tc}$"),
      TeX("$\\rho_{\\; td}$"),
      TeX("$\\rho_{\\; cd}$")))+
    labs(x="estimate",
         y="parameter",
         title=glue("{which_cond}"))+
    scale_x_continuous(limits = c(.5,.75))+
    theme(plot.title = element_text(hjust=0.5),
          plot.subtitle = element_text(hjust=0.5),
          text = element_text(size=16))
  p
  ggsave(p, filename=path(results_dir,"cor_intervals_plot.jpeg"),width=7,height=6)
  save(p, file=path(results_dir,"cor_intervals_plot.RData"))
  rm(p)
  }
}

# combine omega plots into one using patchwork
gc()
outl <- ifelse(outliers_removed,"no_outliers","with_outliers")
load(path("analysis","bayes",which_model,"horizontal",outl,"cor_intervals_plot.RData"))
p_horizontal <- p
rm(p)
gc()

load(path("analysis","bayes",which_model,"triangle",outl,"cor_intervals_plot.RData"))
p_triangle <- p
rm(p)
gc()

pp <- p_triangle / p_horizontal
pp
ggsave(pp,filename = path("analysis","bayes",which_model,glue("omega_both_disp_{outl}_intervals_plot.jpeg")),
       width = 8,height=8)

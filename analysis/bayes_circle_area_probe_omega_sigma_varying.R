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
which_cond <- "horizontal"
results_dir <- here("bayes",glue("sigma_varying/{which_cond}/{tmp}",tmp=ifelse(outliers_removed,"no_outliers","with_outliers")))

results_dir %>%
  dir_ls(regexp = "omega")
omega_global <- qread(path(results_dir,"omega_global.qs")) %>%
  as_draws_array()
omega_local <- qread(path(results_dir,"omega_local.qs")) %>%
  as_draws_array()
Omega <- qread(path(results_dir,"Omega.qs")) %>%
  as_draws_array()

mcmc_areas(Omega,pars = c("x[1,1,3]",
                                 "x[2,1,3]",
                                 "x[3,1,3]",
                                 "x[4,1,3]"))+
  scale_y_discrete(labels=c(
    TeX("$r_{td_{2}}$"),
    TeX("$r_{td_{5}}$"),
    TeX("$r_{td_{9}}$"),
    TeX("$r_{td_{14}}$")
  ))+
  labs(x="r",
       y="parameter",
       title = TeX("$\\Omega_{td}\\,by\\,distance$"),
       subtitle = glue("{which_cond} condition"),
       caption = "Includes both local and global")+
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        text = element_text(size=15))
ggsave(path(results_dir,"Omega_TD_dens_by_dist.jpeg"),width=4,height=4)

mcmc_areas(Omega,pars = c("x[1,2,3]",
                          "x[2,2,3]",
                          "x[3,2,3]",
                          "x[4,2,3]"))+
  scale_y_discrete(labels=c(
    TeX("$r_{cd_{2}}$"),
    TeX("$r_{cd_{5}}$"),
    TeX("$r_{cd_{9}}$"),
    TeX("$r_{cd_{14}}$")
  ))+
  labs(x=TeX("$r_{cd}_{d}$"),
       y="parameter",
       title = TeX("$\\Omega_{cd}\\,by\\,distance$"),
       subtitle = glue("{which_cond} condition"),
       caption = "Includes both local and global")+
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        text = element_text(size=15))
ggsave(path(results_dir,"Omega_CD_dens_by_dist.jpeg"),width=4,height=4)


mcmc_areas(Omega,pars = c("x[1,1,2]",
                          "x[2,1,2]",
                          "x[3,1,2]",
                          "x[4,1,2]"))+
  scale_y_discrete(labels=c(
    TeX("$r_{tc_{2}}$"),
    TeX("$r_{tc_{5}}$"),
    TeX("$r_{tc_{9}}$"),
    TeX("$r_{tc_{14}}$")
  ))+
  labs(x="r",
       y="parameter",
       title = TeX("$\\Omega_{tc}\\,by\\,distance$"),
       subtitle = glue("{which_cond} condition"),
       caption = "Includes both local and global")+
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        text = element_text(size=15))
ggsave(path(results_dir,"Omega_TC_dens_by_dist.jpeg"),width=4,height=4)

mcmc_areas(omega_global,pars = c("x[1,2]",
                                 "x[1,3]",
                                 "x[2,3]"))+
  scale_y_discrete(labels=c(
    TeX("$r_{global}_{tc}$"),
    TeX("$r_{global}_{td}$"),
    TeX("$r_{global}_{cd}$")))+
  labs(x="r",
       y="parameter",
       title = TeX("$\\Omega_{global}$"),
       subtitle = glue("{which_cond} condition"))+
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        text = element_text(size=15))
ggsave(path(results_dir,"omega_global_dens.jpeg"),width=4,height=4)


mcmc_areas(omega_local,pars = c("x[1,1,3]",
                          "x[2,1,3]",
                          "x[3,1,3]",
                          "x[4,1,3]"))+
  scale_y_discrete(labels=c(
    TeX("$r_{local}_{td_{2}}$"),
    TeX("$r_{local}_{td_{5}}$"),
    TeX("$r_{local}_{td_{9}}$"),
    TeX("$r_{local}_{td_{14}}$")
  ))+
  labs(x="r",
       y="parameter",
       title = TeX("$\\Omega_{local}_{td}\\,by\\,distance$"),
       subtitle = glue("{which_cond} condition"))+
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        text = element_text(size=15))
ggsave(path(results_dir,"omega_local_TD_dens_by_dist.jpeg"),width=4,height=4)

mcmc_areas(omega_local,pars = c("x[1,2,3]",
                                "x[2,2,3]",
                                "x[3,2,3]",
                                "x[4,2,3]"))+
  scale_y_discrete(labels=c(
    TeX("$r_{local}_{cd_{2}}$"),
    TeX("$r_{local}_{cd_{5}}$"),
    TeX("$r_{local}_{cd_{9}}$"),
    TeX("$r_{local}_{cd_{14}}$")
  ))+
  labs(x="r",
       y="parameter",
       title = TeX("$\\Omega_{local}_{cd}\\,by\\,distance$"),
       subtitle = glue("{which_cond} condition"))+
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        text = element_text(size=15))
ggsave(path(results_dir,"omega_local_CD_dens_by_dist.jpeg"),width=4,height=4)

mcmc_areas(omega_local,pars = c("x[1,1,2]",
                                "x[2,1,2]",
                                "x[3,1,2]",
                                "x[4,1,2]"))+
  scale_y_discrete(labels=c(
    TeX("$r_{local}_{tc_{2}}$"),
    TeX("$r_{local}_{tc_{5}}$"),
    TeX("$r_{local}_{tc_{9}}$"),
    TeX("$r_{local}_{tc_{14}}$")
  ))+
  labs(x="r",
       y="parameter",
       title = TeX("$\\Omega_{local}_{tc}\\,by\\,distance$"),
       subtitle = glue("{which_cond} condition"))+
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        text = element_text(size=15))
ggsave(path(results_dir,"omega_local_TC_dens_by_dist.jpeg"),width=4,height=4)

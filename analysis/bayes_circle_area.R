# setup =================================================================================
rm(list=ls())
library(here)
library(tidyverse)
library(fs)
library(glue)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(qs)
# install_cmdstan(dir=here()) # FOR INSTALLING CMDSTAN TO PROJECT DIRECTORY
set_cmdstan_path(here("cmdstan-2.35.0")) # SET CMDSTAN PATH 

# Control parameters
outliers_removed <- T

# looping through conditions so I can just run all at once on unity
for(which_cond in c("triangle","horizontal")){
  for(which_model in c("sigma_constant","sigma_varying")){
    
    # Output directory
    results_dir <- here("bayes",glue("{which_model}/{which_cond}/{tmp}",tmp=ifelse(outliers_removed,"no_outliers","with_outliers")))
    dir_create(results_dir)
    
    # number of chains/cores, warm up, iterations
    if(which_model=="sigma_constant"){
      n_chain <- n_core <- 5
      n_warmup <- 800
      n_iter <- 4000
    }else if(which_model=="sigma_varying"){ # want more samples for this model
      n_chain <- n_core <- 6
      n_warmup <- 1000
      n_iter <- 7000
    }
    
    # get stan code file =========================================================================
    stan_file <- here("stan") %>%
      dir_ls(regexp=glue("{which_model}.stan"))
    
    # read in data ================================================================================
    dat_all <- here("data","circle_area","aggregated") %>%
      dir_ls(regexp = ifelse(outliers_removed,"no_outliers.csv","with_outliers")) %>%
      read_csv() %>%
      filter(str_detect(disp_cond,which_cond))
    n_subs <- length(unique(dat_all$sub_n))
    
    # writing out model equation ============================================================
    # subject i 
    # trial j
    # stimulus k
    
    # mu_ijk = (b0+s0_i) + (bw*w_ijk) + (bdiag2*diag2_ij) +
    #           (bdiag3*diag3_ij) +
    #           (bd5*distance5_ij) +
    #           (bd9*distance9_ij) +
    #           (bd14*distance14_ij) +
    #           (bd2d*distance2d_ijk)+
    #           ( (bd5+bd5d)*distance5d_ijk) +
    #           ( (bd9+bd9d)*distance9d_ijk) +
    #           ( (bd14+bd14d)*distance14d_ijk) 
    
    # prep data for stan ======================================================================
    # assigning "new" subject numbers, just so they start at 1 and go up by 1 sequentially
    # this is the "key"
    subs_key <- tibble(
      sub_n = sort(unique(dat_all$sub_n)),
      sub_n_new = seq(1,n_subs,1)
    )
    
    # assign new subject numbers from key
    # also mean center log area
    dat_all_clean <- dat_all %>%
      left_join(subs_key) %>%
      relocate(sub_n_new,.after=sub_n) %>%
      arrange(sub_n_new, block_n, trial_n)%>%
      pivot_longer(c(t,d,c),values_to = "la") %>% 
      mutate(la_cent=la-mean(la)) %>% # MEAN CENTER LOG AREA
      select(-la) %>%
      pivot_wider(names_from = name,values_from = la_cent)
    
    # figure out total number of trials
    n_trials <- nrow(dat_all_clean)
    
    n_trials_per_sub <- dat_all_clean %>%
      group_by(sub_n_new) %>%
      summarise(n=n()) %>%
      ungroup() %>%
      pull(n)
    
    # data for stan
    sub_n_new <- dat_all_clean$sub_n_new
    wt <- wd <- as.integer(dat_all_clean$set=="w") # w - t & d are wide
    wc <- as.integer(dat_all_clean$set=="h") # h - c is wide
    diag2 <- as.integer(dat_all_clean$diag==2) # middle diagonal
    diag3 <- as.integer(dat_all_clean$diag==3) # upper diagonal
    dist2d <- as.integer(dat_all_clean$distance==2) # distance = 2, only applies for decoy
    dist5 <- as.integer(dat_all_clean$distance==5) # distance = 5
    dist9 <- as.integer(dat_all_clean$distance==9) # distance = 9
    dist14 <- as.integer(dat_all_clean$distance==14) # distance = 14
    la_cent <- cbind(dat_all_clean$t, # log area - centered
                     dat_all_clean$c,
                     dat_all_clean$d) 
    dists_all <- recode(dat_all$distance, `2`=1,`5`=2,`9`=3,`14`=4)
    n_dists <- 4
    
    dat_all_clean_unique <- dat_all_clean %>%
      distinct(sub_n_new,set,diag,distance) 
    n_trials_unique <- nrow(dat_all_clean_unique)
    sub_n_new_unique <- dat_all_clean_unique$sub_n_new
    wt_unique <- as.integer(dat_all_clean_unique$set=="h")
    wd_unique <- as.integer(dat_all_clean_unique$set=="h")
    wc_unique <- as.integer(dat_all_clean_unique$set=="w")
    diag2_unique <- as.integer(dat_all_clean_unique$diag==2) # middle diagonal
    diag3_unique <- as.integer(dat_all_clean_unique$diag==3) # upper diagonal
    dist2d_unique <- as.integer(dat_all_clean_unique$distance==2) # distance = 2, only applies for decoy
    dist5_unique <- as.integer(dat_all_clean_unique$distance==5) # distance = 5
    dist9_unique <- as.integer(dat_all_clean_unique$distance==9) # distance = 9
    dist14_unique <- as.integer(dat_all_clean_unique$distance==14) # distance = 14
    
    # put all data in list object for stan
    if(which_model=="sigma_constant"){
      stan_data <- list(
        n_trials=n_trials,
        n_subs=n_subs,
        sub_n_new=sub_n_new,
        wt=wt,
        wc=wc,
        wd=wd,
        diag2=diag2,
        diag3=diag3,
        dist5=dist5,
        dist9=dist9,
        dist14=dist14,
        dist2d=dist2d,
        la_cent=la_cent
      ) 
    }else{
      stan_data <- list(
        n_trials=n_trials,
        n_subs=n_subs,
        sub_n_new=sub_n_new,
        wt=wt,
        wc=wc,
        wd=wd,
        diag2=diag2,
        diag3=diag3,
        dist5=dist5,
        dist9=dist9,
        dist14=dist14,
        dist2d=dist2d,
        la_cent=la_cent,
        dists_all=dists_all,
        n_dists=n_dists
      )
    }
    
    # compile model and sample from posterior ============================================================
    if(length(dir_ls(results_dir,regexp="bayes_circle_area"))==0){
      m <- cmdstan_model(stan_file)
      fit <- m$sample(data=stan_data,
                      chains=n_chain,
                      parallel_chains=n_chain,
                      refresh=100,
                      iter_warmup=n_warmup,
                      iter_sampling=n_iter,
                      output_dir=results_dir,
                      output_basename="bayes_circle_area")
    }
    
    # deal with results =========================================================================
    files <-  dir_ls(path(results_dir),regexp="bayes_circle_area")
    fit <- read_cmdstan_csv(files)
    try({
     fit1 <- as_cmdstan_fit(fit)
     qsave(fit1, file = path(results_dir,"fit.qs"))
     save(fit1, file = path(results_dir,"fit.RData"))
    })

    draws <- as_draws(fit$post_warmup_draws)
    try({
      diagnostics <- fit$post_warmup_sampler_diagnostics
    })
    try({
      qsave(draws,file=path(results_dir,"draws.qs"))
      save(draws,file=path(results_dir,"draws.RData"))
    })
    try({
      qsave(diagnostics,file=path(results_dir,"diagnostics.qs"))
      save(diagnostics,file=path(results_dir,"diagnostics.RData"))
    })
    try({
      fit_summary <- summarise_draws(draws)
    })
    try(qsave(x=fit_summary,file=path(results_dir,"fit_summary.qs")))
    try(save(x=fit_summary,file=path(results_dir,"fit_summary.RData")))
    try(write_csv(x=fit_summary,file=path(results_dir,"fit_summary.csv")))
    bayesplot::color_scheme_set("red")
    try({
      mcmc_trace(draws, pars=c("b0"))
      ggsave(filename=path(results_dir,"b0_trace.jpeg"),width=4,height=4)
    })

    try({
      mcmc_trace(draws, pars=c("bw"))
      ggsave(filename=path(results_dir,"bw_trace.jpeg"),width=4,height=4)
    })

    try({
      mcmc_trace(draws, pars=c("bdiag2"))
      ggsave(filename=path(results_dir,"bdiag2_trace.jpeg"),width=4,height=4)
    })

    try({
      mcmc_trace(draws, pars=c("bdiag3"))
      ggsave(filename=path(results_dir,"bdiag3_trace.jpeg"),width=4,height=4)
    })

    try({
      mcmc_trace(draws, pars=c("bdist5","bdist9","bdist14","bdist2d","bdist5d","bdist9d","bdist14d"))
      ggsave(filename=path(results_dir,"bdist_trace.jpeg"),width=6,height=6)
    })

    try({
      mcmc_trace(draws, pars=c("sigma_b0_s"))
      ggsave(filename=path(results_dir,"sigma_b0_s_trace.jpeg"),width=4,height=4)
    })

    try({
      mcmc_trace(draws, regex_pars="omega")
      ggsave(filename=path(results_dir,"omega_trace.jpeg"),width=4,height=4)
    })

    try({
      mcmc_trace(draws, regex_pars=c("s"))
      ggsave(filename=path(results_dir,"s_trace.jpeg"),width=4,height=4)
    })

    try({
      mcmc_trace(draws, regex_pars=c("b0_s"))
      ggsave(filename=path(results_dir,"b0_s_trace.jpeg"),width=10,height=10)
    })

    try({
      mcmc_hist(draws, pars=c("b0"))
      ggsave(filename=path(results_dir,"b0_hist.jpeg"),width=4,height=4)
    })

    try({
      mcmc_hist(draws, pars=c("bw"))
      ggsave(filename=path(results_dir,"bw_hist.jpeg"),width=4,height=4)
    })
    
    try({
      mcmc_hist(draws, pars=c("bdiag2"))
      ggsave(filename=path(results_dir,"bdiag2_hist.jpeg"),width=4,height=4)
    })

    try({
      mcmc_hist(draws, pars=c("bdiag3"))
      ggsave(filename=path(results_dir,"bdiag3_hist.jpeg"),width=4,height=4)
    })

    try({
      mcmc_hist(draws, pars=c("bdist5","bdist9","bdist14","bdist2d","bdist5d","bdist9d","bdist14d"))
      ggsave(filename=path(results_dir,"bdist_hist.jpeg"),width=6,height=6) 
    })
    
    try({
      mcmc_hist(draws, pars=c("sigma_b0_s"))
      ggsave(filename=path(results_dir,"sigma_b0_s_hist.jpeg"),width=4,height=4)
    })

    try({
      mcmc_intervals(draws, regex_pars=c("^b0_s"),prob_outer = .95)
      ggsave(filename=path(results_dir,"b0_s_intervals.jpeg"),width=6,height=6)
    })

    try({
      mcmc_hist(draws, regex_pars=c("^s"))
      ggsave(filename=path(results_dir,"s_hist.jpeg"),width=4,height=4)
    })
    try({
      draws_rvars <- as_draws_rvars(draws)
    })
    try(mu <- as.array(draws_of(draws_rvars$mu)))
    try(s <- as.array(draws_of(draws_rvars$s)))
    try(qsave(mu,path(results_dir,"mu.qs")))
    try(qsave(s,path(results_dir,"s.qs")))
    try(save(mu,path(results_dir,"mu.RData")))
    try(save(s,path(results_dir,"s.RData")))
    dat_all <- list(dat_all_clean, stan_data)
    try(qsave(dat_all, path(results_dir,"dat_for_model.qs")))
    try(save(dat_all, path(results_dir,"dat_for_model.RData")))
    
    if(which_model=="sigma_constant"){
      try({
        mcmc_hist(draws, regex_pars="cor")
        ggsave(filename=path(results_dir,"cor_hist.jpeg"),width=5,height=5)
      })
      try({
        mcmc_trace(draws, regex_pars="cor")
        ggsave(filename=path(results_dir,"cor_trace.jpeg"),width=5,height=5)
      })
      try({
        mcmc_areas_ridges(draws, regex_pars="cor")
        ggsave(filename=path(results_dir,"cor_trace.jpeg"),width=5,height=5)
      })

      try({
        omega <- draws_rvars$omega
        try(qsave(omega,path(results_dir,"omega.qs")))
        try(save(omega,path(results_dir,"omega.RData")))
      })
      try({
        cor_array <- draws_rvars$cor
        try(qsave(cor_array,path(results_dir,"cor.qs")))
        try(save(cor_array,path(results_dir,"cor.RData")))
      })
    }else if(which_model=="sigma_varying"){
      try({
        mcmc_trace(draws, pars=c("rho"))
        ggsave(filename=path(results_dir,"rho_trace.jpeg"),width=4,height=4) 
      })
      
      try({
        mcmc_trace(draws, regex_pars="omega_global")
        ggsave(filename=path(results_dir,"omega_global_trace.jpeg"),width=4,height=4) 
      })
      
      try({
        mcmc_hist(draws,regex_pars="omega_global")
        ggsave(filename=path(results_dir,"omega_global_hist.jpeg"),width=6,height=6) 
      })
      
      try({
        mcmc_trace(draws, regex_pars="omega_local")
        ggsave(filename=path(results_dir,"omega_local_trace.jpeg"),width=6,height=6) 
      })
      
      try({
        mcmc_hist(draws, regex_pars="omega_local")
        ggsave(filename=path(results_dir,"omega_local_hist.jpeg"),width=6,height=6) 
      })
      
      try({
        mcmc_hist(draws, regex_pars="Omega")
        ggsave(filename=path(results_dir,"Omega_hist.jpeg"),width=6,height=6) 
      })
      
      try({
        mcmc_hist(draws, pars=c("rho"))
        ggsave(filename=path(results_dir,"rho_hist.jpeg"),width=6,height=6) 
      })
      
      try(Omega <- draws_rvars$Omega)
      try(qsave(Omega,path(results_dir,"Omega.qs")))
      try(save(Omega,path(results_dir,"Omega.RData")))
      
      try({
        omega_local <- draws_rvars$omega_local
        try(qsave(omega_local,path(results_dir,"omega_local.qs")))
        try(save(omega_local,path(results_dir,"omega_local.RData")))
        try(write_csv(omega_local,path(results_dir,"omega_local.csv")))
      })
      
      try({
        omega_global <- draws_rvars$omega_global
        try(qsave(omega_global,path(results_dir,"omega_global.qs")))
        try(save(omega_global,path(results_dir,"omega_global.RData")))
        try(write_csv(omega_global,path(results_dir,"omega_global.csv")))
      })
    }
    
  }
}

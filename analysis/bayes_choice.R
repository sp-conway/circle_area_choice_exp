# bayesian modeling of choice data
# technically need to run this script twice, once to fit model and once to analyze, plot posterior distributions of params, etc
# setup ====================================================================================================
rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(glue)
library(cmdstanr)
library(bayesplot)

# control settings
which_model <- "bayes_choice_2"

# sampler settings
n_chain <- n_core <- 4
n_iter <- 2500

# set path to cmdstan
set_cmdstan_path(here("cmdstan-2.36.0"))

# LOOP THROUGH BOTH CONDITIONS =========================================================================
for(which_cond in c("triangle","horizontal")){
  # paths etc
  stan_dir <- here("analysis","bayes_choice","stan")
  stan_file <- path(stan_dir,glue("{which_model}.stan"))
  results_dir <- here("analysis","bayes_choice",which_model,which_cond)
  dir_create(results_dir)
  
  # load and prep data ====================================================================================================
  d <- here("data","choice","aggregated","choice_all.csv") %>%
    read_csv() %>%
    filter(str_detect(effect,"attraction") & 
             str_detect(disp_cond,which_cond)) %>%
    mutate(choice_tdc=factor(choice_tdc,
                             levels=c("target","competitor","decoy")))
  n_subs <- length(unique(d$sub_n))
  
  subs_key <- tibble(
    sub_n = sort(unique(d$sub_n)),
    sub_n_new = seq(1,n_subs,1)
  )
  d_counts <- d %>%
    group_by(sub_n,distance,set,choice_tdc) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    pivot_wider(names_from = choice_tdc, values_from = n, values_fill = 0) %>%
    left_join(subs_key) %>%
    relocate(sub_n_new,.after=sub_n) 
  
  tw <- dw <- if_else(d_counts$set=="w",1,0) # target and decoy always oriented the same
  cw <- if_else(d_counts$set=="h",1,0) # if target h then comp w and vice versa
  distance5 <- if_else(d_counts$distance==5,1,0)
  distance9 <- if_else(d_counts$distance==9,1,0)
  distance14 <- if_else(d_counts$distance==14,1,0)
  sub_ns_new <- d_counts$sub_n_new
  N <- nrow(d_counts)
  J <- n_subs
  choice_counts <- as.matrix(d_counts[,c("target","competitor","decoy")])
  stan_data <- list(
    tw=tw,
    cw=cw,
    dw=dw,
    sub_ns_new=sub_ns_new,
    N=N,
    J=J,
    distance5=distance5,
    distance9=distance9,
    distance14=distance14,
    choice_counts=choice_counts
  )
  
  # compile and fit model =========================================================================================
  m <- cmdstan_model(stan_file)
  
  if(length(dir_ls(results_dir,regexp="bayes_choice"))==0){
    m <- cmdstan_model(stan_file)
    fit <- m$sample(data=stan_data,
                    chains=n_chain,
                    parallel_chains=n_chain,
                    refresh=100,
                    iter_sampling=n_iter,
                    output_dir=results_dir,
                    output_basename="bayes_choice")
    try(fit$save_object(file=path(results_dir,"fit.RDS")))
    save(stan_data,choice_counts,subs_key,file=path(results_dir,"data_for_model.RData"))
    fit_summary <- fit$summary(variables=c("b_0","b_target","b_comp","b_distance5","b_distance9","b_distance14","b_target_distance5","b_target_distance9","b_target_distance14",
                             "b_w","b_0_s_sigma","b_w_s_sigma","b_comp_s_sigma","b_target_s_sigma","b_distance_s_sigma","b_targetcomp_distance_intxn_s_sigma"),
                              mean, median, sd,
                               ~quantile(.x, probs=c(.025,.975)))
    save(fit_summary,file=path(results_dir,"fit_summary.RData"))
    p <- fit$draws(variables="p")
    save(p,file=path(results_dir,"p_pred.RData"))
    choice_counts_rep <- fit$draws(variables="choice_counts_rep")
    save(choice_counts_rep,file=path(results_dir,"choice_counts_rep_pred.RData"))
  }else{
    fit <- read_cmdstan_csv(dir_ls(results_dir,regexp = ".csv"))
    post <- as.array(fit)$post_warmup_draws
    
    # params change by the model, so need to manually assign relevant parameters to plot
    if(which_model %in% c("bayes_choice_1","bayes_choice_2","bayes_choice_3")){
      par_sets <- list(
        "lp"=c("lp__"),
        "b_0"=c("b_0"),
        "b_target_comp"=c("b_target","b_comp"),
        "b_distance"=c("b_distance5","b_distance9","b_distance14"),
        "b_comp_distance"=c("b_comp_distance5","b_comp_distance9","b_comp_distance14"),
        "b_target_distance"=c("b_target_distance5","b_target_distance9","b_target_distance14"),
        "b_w"="b_w",
        "sigma_all"=c("b_0_s_sigma","b_w_s_sigma","b_comp_s_sigma","b_target_s_sigma","b_distance_s_sigma","b_targetcomp_distance_intxn_s_sigma")
      )
    }
    color_scheme_set('red')
    
    n_par_sets <- length(par_sets)
    for(p in 1:n_par_sets){
      par_names <- names(par_sets)[p]
      pars <- par_sets[[p]]
      
      try({
        mcmc_trace(post,pars=pars)
        ggsave(filename=path(results_dir,glue("{par_names}_trace.jpeg")),width=5,height=5)
      })
      try({
        mcmc_dens(post,pars=pars)
        ggsave(filename=path(results_dir,glue("{par_names}_dens.jpeg")),width=5,height=5)
      })
      try({
        mcmc_dens_chains(post,pars=pars)
        ggsave(filename=path(results_dir,glue("{par_names}_dens_chains.jpeg")),width=5,height=5)
      })
      try({
        mcmc_hist(post,pars=pars)
        ggsave(filename=path(results_dir,glue("{par_names}_hist.jpeg")),width=5,height=5)
      })
      try({
        mcmc_intervals(post,pars=pars)
        ggsave(filename=path(results_dir,glue("{par_names}_intervals.jpeg")),width=5,height=5)
      })
    }
    rm(fit)
    gc()
  }
}

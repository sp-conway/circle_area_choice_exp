# setup =================================================================================
rm(list=ls())
library(here)
library(tidyverse)
library(fs)
library(glue)
library(posterior)
library(bayesplot)
library(cmdstanr)
set_cmdstan_path(here("cmdstan-2.36.0"))

# Control parameters
outliers_removed <- T
testing <- F

if(testing){
  n_chain <- n_core <- 4
  n_iter <- 100
}else{
  n_chain <- n_core <- 4
  n_iter <- 2500
}

# looping through conditions so I can just run all at once on unity
for(which_cond in c("triangle","horizontal")){
  
  for(which_model in c("sigma_constant","sigma_constant_comp_effect","sigma_constant_target_effect")){
    # Output directory
    results_dir <- here("analysis","bayes",glue("{which_model}/{which_cond}/{tmp}",tmp=ifelse(outliers_removed,"no_outliers","with_outliers")))
    dir_create(results_dir)
    
    # get stan code file =========================================================================
    stan_file <- here("analysis","bayes","stan",glue("bayes_circle_area_{which_model}.stan"))
    
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
    
    
    
    # put all data in list object for stan
    #if(which_model=="sigma_constant"){
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
    #}else{
    #  stan_data <- list(
    #   n_trials=n_trials,
    #   n_subs=n_subs,
    #   sub_n_new=sub_n_new,
    #   wt=wt,
    #   wc=wc,
    #   wd=wd,
    #   diag2=diag2,
    #   diag3=diag3,
    #  dist5=dist5,
    #  dist9=dist9,
    #  dist14=dist14,
    #  dist2d=dist2d,
    #  la_cent=la_cent
    #) 
    #}
    save(dat_all_clean,
         stan_data,
         n_trials,
         n_trials_per_sub,
         subs_key,
         file=path(results_dir,"dat_for_model.RDS"))
    
    
    # compile model and sample from posterior ============================================================
    if(length(dir_ls(results_dir,regexp="bayes_circle_area"))==0){
      m <- cmdstan_model(stan_file)
      fit <- m$sample(data=stan_data,
                      chains=n_chain,
                      parallel_chains=n_chain,
                      refresh=100,
                      iter_sampling=n_iter,
                      output_dir=results_dir,
                      output_basename=ifelse(testing,"DELETE","bayes_circle_area"))
      if(!testing) try(fit$save_object(file=path(results_dir,"fit.RDS")))
    }
  }
}

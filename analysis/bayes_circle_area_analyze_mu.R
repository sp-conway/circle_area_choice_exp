# parsing posterior samples of mu, joining to data (a pain), computing means and HDIs
# this script will produce massive memory load. I ran this code on a remote computing cluster
# will crash most individual machines
# setup =================================================================================
rm(list=ls()); gc()
library(here)
library(tidyverse)
library(fs)
library(glue)
library(latex2exp)
library(posterior)
library(bayesplot)
library(cmdstanr)
library(HDInterval)
library(patchwork)
set_cmdstan_path(here("cmdstan-2.36.0")) #might not actually matter here but just in case


# Control parameters
outliers_removed <- T # whether or not to use data w/ outliers removed (currently only yes)
testing <- F # for debugging
which_model <- "sigma_constant_target_effect" # currently "sigma_constant" or "sigma_constant_target_effect"
# target_effect refers to whether or not we allow for a target coefficient

for(which_cond in c("triangle","horizontal")){
    outl <-ifelse(outliers_removed,"no_outliers","with_outliers")
    results_dir <- here("analysis","bayes",glue("{which_model}/{which_cond}/{outl}"))
    
    # load data and get distinct trials
    load(path(results_dir,"dat_for_model.RDS"))
    dat_all_clean_distinct <- dat_all_clean %>%
      group_by(sub_n,set,distance,diag) %>%
      mutate(n=1:n()) %>%
      ungroup() %>%
      mutate(distinct=n==1,
             N=1:n()) 
    
    # load and clean mu
    load(path(results_dir,"mu.RData"))
    
    # convert to draws_df, figure out target/competitor, decoy draws
    # N is a placeholder variable to figure out the index of the data
    # there are N total data points (where a data point has a t/c/d rating)
    # so if we figure out index of model, can get back the conditions from the data
    # could maybe do this using stan variables, but this requires hand-recoding data
    # this is more foolproof
    mu_dat <- as_draws_df(mu) %>%
      pivot_longer(everything()) %>%
      mutate(stim=case_when(
        str_detect(name, "mu\\[\\d+,1\\]$")~"t",
        str_detect(name, "mu\\[\\d+,2\\]$")~"c",
        str_detect(name, "mu\\[\\d+,3\\]$")~"d",
      ),
      N=as.numeric(str_extract(name,"(?<=\\[)\\d+(?=,)"))) %>%
      select(-name) 
    
    # given mu_dat, also need to figure out iteration of sampler
    # then join to actual data
    # then we filter out the non-distinct trials from model
    # that is, participants see each trial a few times, but we only want unique trial types in model results
    mu_dat1 <-mu_dat %>%
      group_by(N) %>%
      mutate(iter=1:n()) %>%
      ungroup() %>%
      left_join(select(dat_all_clean_distinct,
                       sub_n,block_n,trial_n,diag,set,distance,distinct,N),by="N",
                relationship = "many-to-one") %>%
      filter(distinct)
    
    # Mu mean and HDI, split by subject, set, distance, diagonal, stim (t/d/c)
    mu_avgs_by_sub_set_distance_diag <- mu_dat1 %>%
      group_by(sub_n,diag, set, distance, stim) %>%
      summarise(m=mean(value),
                hdi_lower=hdi(value)[1],
                hdi_upper=hdi(value)[2]) %>%
      ungroup()
    
    # Mu mean and HDI, split by subject, distance, stim (t/d/c)
    mu_avgs_by_sub_distance <- mu_dat1 %>%
      group_by(sub_n,distance, stim) %>%
      summarise(m=mean(value),
                hdi_lower=hdi(value)[1],
                hdi_upper=hdi(value)[2]) %>%
      ungroup()
    
    # Mu mean and HDI, split by distance and stimulis
    mu_avgs_fully_collapsed_w_hdis <- mu_dat1 %>%
      group_by(iter, distance, stim) %>%
      summarise(mm=mean(value)) %>%
      group_by(stim, distance) %>%
      summarise(m=mean(mm),
                hdi_lower=hdi(mm)[1],
                hdi_upper=hdi(mm)[2]) %>%
      ungroup()
    
    # save these data
    save(mu_avgs_by_sub_distance,mu_avgs_by_sub_set_distance_diag,mu_avgs_fully_collapsed_w_hdis, file=path(results_dir,"mu_avgs.RData"))
    
    # clean out these very large R objects and clear RAM space
    rm(mu_dat)
    rm(mu_dat1)
    rm(mu_avgs_fully_collapsed_w_hdis)
    rm(mu_avgs_by_sub_distance)
    rm(mu_avgs_by_sub_set_distance_diag)
    rm(dat_all_clean)
    rm(dat_all_clean_distinct)
    rm(stan_data)
    rm(subs_key)
    gc()
}
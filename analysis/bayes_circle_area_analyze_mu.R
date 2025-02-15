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
set_cmdstan_path(here("cmdstan-2.36.0"))

# Control parameters
outliers_removed <- T
which_model <- "sigma_constant_comp_effect"

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
    
    mu_dat <- as_draws_df(mu) %>%
      pivot_longer(everything()) %>%
      mutate(stim=case_when(
        str_detect(name, "mu\\[\\d+,1\\]$")~"t",
        str_detect(name, "mu\\[\\d+,2\\]$")~"c",
        str_detect(name, "mu\\[\\d+,3\\]$")~"d",
      ),
      N=as.numeric(str_extract(name,"(?<=\\[)\\d+(?=,)"))) %>%
      select(-name) 
    mu_dat1 <-mu_dat %>%
      group_by(N) %>%
      mutate(iter=1:n()) %>%
      ungroup() %>%
      left_join(select(dat_all_clean_distinct,
                       sub_n,block_n,trial_n,diag,set,distance,distinct,N),by="N",
                relationship = "many-to-one") %>%
      filter(distinct)
    mu_avgs_by_sub_set_distance_diag <- mu_dat1 %>%
      group_by(sub_n,diag, set, distance, stim) %>%
      summarise(m=mean(value),
                hdi_lower=hdi(value)[1],
                hdi_upper=hdi(value)[2]) %>%
      ungroup()
    mu_avgs_by_sub_distance <- mu_dat1 %>%
      group_by(sub_n,distance, stim) %>%
      summarise(m=mean(value),
                hdi_lower=hdi(value)[1],
                hdi_upper=hdi(value)[2]) %>%
      ungroup()
    mu_avgs_fully_collapsed_w_hdis <- mu_dat1 %>%
      group_by(iter, distance, stim) %>%
      summarise(mm=mean(value)) %>%
      group_by(stim, distance) %>%
      summarise(m=mean(mm),
                hdi_lower=hdi(mm)[1],
                hdi_upper=hdi(mm)[2]) %>%
      ungroup()
    save(mu_avgs_by_sub_distance,mu_avgs_by_sub_set_distance_diag,mu_avgs_fully_collapsed_w_hdis, file=path(results_dir,"mu_avgs.RData"))
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
# setup =================================================================================
rm(list=ls())
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
which_model <- "sigma_constant"

outl <-ifelse(outliers_removed,"no_outliers","with_outliers")
results_dir_tri <- here("analysis","bayes",glue("{which_model}/triangle/{outl}"))
fit <- readRDS(path(results_dir_tri,"fit.RDS"))
load(path(results_dir_tri,"dat_for_model.RDS"))

mu <- fit$draws("mu",inc_warmup=F)
mu <- as_draws_df(mu)
colnames(mu) <- paste0(rep(c("t","c","d"),nrow(dat_all_clean)),
                       "_",
                       sort(rep(1:nrow(dat_all_clean),3)))
dat_all_clean_distinct <- dat_all_clean %>%
  group_by(sub_n,set,distance,diag) %>%
  mutate(n=1:n()) %>%
  ungroup() %>%
  mutate(distinct=n==1)
max(which(dat_all_clean_distinct$distinct))
mu_d <- mu %>% 
  pivot_longer(everything(),names_to=c("stim","trial"),names_sep="_") %>%
  mutate(trial=as.numeric(trial)) %>%
  filter(trial %in% which(dat_all_clean_distinct$distinct)) %>%
  left_join(filter(dat_all_clean_distinct) %>% select(-c(t,c,d)))

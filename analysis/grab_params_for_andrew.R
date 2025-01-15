rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(qs)

model_dir <- here("bayes","sigma_constant","triangle","no_outliers")
mu_cv <- path(model_dir,"mu_cv_collapsed.qs") %>%
  qread()
mu_all <- mu_cv[[1]]
dat <- path(model_dir,"sim_choice_preds_collapsed.qs") %>%
  qread()
dat_clean <- dat[[2]] %>%
  select(-c(t,c,d))
mu_all

mu_dat <- bind_cols(
  dat_clean, 
  tibble(t=mu_all[,1],
  c=mu_all[,2],
  d=mu_all[,3])
)

mu <- mu_dat %>%
  group_by(distance) %>%
  summarise(t=mean(t),
            c=mean(c),
            d=mean(d)) %>%
  ungroup() %>%
  as.data.frame()
load(path(model_dir,"cor.RData"))
s <- qread(path(model_dir,"s.qs"))
s <- sqrt(s)
s <- apply(s,2,mean)
names(s) <- c("t","c","d")
r_tc <- mean(cor_array[,1,2])
r_td <- mean(cor_array[,1,3])
r_cd <- mean(cor_array[,2,3])
cors <- matrix(c(1,r_tc,r_td,
                 r_tc,1,r_cd,
                 r_td,r_cd,1),nrow=3,byrow=T)
rownames(cors) <- c("t","c","d")
colnames(cors) <- c("t","c","d")
params <- list(mu,
               s,
               cors)
names(params) <- c("mu","s","cors")
params
dir <- dir_create(glue::glue("{here('params_for_andrew')}_{Sys.Date()}"))
save(params,file=path(dir,"params_tmp.RData"))
params

# Setup =================================================================================
rm(list=ls())
library(tidyverse)
library(here)
library(glue)
library(posterior)
library(bayesplot)
library(fs)
library(qs)
library(mvtnorm)
# library(doParallel)

# control params
which_model <- "sigma_constant"
outliers_removed <- T
which_cond <- "triangle"
# which_cond <- "horizontal"
# results directory
results_dir <- here("bayes",glue("sigma_constant/{which_cond}/{tmp}",
                                 tmp=ifelse(outliers_removed,"no_outliers","with_outliers")))

# CONTROL PARAMS
N <- 1000

# CONSTANTS
n_chain <- 5 # CANNOT CHANGE THESE - NUMBER OF CHAINS USED WHEN FITTING MODEL
n_iter <- 4000 # CANNOT CHANGE THESE - NUMBER OF CHAINS USED WHEN FITTING MODEL

# number of total draws
n_draws <- n_chain*n_iter

# load in modeling results ====================================================================
mu <- path(results_dir,"mu.qs") %>%  # mu 
  qread()
s <- path(results_dir,"s.qs") %>% # variances (should probably be v or s quared but whatever)
  qread()
dat_all <- path(results_dir,"dat_for_model.qs") %>%
  qread() # # data 
d_clean <- dat_all[[1]] # df clean
stan_data <- dat_all[[2]] # stan data

# load in correlations and make a matrix (takes a few conv. steps)
cors <- path(results_dir,"cor.qs") %>%
  qread() %>%
  as_draws_array() %>%
  as_draws_matrix() 

# get all covariance matrices from model =======================================================
get_cv <- function(s,cors){
  cors <- matrix(cors,nrow=3,ncol=3,byrow=F)
  cors * (s %*% t(s) )
}
cv_all <- map(1:n_draws, ~get_cv(s[.x,],cors[.x,]))
cor_avgd <- matrix(apply(cors, 2, mean),
                   nrow=3,ncol=3, byrow=F)
s_avgd <- apply(s, 2, mean)
cv_avgd <- get_cv(s_avgd,cor_avgd)

# function for simulation ===================================================================
sim_choice <- function(N,mu,cv){
  samp <- mvtnorm::rmvnorm(N,mu,cv)
  mx <- apply(samp, 1, which.max)
  p <- c(sum(mx==1),
         sum(mx==2),
         sum(mx==3))/N
  return(p)
}

# get unique trial type indices per subject =======================================================
# grouping data by all trial types
# then getting an index for each
# removing repeats from mu later for more efficient simulations
# don't need to simualte the same trial type for each subject more than once
d_clean_w_ind <- d_clean %>%
  group_by(sub_n_new,set,distance,diag) %>% 
  mutate(iter=1:n()) %>%
  ungroup() %>%
  mutate(is_unique=iter==1)

# INDICES IN DATA
dat_ind <- which(d_clean_w_ind$is_unique)

# CLEAN DF FOR LATER
d_clean_unique <- filter(d_clean_w_ind, is_unique)

# NUMBER OF DATA POINTS FOR MU
n_dat <- length(dat_ind)

mu_collapsed <- matrix(NA, nrow=n_dat, ncol=3)
mu_collapsed[,1] <- apply(mu[,dat_ind,1], 2, mean)
mu_collapsed[,2] <- apply(mu[,dat_ind,2], 2, mean)
mu_collapsed[,3] <- apply(mu[,dat_ind,3], 2, mean)

# do simulation !!!! ====================================================================================================
p <- matrix(NA, nrow=n_dat, ncol=3)
for(i in 1:n_dat){
  cat(i,"/",n_dat,"\n")
  p[i,] <- sim_choice(N, mu_collapsed[i,], cv_avgd)
}

tryCatch(qsave(list(p,d_clean_unique),file=path(results_dir,"sim_choice_preds_collapsed.qs")),error=function(e) print(e))
tryCatch(save(p,d_clean_unique,file=path(results_dir,"sim_choice_preds_collapsed.RData")),
         error=function(e) print(e))
tryCatch(qsave(list(mu_collapsed,cv_avgd),file=path(results_dir,"mu_cv_collapsed.qs")),error=function(e) print(e))
tryCatch(save(mu_collapsed,cv_avgd,file=path(results_dir,"mu_cv_collapsed.RData")),error=function(e) print(e))

# (try) and clean results ====================================================================================================
try({
  res_df <- bind_cols(
    disp_cond=d_clean_unique$disp_cond,
    sub_n=d_clean_unique$sub_n,
    set=d_clean_unique$set,
    distance=d_clean_unique$distance,
    diag=d_clean_unique$diag,
    target=p[,1],
    competitor=p[,2],
    decoy=p[,3]) %>%
    pivot_longer(c(target,decoy,competitor),names_to = "stim", values_to = "prop") 
  
  tryCatch(qsave(res_df,file=path(results_dir,"sim_choice_preds_clean_collapsed.qs")),
           error=function(e) print(e))
  tryCatch(save(res_df,file=path(results_dir,"sim_choice_preds_clean_collapsed.RData")),
           error=function(e) print(e))
  tryCatch(write_csv(res_df,path(results_dir,"sim_choice_preds_clean_collapsed.csv")),
           error=function(e) print(e))
})






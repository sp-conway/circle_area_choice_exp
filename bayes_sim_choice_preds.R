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
which_cond <- "horizontal"
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

# setting up "chunks" for more efficient simulation ======================================================================
n_cores <- 20#detectCores()-2
per_chunk <- floor(n_draws/n_cores)
start <- 1
stop <- per_chunk
for(i in 2:(n_cores-1)){
  start <- c(start, stop[i-1]+1)
  stop <- c(stop, start[i]+per_chunk)
}

chunks <- cbind(start,stop)
chunks <- rbind(chunks,cbind(start=max(stop)+1,stop=n_draws))
n_chunks <- nrow(chunks)
if(max(stop) < n_draws){
  chunks[n_chunks,2] <- n_draws
}

# do simulation !!!! ====================================================================================================
cluster <- makeCluster(n_cores)
registerDoParallel(cluster)
results <- vector("list",n_chunks)
results[[i]] <- foreach(i=1:n_chunks) %dopar% {
  k <- 1
  sink(paste0(results_dir, "/",which_cond, "_worker_", i, "_output.txt"))
  cat("Worker ", i, " is processing ", k, "/", per_chunk, sep = "")
  sink()  # Reset sink to standard output
  iter_vec <- chunks[i,]
  pp <- array(NA_real_,dim=c(diff(chunks[i,]),n_dat,3))
  for(iter in iter_vec){
    for(dat in 1:n_dat){
      pp[iter,dat,] <- sim_choice(N,mu[iter,dat_ind[dat],],cv_all[[iter]])
    }
    k <- k+1
  }
}
stopCluster(cl=cluster)
p <- array(NA_real_,dim=c(n_iter,n_dat,3))

for(i in 1:n_chunks){
  p[chunks[i,1]:chunks[i,2],,] <- results[[i]]
}

tryCatch(qsave(p,file=path(results_dir,"sim_choice_preds.qs")),error=function(e) print(e))
tryCatch(save(list(p,dat_all),file=path(results_dir,"sim_choice_preds.RData")),
         error=function(e) print(e))

# (try) and clean results ====================================================================================================
try({
  res_df <- bind_cols(
    sub_n=rep(d_clean_unique$sub_n,times=1:n_draws),
    set=rep(d_clean_unique$set,times=1:n_draws),
    distance=rep(d_clean_unique$distance,times=1:n_draws),
    target=p[,,1],
    competitor=p[,,2],
    decoy=p[,,3]) %>%
    pivot_longer(c(target,decoy,competitor),names_to = "stim", values_to = "prop") %>%
    group_by(sub_n,set,stim) %>%
    summarise(mean_prop=mean(prop),
              sd_prop=sd(prop),
              se_prop=sd(prop)/sqrt(n()),
              hdi_lower=HDInterval::hdi(prop)[1],
              hdi_upper=HDInterval::hdi(prop)[2]) %>%
    ungroup()
  tryCatch(qsave(res_df,file=path(results_dir,"sim_choice_preds_clean_by_set_avgd.qs")),
           error=function(e) print(e))
  tryCatch(save(res_df,file=path(results_dir,"sim_choice_preds_clean_by_set_avgd.RData")),
           error=function(e) print(e))
  tryCatch(write_csv(res_df,path(results_dir,"sim_choice_preds_clean_by_set_avgd.csv")),
           error=function(e) print(e))
  
})

try(rm(res_df)) # VERY CLUNKY 

try({
  res_df <- bind_cols(
    sub_n=rep(d_clean_unique$sub_n,times=1:n_draws),
    distance=rep(d_clean_unique$distance,times=1:n_draws),
    target=p[,,1],
    competitor=p[,,2],
    decoy=p[,,3]) %>%
    pivot_longer(c(target,decoy,competitor),names_to = "stim", values_to = "prop") %>%
    group_by(sub_n,stim) %>%
    summarise(mean_prop=mean(prop),
              sd_prop=sd(prop),
              se_prop=sd(prop)/sqrt(n()),
              hdi_lower=HDInterval::hdi(prop)[1],
              hdi_upper=HDInterval::hdi(prop)[2]) %>%
    ungroup()
  tryCatch(qsave(res_df,file=path(results_dir,"sim_choice_preds_clean_collapsed_avgd.qs")),
           error=function(e) print(e))
  tryCatch(save(res_df,file=path(results_dir,"sim_choice_preds_clean_collapsed_avgd.RData")),
           error=function(e) print(e))
  tryCatch(write_csv(res_df,path(results_dir,"sim_choice_preds_clean_collapsed_avgd.csv")),
           error=function(e) print(e))
  
})
try(rm(res_df))
try(rm(p))
try(rm(mu))
try(rm(d_clean))
try(rm(d_clean_unique))
try(rm(chunks))
try(rm(s))
try(rm(cors))
try(rm(stan_data))
try(rm(results_dir))
try(rm(cv_all))




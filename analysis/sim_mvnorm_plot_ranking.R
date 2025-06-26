# simulating rankings using comparable params to experiment
# figure goes in dissertation
# Setup =================================================================================
rm(list=ls())
library(tidyverse)
library(here)
library(glue)
library(fs)
library(mvtnorm)
library(latex2exp)
library(matrixcalc)

# CONTROL PARAMS
N <- 1e6 # number of draws

# set parameters ====================================================================
mu_tc <- 0
mu_d <- -.1
mu_all <- c(mu_tc,mu_tc,mu_d)
s <- c(1/3,1/3,1/3) 

r_cd <- r_tc <- .65
r_td <- .75
  
# get all covariance matrices from model =======================================================
get_cv <- function(s,cors) matrix(cors,nrow=3,ncol=3,byrow=F) * (s %*% t(s) )

# function for simulation ===================================================================
sim_mvnorm <- function(N,mu,cv){
  samp <- rmvnorm(N,mu,cv) # sim from multivariate normal
  mx <- apply(samp, 1, which.max)
  mn <- apply(samp, 1, which.min)
  return(
    tibble(max=case_when(
      mx==1~"t",
      mx==2~"c",
      mx==3~"d"
    ),
    min=case_when(
      mn==1~"t",
      mn==2~"c",
      mn==3~"d"
    ),
    middle=case_when(
      max=="t" & min=="c"~"d",
      max=="c" & min=="t"~"d",
      max=="t" & min=="d"~"c",
      max=="d" & min=="t"~"c",
      max=="c" & min=="d"~"t",
      max=="d" & min=="c"~"t"
    ),
    order=str_c(max,middle,min)
  ) %>%
    select(order))
}


sim_res <- matrix(NA,nrow=0,ncol=3)
cor_tmp <- matrix(c(1,r_tc,r_td,
                    r_tc,1,r_cd,
                    r_td,r_cd,1),nrow=3,ncol=3,byrow=T)
cv_tmp <- get_cv(s,cor_tmp)
sim <- sim_mvnorm(N,mu_all,cv_tmp)

sim_res <- sim %>%
  group_by(order) %>%
  summarise(N=n()) %>%
  ungroup() %>%
  mutate(prop=N/sum(N))


sim_res %>%
  ggplot(aes(reorder(order,-prop),prop))+
  geom_col(position="dodge",fill="lightblue")+
  scale_y_continuous(limits=c(0,.3))+
  labs(x="ranking",y="proportion")+
  ggthemes::theme_few()
ggsave(filename = here("analysis/plots/sim_mvnorm_rank.jpeg"),width=3,height=3.5)

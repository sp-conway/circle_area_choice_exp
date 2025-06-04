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
N <- 200000 # number of draws

# set parameters ====================================================================
mu_tc <- 0
mu_d <- -.2
mu_all <- c(mu_tc,mu_tc,mu_d)
s <- c(1/3,1/3,1/3) 

r_cd <- r_tc <- .54

r_td_all <- seq(0,.99,.01)
n_cors <- length(r_td_all)
# get all covariance matrices from model =======================================================
get_cv <- function(s,cors) matrix(cors,nrow=3,ncol=3,byrow=F) * (s %*% t(s) )

# function for simulation ===================================================================
sim_mvnorm <- function(N,mu,cv){
  samp <- rmvnorm(N,mu,cv) # sim from multivariate normal
  diff <- samp[,1]-samp[,3] # target - decoy
  return(diff)
}


sim_res <- matrix(NA,nrow=0,ncol=3)
for(r_td in r_td_all){
  print(r_td)
  cor_tmp <- matrix(c(1,r_tc,r_td,
                      r_tc,1,r_cd,
                      r_td,r_cd,1),nrow=3,ncol=3,byrow=T)
  cv_tmp <- get_cv(s,cor_tmp)
  if(is.positive.semi.definite(cv_tmp)){
    tmp <- sim_mvnorm(N,mu_all,cv_tmp)
  }else{
    tmp <- rep(NA_real_,N)
  }
  sim_res <- rbind(sim_res,cbind(mu_d,r_td,tmp))
}


sim_df <- tibble(
  mu_diff=round(sim_res[,1],digits=3),
  r_td=sim_res[,2],
  td_diff=sim_res[,3]
) %>%
  group_by(r_td) %>%
  reframe(
    td_diff=quantile(td_diff,probs = c(.025,.975)),
    q=c("l","u")) %>%
  ungroup() %>%
  pivot_wider(names_from = q, values_from = td_diff)
poly_df <- bind_rows(
  sim_df %>% select(r_td, y = l),
  sim_df %>% arrange(desc(r_td)) %>% select(r_td, y = u)
)

poly_df %>%
  ggplot() +
  geom_polygon(data=poly_df,aes(x = r_td, y = y),fill = "lightgray",alpha = 0.5)+
  geom_hline(aes(yintercept=mu_tc-mu_d),linetype="dashed",col="red")+
  scale_y_continuous(limits=c(-1.5,1.5))+
  ggthemes::theme_few()+
  labs(x=TeX("$\\rho_{\\;td}$"),y="target sample - decoy sample")+
  theme(axis.text.x=element_text(angle=90),
        text=element_text(size=16))
ggsave(filename=here("analysis","plots","sim_mvnorm_vary_r_td.jpeg"),
       width=4,height=3)



# Setup =================================================================================
rm(list=ls())
library(tidyverse)
library(here)
library(glue)
library(fs)
library(mvtnorm)
library(latex2exp)
# library(doParallel)



# load in modeling results ====================================================================
mu_td <- c(0,0)
mu_diffs <- c(9.6-9.58)#,
              ##9.59-9.565,
              #9.58-9.55,
              # 9.57-9.52)
s <- c(0.3351327, 0.3377775, 0.3346760) # means from modeling

r_cd <- r_tc <- .54

r_td_all <- seq(-.99,.99,.01)

n_cors <- length(r_td_all)
# get all covariance matrices from model =======================================================
get_cv <- function(s,cors){
  cors <- matrix(cors,nrow=3,ncol=3,byrow=F)
  cors * (s %*% t(s) )
}

# function for simulation ===================================================================
sim_mvnorm <- function(N,mu,cv){
  samp <- mvtnorm::rmvnorm(N,mu,cv)
  diff <- samp[,1]-samp[,3]
  return(diff)
}
# CONTROL PARAMS
N <- 5000
sim_res <- matrix(NA,nrow=0,ncol=3)
for(mu_d in mu_diffs){
  print(mu_d)
  mu_tmp <- c(mu_td,mu_td[1]-mu_d)
  for(r_td in r_td_all){
    print(r_td)
    cor_tmp <- matrix(c(1,r_tc,r_td,
                        r_tc,1,r_cd,
                        r_td,r_cd,1),nrow=3,ncol=3,byrow=T)
    cv_tmp <- get_cv(s,cor_tmp)
    tmp <- sim_mvnorm(N,mu_tmp,cv_tmp)
    sim_res <- rbind(sim_res,cbind(mu_d,r_td,tmp))
  }
}

sim_df <- tibble(
  mu_diff=round(sim_res[,1],digits=3),
  r_td=sim_res[,2],
  td_diff=sim_res[,3]
)

sim_df %>%
  # mutate(r_td=as.factor(r_td)) %>%
  ggplot(aes(r_td,td_diff))+
  geom_point(shape=".",alpha=.5)+
  # geom_hline(yintercept=0)+
  geom_hline(aes(yintercept=mu_diff),linetype="dashed",col="red")+
  geom_vline(xintercept=.67,col="blue",linetype="dashed",alpha=.5)+
  # annotate(geom="text",x=.9,y=-1.5,size=3,label="Red line marks true \ndifference in means.")+
  # annotate(geom="text",x=.9,y=1.5,size=3,label="Blue line marks \nobserved correlation \nfrom triangle condition.")+
  # facet_grid(mu_diff~.,scale="free_y")+
  ggthemes::theme_few()+
  labs(x=TeX("$\\rho_{target-decoy}$"),y="target sample - decoy sample")+
  theme(axis.text.x=element_text(angle=90))
ggsave(filename=here("plots","sim_mvnorm_vary_r_td.jpeg"),
       width=5.5,height=4)

sim_df %>%
  mutate(r_td=round(r_td,digits=2)) %>%
  filter(r_td==.67) %>%
  summarise(vartd=var(td_diff),
            sdtd=sd(td_diff))

two_afc <- here("2afc","data_means_discrim_TD_collapsed.csv") %>%
  read_csv()


# function for simulation ===================================================================
# CONTROL PARAMS
N <- 500000
sim_mvnorm2 <- function(N,mu,cv){
  samp <- mvtnorm::rmvnorm(N,mu,cv)
  # browser()
  # diff <- mean( samp[,1]<samp[,3] ) - mean(samp[,2]<samp[,3])
  # diff <- (samp[,1]-samp[,3])  - (samp[,2]-samp[,3])
  p <- mean((samp[,1] < samp[,3]) & (samp[,2] > samp[,3]))
  return(p)
}

sim_res <- matrix(NA,nrow=0,ncol=3)
for(mu_d in mu_diffs){
  print(mu_d)
  mu_tmp <- c(mu_td,mu_td[1]-mu_d)
  for(r_td in r_td_all){
    print(r_td)
    cor_tmp <- matrix(c(1,r_tc,r_td,
                        r_tc,1,r_cd,
                        r_td,r_cd,1),nrow=3,ncol=3,byrow=T)
    cv_tmp <- get_cv(s,cor_tmp)
    tmp <- sim_mvnorm2(N,mu_tmp,cv_tmp)
    sim_res <- rbind(sim_res,cbind(mu_d,r_td,tmp))
  }
}

sim_df <- tibble(
  mu_diff=round(sim_res[,1],digits=3),
  r_td=sim_res[,2],
  p=sim_res[,3]
)

sim_df %>%
  # mutate(r_td=as.factor(r_td)) %>%
  ggplot(aes(r_td,p))+
  geom_smooth(se = F)+
  # geom_point(alpha=.5)+
  # geom_hline(yintercept=0)+
  # geom_hline(aes(yintercept=mu_diff),linetype="dashed",col="red")+
  geom_vline(xintercept=.67,col="red",linetype="dashed",alpha=3)+
  # annotate(geom="text",x=.9,y=-1.5,size=3,label="Red line marks true \ndifference in means.")+
  # annotate(geom="text",x=.9,y=1.5,size=3,label="Blue line marks \nobserved correlation \nfrom triangle condition.")+
  # facet_grid(mu_diff~.,scale="free_y")+
  ggthemes::theme_few()+
  labs(x=TeX("$\\rho_{target-decoy}$"),
       y="p(D>T) & (D<C)")+
  theme(axis.text.x=element_text(angle=90))
ggsave(filename=here("plots","sim_mvnorm_vary_r_td_prob_diffs.jpeg"),
       width=5.5,height=4)




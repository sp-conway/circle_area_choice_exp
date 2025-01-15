rm(list=ls())
library(mvtnorm)
library(tidyverse)
library(glue)
library(here)
n_sim <- 10000 
n_sub <- 50
n_trial <- 50
tc_mu <- 1
d_mu <- .8
r_tc <- r_cd <- .4
r_td <- .6
cor_mat <- matrix(c(1,r_tc, r_td, 
                    r_tc, 1, r_cd,
                    r_td,r_cd,1),
                  nrow=3,byrow=T)
sigma <- rep(1,3)
a <- sigma %*% t(sigma)
cv <- cor_mat*a

r_tc_rep <- r_td_rep <- r_cd_rep <- numeric(n_sim)

for(s in 1:n_sim){
  print(s)
  mu_sub_tmp <- rnorm(n_sub)
  mu_all_tmp <- matrix(NA,n_sub,3)
  
  mu_all_tmp[,1] <- tc_mu+mu_sub_tmp
  mu_all_tmp[,2] <- tc_mu+mu_sub_tmp
  mu_all_tmp[,3] <- d_mu+mu_sub_tmp
  
  dtmp <- array(NA,c(n_sub,n_trial,3))
  for(i in 1:n_sub){
    dtmp[i,,] <- rmvnorm(n_trial, mean=mu_all_tmp[i,],  sigma=cv)
  }
  
  d_adj_tmp <- array(NA, dim(dtmp))
  mu_sub_rep_tmp <- apply(dtmp, 1, mean)
  for(i in 1:n_sub){
    d_adj_tmp[i,,] <- dtmp[i,,]-mu_sub_rep_tmp[i]
  }
  
  # target - competitor correlation. should be .4
  r_tc_rep[s] <-  cor(as.vector(d_adj_tmp[,,1]),as.vector(d_adj_tmp[,,2]))
  # decoy - competitor correlation. should be .4
  r_cd_rep[s] <- cor(as.vector(d_adj_tmp[,,2]),as.vector(d_adj_tmp[,,3]))
  # target - decoy correlation. should be .6
  r_td_rep[s] <- cor(as.vector(d_adj_tmp[,,1]),as.vector(d_adj_tmp[,,3]))
}

sim <- tibble(
  r_td=r_td_rep,
  r_cd=r_cd_rep,
  r_tc=r_tc_rep
) %>%
  pivot_longer(everything(),names_to = "stim",values_to = "r") %>%
  mutate(gen_val=case_when(
    str_detect(stim,"tc|cd")~r_tc,
    str_detect(stim,"td")~r_td,
  ))
ggplot(sim,aes(r))+
  geom_histogram(fill="lightblue",col="black")+
  geom_vline(aes(xintercept=gen_val),linetype="dashed",linewidth=.8,col="red")+
  scale_x_continuous(limits=c(0.2,.7))+
  facet_grid(stim~.)+
  labs(subtitle = glue("true r_cd = r_tc={r_tc}\ntrue r_td={r_td}"),
       caption = "red line marks true value.")+
  ggthemes::theme_few()+
  theme(text = element_text(size=15),
        plot.subtitle = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0))
ggsave(filename = here("plots","gen_fake_data_recover_cors.jpeg"),width=4,height=5)

rm(list=ls())
library(here)
library(tidyverse)
library(mvtnorm)

load(here("tmp_for_brownbag","att_pars_tmp.RData"))
load(here("tmp_for_brownbag","att_props_mean_by_set_tmp.RData"))
disp_cond_all <- c("horizontal","triangle")
distance_all <- c(2,5,9,14)
set_all <- c("a-b-da","a-b-db")
n_cond <- length(disp_cond_all)
n_dist <- length(distance_all)
n_set <- length(set_all)
n_stim <- 3
mean_pars_clean <- mean_pars %>%
  arrange(disp_cond, distance, set, stim_by_type) %>%
  select(-c(se,se_lower,se_upper))
mu <- array(NA, dim=c(n_cond,n_dist,n_set,n_stim))
for(i in 1:n_cond){
  for(j in 1:n_dist){
    for(k in 1:n_set){
      mu_tmp <- filter(mean_pars_clean, disp_cond==disp_cond_all[i] & distance==distance_all[j] & set==set_all[k]) %>%
        pull(m_ca)
      if(set_all[k]=="a-b-da"){
        mu_tmp1 <- mu_tmp
      }else{
        mu_tmp1 <- c(mu_tmp[2],mu_tmp[1],mu_tmp[3])
      }
      mu[i,j,k,] <- mu_tmp1
    }
  }
}


make_cv <- function(d, conds){
  cv <- array(NA, dim=c(n_cond, 3, 3))
  k <- 0
  for(cond in conds){
    k <- k+1
    dd <- d %>%
      filter(disp_cond==cond)
    cv[k,,] <- matrix(c(dd$var_t, dd$cov_tc, dd$cov_td, dd$cov_tc, dd$var_c, dd$cov_cd, dd$cov_td, dd$cov_cd, dd$var_d),
                     nrow=3,ncol=3,byrow=T)
  }
  return(cv)
}
cv <- make_cv(cov_pars, disp_cond_all)

# simulations ======================================================================
sim_choice_model <- function(n, mu, cv, set, cond, distance, beta=NULL){
  if(is.null(beta)){
    tmp <- rmvnorm(n, mu, cv)
    s=as.character(apply(tmp, 1, which.max))
  }else{
    tmp <- rmvnorm(n, mu, cv)
    softmax <- function(x, beta) exp(beta*x)/sum(exp(beta*x))
    p <- apply(tmp,1,softmax, beta=beta)
    s <- map_dbl(1:n, ~sample(c(1,2,3),1, prob=p[,.x]))
  }
  # browser()
  d <- tibble(n=1:n,
         s=s,
         disp_cond=cond,
         set=set, 
         distance=distance,
         choice=case_when(
           set=="a-b-da" ~ str_replace_all(s,c("1"="a","2"="b","3"="d")),
           set=="a-b-db" ~ str_replace_all(s,c("1"="b","2"="a","3"="d")),
         )) %>%
    select(-s)
  return(d)
}

sim_model_wrapper <- function(N, disp_cond_all, set_all,distance_all, mu, cv, beta=NULL){
  sim <- tibble()
  for(i in 1:n_cond){
    for(j in 1:n_dist){
      for(k in 1:n_set){
        cat("\n=====================\n",ifelse(is.null(beta),"Standard","Softmax"),"\n", i,"/",n_cond,",",j,"/",n_dist,",",k,"/",n_set,"\n","\n=====================\n")
        tmp <- sim_choice_model(n=N, mu=mu[i,j,k,],cv=cv[i,,],cond=disp_cond_all[i], distance = distance_all[j], set=set_all[k],beta=beta)
        sim <- bind_rows(sim,tmp)
      }
    }
  }
  return(sim)
}


summarise_model <- function(d){
  d %>%
    group_by(disp_cond, set, distance, choice) %>%
    summarise(n=n()) %>%
    group_by(disp_cond, set, distance) %>%
    mutate(prop=n/sum(n)) %>%
    ungroup() %>%
    select(-n) %>%
    mutate(source="model",
           distance=as.factor(distance))
}

combine_data <- function(data, model){
  data %>%
    rename(choice=choice_abd,
           prop=m) %>%
    select(-c(lwr,upr)) %>%
    mutate(source="data") %>%
    bind_rows(model)
}
N <- 100000
beta <- 4
sim_res_no_sm <- sim_model_wrapper(N=N, disp_cond_all=disp_cond_all, set_all=set_all, distance_all=distance_all, mu=mu, cv=cv)
sim_prop_no_sm <- summarise_model(sim_res_no_sm)
data_all_no_sm <- combine_data(att_mean_choice_props_by_set,sim_prop_no_sm)
sim_res_sm <- sim_model_wrapper(N=N, disp_cond_all=disp_cond_all, set_all=set_all, distance_all=distance_all, mu=mu, cv=cv, beta=beta)
sim_prop_sm <- summarise_model(sim_res_sm)
data_all_sm <- combine_data(att_mean_choice_props_by_set,sim_prop_sm)

plot_results <- function(d){
  d %>%
    ggplot(aes(distance,prop,col=choice,shape=source))+
    geom_point(size=2.95,alpha=.8)+
    # geom_errorbar(aes(ymin=lwr,ymax=upr),width=.25)+
    scale_shape_manual(values=c(1,4))+
    scale_x_discrete(labels=c("2%","5%","9%","14%"))+
    scale_y_continuous(limits=c(0,.8))+
    labs("target-decoy attribute distance",y="mean proportion")+
    lemon::facet_rep_grid(disp_cond~set, scales = "free_x", repeat.tick.labels = "bottom")+
    ggthemes::theme_few()
}
plot_results(data_all_no_sm)
ggsave(filename=here("tmp_for_brownbag","sim_res_no_sm.jpeg"),width=5,height=5)
plot_results(data_all_sm)
ggsave(filename=here("tmp_for_brownbag","sim_res_sm.jpeg"),width=5,height=5)


rm(list=ls())
library(tidyverse)
library(glue)
library(here)
N_sim <- 1000
N_trial <- 8000
betas_gen <- betas_rec <- matrix(NA,N_sim,4)
options(digits = 20)

for(type in c("raw","sqlog","logsq")){
  for(i in 1:N_sim){
    print(i)
    w <- runif(N_trial, 56,195)
    h <- runif(N_trial, 56,195)
    
    a <- w*h
    la <- log(a)
    
    if(type=="raw"){
      ar <- pmax(w,h)/pmin(w,h)
      ars <- ar^2
    }else if(type=="sqlog"){
      ar <- log(pmax(w,h)/pmin(w,h))
      ars <- log(pmax(w)/pmin(h))^2
    }else if(type=="logsq"){
      ar <- log(pmax(w,h)/pmin(w,h))
      ars <- log(pmax(w)/pmin(h)^2)
    }
    
    beta_0 <- rnorm(1,0,10)
    beta_la <- rnorm(1,0,10)
    beta_ar <- rnorm(1,0,10)
    beta_ars <- rnorm(1,0,10)
    
    y <- beta_0 + beta_la*la + beta_ar*ar + beta_ars*ars + rnorm(N_trial,0,20)
    
    fit <- lm(y~la+ar+ars)
    f <- summary(fit)
    betas_gen[i,] <- c(beta_0,beta_la,beta_ar,beta_ars)
    betas_rec[i,] <- f$coefficients[,1]
  }
  
  colnames(betas_gen) <- colnames(betas_rec) <- c("b_0","b_la","b_ar","b_ars")
  d <- bind_rows(as.data.frame(betas_gen),as.data.frame(betas_rec)) %>%
    mutate(sim=c(1:N_sim,1:N_sim),
           type=c(rep("gen",N_sim),rep("rec",N_sim)))
  dd <- d %>%
    pivot_longer(-c(sim,type),names_to = "param") %>%
    pivot_wider(names_from = type,values_from = value)
  
  dd %>%
    ggplot(aes(gen,rec))+
    geom_point(shape=".")+
    coord_fixed(xlim=c(-50,50),ylim=c(-50,50))+
    facet_grid(param~.)+
    ggthemes::theme_few()+
    theme(text=element_text(size=16))
  ggsave(filename=here("analysis","plots",glue("{type}_filler_sim_rec.jpeg")),width=4,height=6)
  dd %>%
    group_by(param) %>%
    summarise(r=cor(gen,rec))
  
}

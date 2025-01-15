rm(list=ls())
library(here)
library(tidyverse)
library(fs)
library(latex2exp)
library(glue)
library(rstanarm)
library(ggsci)
# minimum radius
min_rad <- 5
min_area <- pi*min_rad^2
min_log_area <- log(min_area)

d <- here("data","circle_area","aggregated","circle_area_all.csv") %>%
  read_csv() %>%
  mutate(disp_cond=factor(disp_cond,levels=c("triangle","horizontal"))) %>%
  filter(str_detect(effect,"filler"))

clean_data <- function(dat,cond){
  dat %>%
    filter(str_detect(disp_cond,cond)) %>%
    mutate(asp1=if_else(h1>w1,h1/w1,w1/h1),
           asp2=if_else(h2>w2,h2/w2,w2/h2),
           asp3=if_else(h3>w3,h3/w3,w3/h3)) %>%
    select(-c(computer_n,effect,contains("_rad"),rt,h1,h2,h3,w1,w2,w3,set,distance,diag)) %>%
    pivot_longer(c(contains("circle"),contains("rect"),contains("asp"))) %>%
    mutate(name=str_remove(name,"_area")) %>%
    separate(name,into=c("var","stim"),sep = "(?=[0-9])") %>% 
    pivot_wider(names_from = var,
                values_from = value) %>%
    filter(circle!=min_area) %>%
    mutate(circle=log(circle),
           rect=log(rect),
           asp=log(asp),
           aspsq=asp^2) %>%
    group_by(sub_n,trial_n,block_n) %>%
    mutate(circle=circle-mean(circle)) %>%
    ungroup() %>%
    mutate(rect=rect-mean(rect),
           sub_n=as.factor(sub_n))
  
}

triangle <- clean_data(d,"triangle")
horizontal <- clean_data(d,"horizontal")

m_tri_quad <- stan_glmer(circle~rect+asp+aspsq+(1|sub_n),data=triangle,
                         chains=4,iter=1000,cores=4)

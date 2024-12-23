rm(list=ls())
library(here)
library(tidyverse)
library(fs)
library(glue)
library(lme4)
library(lmerTest)
sean_data <- F
if(sean_data){
  dataf <- here("sean_test_data","circle_area","aggregated","sean_circle_area_cleaned_for_modeling.csv") 
}else{
  dataf <- here("data","circle_area","aggregated","circle_area_logtransformed_cleaned_no_outliers.csv") 
}

conds <- c("horizontal","triangle")
load_data <- function(f,which_cond){
  f %>%
    read_csv() %>%
    filter(str_detect(disp_cond,which_cond)) %>%
    select(-contains("identity")) %>%
    pivot_longer(c(t,c,d),names_to = "stim", values_to = "la") %>% # log area
    mutate(w=as.integer((set=="a-b-da" & stim=="c") | (set=="a-b-db" & stim=="t") | (set=="a-b-db" & stim=="d")),
           diag2=as.integer(diag==2),
           diag3=as.integer(diag==3),
           # d=as.integer(stim=="d"),
           d5=as.integer(distance==5),
           d9=as.integer(distance==9),
           d14=as.integer(distance==14),
           d2d=as.integer(distance==2 & stim=="d"),
           d5d=as.integer(distance==5 & stim=="d"),
           d9d=as.integer(distance==9 & stim=="d"),
           d14d=as.integer(distance==14 & stim=="d"),
           la_c=la - mean(la) )
  
}
mydir <- here(glue("bayes_no_cor{tmp}",tmp=ifelse(sean_data,"_sean","")))
dir_create(mydir)

run_m_fe <- function(data, f){
  if(!file_exists(f)){
    m <- glm(la_c~w+diag2+diag3+d5+d9+d14+d2d+d5d+d9d+d14d,data=data)
    save(m, file=f)
  }
}

run_m_me <- function(data, f){
  if(!file_exists(f)){
    m <- lm(la_c~w+diag2+diag3+d5+d9+d14+d2d+d5d+d9d+d14d+(1|sub_n))
    save(m, file=f)
  }
}

for(cond in conds){
  d <- load_data(dataf, cond)
  ffe <- path(mydir,"fit_fe.RData")
  fme <- path(mydir,"fit_me.RData")
  run_m_fe(d,ffe)
  run_m_me(d, fme)
}

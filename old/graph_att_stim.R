rm(list=ls())
library(here)
library(tidyverse)
library(readxl)
circle <- here("specs","circle_trials.xlsx") %>%
  read_excel() %>%
  filter(str_detect(effect,"att"))
choice <- here("specs","choice_trials.xlsx") %>%
  read_excel() %>%
  filter(str_detect(effect,"att"))
do_plot <- function(d){
  d %>% 
    pivot_longer(c(h1,w1,h2,w2,h3,w3)) %>%
    mutate(name1=str_remove(name,"[:digit:]"),
           rect=str_remove(name,"[:alpha:]")) %>%
    select(-name) %>%
    pivot_wider(names_from = name1,values_from = value) %>%
    ggplot(aes(w,h))+
    geom_point()+
    facet_wrap(vars(set))+
    ggthemes::theme_few()
}
do_plot(circle)
do_plot(choice)

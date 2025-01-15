rm(list=ls())
library(tidyverse)
library(here)
library(fs)

pcir <- here("specs","circle_trial_params")
pcho <- here("specs","choice_trial_params")

fcir <- dir_ls(pcir,regexp = "*.txt")
fcho <- dir_ls(pcho,regexp="*.txt")

r <- function(f){
  x <- read_lines(f)
  x <- as.data.frame(x)
  ff <- basename(f) %>% 
    str_remove("\\.txt$")
  colnames(x) <- ff
  return(x)
}

dcir <- map_dfc(fcir,r) %>%
  mutate(across(c(h1,h2,h3,w1,w2,w3),as.numeric))
dcho <- map_dfc(fcho, r) %>%
  mutate(across(c(h1,h2,h3,w1,w2,w3),as.numeric))

dcir2 <- dcir %>%
  rowwise() %>%
  mutate(set2=case_when(
    sum(c(h1>w1,h2>w2,h3>w3))==2~"a-b-da",
    sum(c(h1>w1,h2>w2,h3>w3))==1~"a-b-db"
  )) %>%
  ungroup() %>%
  relocate(set,.before=set2)

dcho2 <- dcho %>%
  rowwise() %>%
  mutate(set2=case_when(
    sum(c(h1>w1,h2>w2,h3>w3))==2~"a-b-da",
    sum(c(h1>w1,h2>w2,h3>w3))==1~"a-b-db"
  )) %>%
  ungroup() %>%
  relocate(set,.before=set2)

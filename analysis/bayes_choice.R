rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(glue)
library(cmdstanr)

which_cond <- "triangle"
which_model <- "bayes_choice_1"
stan_dir <- here("analysis","bayes_choice","stan")
stan_file <- path(stan_dir,glue("{which_model}.stan"))

d <- here("data","choice","aggregated","choice_all.csv") %>%
  read_csv() %>%
  filter(str_detect(effect,"attraction") & 
         str_detect(disp_cond,"triangle")) %>%
  mutate(choice_tdc=factor(choice_tdc,
                           levels=c("target","competitor","decoy")))
n_subs <- length(unique(d$sub_n))

subs_key <- tibble(
  sub_n = sort(unique(d$sub_n)),
  sub_n_new = seq(1,n_subs,1)
)
d_counts <- d %>%
  group_by(sub_n,distance,set,choice_tdc) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  pivot_wider(names_from = choice_tdc, values_from = n, values_fill = 0) %>%
  left_join(subs_key) %>%
  relocate(sub_n_new,.after=sub_n) 

tw <- dw <- if_else(d_counts$set=="w",1,0) # target and decoy always oriented the same
cw <- if_else(d_counts$set=="h",1,0) # if target h then comp w and vice versa
distance5 <- if_else(d_counts$distance==5,1,0)
distance9 <- if_else(d_counts$distance==9,1,0)
distance14 <- if_else(d_counts$distance==14,1,0)
sub_ns_new <- d_counts$sub_n_new
N <- nrow(d_counts)
J <- n_subs
choice_counts <- as.matrix(d_counts[,c("target","competitor","decoy")])
stan_data <- list(
  tw=tw,
  cw=cw,
  dw=dw,
  sub_ns_new=sub_ns_new,
  N=N,
  J=J,
  distance5=distance5,
  distance9=distance9,
  distance14=distance14,
  choice_counts=choice_counts
)


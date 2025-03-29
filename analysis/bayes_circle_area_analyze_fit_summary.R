rm(list=ls())
library(tidyverse)
library(here)

which_model <- "sigma_constant_comp_effect"
load_fit <- function(cond){
  f <- here("analysis","bayes",which_model,cond,"no_outliers","fit_summary.RData")
  load(f)
  fit_summary %>%
    mutate(disp_cond=cond) %>%
    relocate(disp_cond,.before=everything())
}

d <- map(c("triangle","horizontal"),load_fit) %>%
  list_rbind()
d %>%
  filter(variable=="bcomp")

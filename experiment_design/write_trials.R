rm(list=ls())
library(tidyverse)
library(readxl)
library(here)
library(glue)
library(fs)

for(phase in c("choice", "circle")){
  trials <- here('specs',glue('{phase}_trials.xlsx')) %>%
                   read_excel() %>%
                   mutate(across(c(w1,h1,w2,h2,w3,h3),replace_na,0),
                          distance=replace_na(as.numeric(distance),0))
   trial_params_dir <- here("specs",glue("{phase}_trial_params"))
   dir_create(trial_params_dir)
   for(i in 1:ncol(trials)){
     tmp <- unname(unlist(as.vector(trials[,i])))
     tmp_file <- glue("{trial_params_dir}/{colnames(trials)[i]}.txt")
     fmt <- ifelse(is.numeric(tmp),"%.5f","%s")
     write_lines(sprintf(fmt,tmp),
                 file = tmp_file,
                 append = F,
                 sep = "\n")
   }
}

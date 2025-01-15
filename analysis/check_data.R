rm(list=ls())
# checking data specs for paper
library(here)
library(tidyverse)

# circle
circle  <- here("data","circle_area","aggregated","circle_area_all.csv") %>%
  read_csv() %>%
  mutate(disp_cond=factor(disp_cond,levels=c("triangle","horizontal")))

circle %>%
  filter(block_n>0 & effect!="calibration") %>%
  group_by(sub_n,block_n) %>%
  summarise(n=n()) %>%
  ungroup()  %>%
  arrange(desc(n))

circle %>%
  filter(block_n>0 & effect!="calibration") %>%
  group_by(sub_n,block_n,effect) %>%
  summarise(n=n()) %>%
  ungroup()  %>%
  arrange(desc(sub_n),desc(n))

circle %>%
  filter(effect=="attraction") %>%
  group_by(sub_n,block_n,distance) %>%
  summarise(n=n()) %>%
  ungroup()  %>%
  arrange(desc(sub_n),desc(n))

circle %>%
  filter(effect=="attraction") %>%
  group_by(sub_n,block_n,distance,set) %>%
  summarise(n=n()) %>%
  ungroup()  %>%
  arrange(desc(sub_n),desc(n))

choice <- here("data","choice","aggregated","choice_all.csv") %>%
  read_csv() %>%
  mutate(disp_cond=factor(disp_cond,levels=c("triangle","horizontal")))

choice %>%
  filter(effect!="practice" & block_n>0) %>%
  group_by(sub_n,block_n) %>%
  summarise(n=n()) %>%
  ungroup()  %>%
  arrange(desc(n))

choice %>%
  filter(effect!="practice" & block_n>0) %>%
  group_by(sub_n,block_n,effect) %>%
  summarise(n=n()) %>%
  ungroup()  %>%
  arrange(desc(sub_n),desc(n))


choice %>%
  filter(effect=="attraction") %>%
  group_by(sub_n,block_n,distance) %>%
  summarise(n=n()) %>%
  ungroup()  %>%
  arrange(desc(sub_n),desc(n))

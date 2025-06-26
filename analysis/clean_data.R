rm(list=ls())

library(here)
library(tidyverse)
library(fs)
library(glue)

# utility functions - for attraction choice specs
source(here("analysis","utility_functions.R"))

# circle area data
data_path_ca <- here("data","circle_area","raw")
data_path_ca_sub_dup <- here("data","circle_area","raw","duplicate")
data_files_ca_raw <- dir_ls(data_path_ca,type="file")
data_files_ca_raw_sub_dup <- dir_ls(data_path_ca_sub_dup)# subject numbers that were repeated. need to assign them a new subject # based on computer

# choice data
data_path_ch <- here("data","choice","raw")
data_path_ch_sub_dup <- here("data","choice","raw","duplicate")
data_files_ch_raw <- dir_ls(data_path_ch,type="file")
data_files_ch_raw_sub_dup <- dir_ls(data_path_ch_sub_dup)# subject numbers that were repeated. need to assign them a new subject # based on computer

read_data <- function(f, is_duplicate=F){
  d <- data.table::fread(f) %>%
    as_tibble()
  if(nrow(d)==0){
    return(NULL)
  }
  d <- d %>%
    mutate(across(c(distance,diag),~na_if(.x, 999)),
           sub_n=as.numeric(str_replace_all(sub_n,"\\D",""))) %>%
    rowwise() %>%
    mutate(set=case_when(  # used a-b-da in experiment code but this is confusing so recoding
      effect!="attraction"~NA_character_,
      effect=="attraction" & sum(c(h1>w1,h2>w2,h3>w3))==2 ~ "h",
      effect=="attraction" & sum(c(h1>w1,h2>w2,h3>w3))==1 ~ "w",
    )) %>%
    ungroup()
  
  # Octave rounded the circle area to 1 or 2 digits
  # not a big deal but this is more precise
  if("circle1_area" %in% colnames(d)){
    d$circle1_area <- pi*d$circle1_rad^2
    d$circle2_area <- pi*d$circle2_rad^2
    d$circle3_area <- pi*d$circle3_rad^2
  }
  
  if(is_duplicate){
    # create new subject number by combining subject & computer numbers and adding a couple of zeros
    # ensures no one else will have this number (since the same sub n can't get reused at the same computer)
    d$sub_n <- as.numeric(glue("{unique(d$sub_n)}{unique(d$computer_n)}00")) 
  }
  print(unique(d$sub_n))
  return(d)
}

combine_data <- function(data_files_raw, data_files_raw_sub_dup){
  d1 <- map_dfr(data_files_raw, read_data)
  d2 <- map_dfr(data_files_raw_sub_dup, read_data, is_duplicate=T) 
  d_all <- bind_rows(d1,d2) %>%
    mutate(sub_n=as.factor(sub_n),
           rect1_area=h1*w1,
           rect2_area=h2*w2,
           rect3_area=h3*w3)
  return(d_all)
}

d_all_ca1 <- combine_data(data_files_ca_raw, data_files_ca_raw_sub_dup)
d_all_ch1 <- combine_data(data_files_ch_raw, data_files_ch_raw_sub_dup)
sub_ns_initial <- d_all_ca1 %>%
  distinct(disp_cond, sub_n) %>%
  group_by(disp_cond) %>%
  summarise(n_init=n()) %>%
  ungroup()

# which participants to keep
# only keeping participants who completed the full experiment
subs_finished_ch <- d_all_ch1 %>%
  count(sub_n) %>%
  filter(n==max(n)) %>%
  pull(sub_n)
subs_finished_ca <- d_all_ca1 %>%
  count(sub_n) %>%
  filter(n==max(n)) %>%
  pull(sub_n)
d_all_ca2 <- filter(d_all_ca1, sub_n %in% subs_finished_ca & sub_n %in% subs_finished_ch)
d_all_ch2 <- filter(d_all_ch1, sub_n %in% subs_finished_ca & sub_n %in% subs_finished_ch)

# number who did not complete full experiment
sum(sub_ns_initial$n_init)-length(unique(d_all_ch2$sub_n))

# number of participants per condition after removing participants
d_all_ch2 %>%
  distinct(sub_n, disp_cond) %>%
  group_by(disp_cond) %>%
  summarise(N=n())


# Make sure ppts computer number is the same for both datasets ===================================================
bind_rows(
  distinct(d_all_ca2,sub_n,computer_n,disp_cond),
  distinct(d_all_ca2,sub_n,computer_n,disp_cond),
) %>%
  group_by(sub_n,computer_n) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  distinct(n)

# remove participants who failed to achieve at least 75% correct (6/8) on catch trials ================================================================================
subs_keep <- d_all_ca2 %>%
  filter(str_detect(effect,"catch")) %>%
  mutate(a1=h1*w1,
         a2=h2*w2,
         a3=h3*w3) %>%
  rowwise() %>%
  mutate(rmax=which.max(c(a1,a2,a3)),
         cmax=which.max(c(circle1_area,circle2_area,circle3_area)),
         correct=case_when(
           rmax==cmax~T,
           T~F)) %>%
  ungroup() %>%
  group_by(sub_n) %>%
  summarise(prop=mean(correct)) %>%
  ungroup() %>%
  filter(prop >= .75) %>%
  pull(sub_n)
d_all_ca_clean <- filter(d_all_ca2, sub_n %in% subs_keep)
d_all_ch_clean <- filter(d_all_ch2, sub_n %in% subs_keep) %>%
  filter(rt>=.1 & rt<=10)#remove very fast and very slow RTs !!!!!!

length(unique(d_all_ch2$sub_n)) - length(subs_keep) # n removed for failing catch trials
# number of participants (after removing) ================================================
d_all_ca_clean %>%
  distinct(sub_n,disp_cond) %>%
  group_by(disp_cond) %>%
  summarise(n_keep=n()) %>% 
  ungroup() %>%
  left_join(sub_ns_initial) %>%
  relocate(n_init,.after=disp_cond) %>%
  mutate(n_drop=n_init-n_keep)

d_all_ch_clean %>%
  distinct(sub_n,disp_cond) %>%
  group_by(disp_cond) %>%
  summarise(n_keep=n()) %>% 
  ungroup()%>%
  left_join(sub_ns_initial) %>%
  relocate(n_init,.after=disp_cond) %>%
  mutate(n_drop=n_init-n_keep)

# get attraction specs ================================================================================================================
d_all_ch_clean_w_att_choices <- d_all_ch_clean %>%
  filter(str_detect(effect,"attraction")) %>%
  get_att_specs(data="choice") %>%
  bind_rows(filter(d_all_ch_clean,str_detect(effect,"attraction",negate=T)))

# write data to csv files ================================================================================
write_csv(d_all_ca_clean, here("data","circle_area","aggregated","circle_area_all.csv"))
write_csv(d_all_ch_clean_w_att_choices, here("data","choice","aggregated","choice_all.csv"))

# do the same for sean test data ============================================

# sean data (for testing)
data_path_sean_ca <- here("sean_test_data","circle_area","raw")
data_files_sean_ca_raw <- dir_ls(data_path_sean_ca, type="file")
data_path_sean_ch <- here("sean_test_data","choice","raw")
data_files_sean_ch_raw <- dir_ls(data_path_sean_ch, type="file")
d_sean_ca <- combine_data(data_files_sean_ca_raw,tibble())
d_sean_ch <- combine_data(data_files_sean_ch_raw,tibble())

write_csv(d_sean_ca, here("sean_test_data","circle_area","aggregated","sean_circle_area_clean.csv"))
write_csv(d_sean_ch, here("sean_test_data","choice","aggregated","sean_choice_clean.csv"))
  
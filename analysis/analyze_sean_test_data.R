rm(list=ls())

library(here)
library(fs)
library(tidyverse)

choice <- here("sean_test_data","choice","old") %>%
  dir_ls() %>%
  read_csv()
circle_area <- here("sean_test_data","circle_area","old") %>%
  dir_ls() %>%
  read_csv()

circle_area%>%
  filter(str_detect(effect,"prac|calib",T)) %>%
  select(-c(h1,h2,h3,w1,w2,w3)) %>%
  pivot_longer(c(circle1_area,circle2_area, circle3_area),
               names_to = "circle",values_to = "area") %>%
  mutate(circle=str_remove(circle,"_area")) %>%
  ggplot(aes(circle,area))+
  geom_boxplot()

choice %>%
  filter(str_detect(effect,"att")) %>%
  mutate(a1=h1*w1,
         a2=h2*w2,
         a3=h3*w3,
         d_identity=case_when(
           a1 < a2 & a1 < a3 ~ 1,
           a2 < a1 & a2 < a3 ~ 2,
           a3 < a1 & a3 < a2 ~ 3,
         ),
         t_identity = case_when(
           set=="a-b-da" & d_identity == 1 & h2 > w2 ~ 2,
           set=="a-b-da" & d_identity == 1 & h3 > w3 ~ 3,
           set=="a-b-da" & d_identity == 2 & h3 > w3 ~ 3,
           set=="a-b-da" & d_identity == 2 & h1 > w1 ~ 1,
           set=="a-b-da" & d_identity == 3 & h1 > w1 ~ 1,
           set=="a-b-da" & d_identity == 3 & h2 > w2 ~ 2,
           
           set=="a-b-db" & d_identity == 1 & h2 < w2 ~ 2,
           set=="a-b-db" & d_identity == 1 & h3 < w3 ~ 3,
           set=="a-b-db" & d_identity == 2 & h3 < w3 ~ 3,
           set=="a-b-db" & d_identity == 2 & h1 < w1 ~ 1,
           set=="a-b-db" & d_identity == 3 & h1 < w1 ~ 1,
           set=="a-b-db" & d_identity == 3 & h2 < w2 ~ 2
         ),
         c_identity=case_when(
           d_identity==1 & t_identity==2 ~ 3,
           d_identity==1 & t_identity==3 ~ 2,
           d_identity==2 & t_identity==1 ~ 3,
           d_identity==2 & t_identity==3 ~ 1,
           d_identity==3 & t_identity==1 ~ 2,
           d_identity==3 & t_identity==2 ~ 1,
         ),
         choicet = case_when(
           choice==1 & t_identity==1 ~ "t",
           choice==1 & d_identity==1 ~ "d",
           choice==1 & c_identity==1 ~ "c",
           choice==2 & t_identity==2 ~ "t",
           choice==2 & d_identity==2 ~ "d",
           choice==2 & c_identity==2 ~ "c",
           choice==3 & t_identity==3 ~ "t",
           choice==3 & d_identity==3 ~ "d",
           choice==3 & c_identity==3 ~ "c"
         )) %>%
  group_by(choicet) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  mutate(prop=n/sum(n))

plot_dims <- function(d, effect_type){
  d %>%
    filter(str_detect(effect,effect_type)) %>%
    pivot_longer(c(h1,w1,h2,w2,h3,w3)) %>%
    mutate(dim=str_sub(name,1,1),
           rect=str_sub(name,2,2)) %>%
    select(-c(name)) %>%
    pivot_wider(names_from = dim,values_from = value) %>%
    ggplot(aes(w,h,col=rect))+
    geom_point()+
    coord_fixed(xlim = c(0,300),ylim=c(0,300))+
    ggthemes::theme_few()
  
}
plot_dims(circle_area,"att")
plot_dims(choice,"att")
plot_dims(circle_area,"catch")
plot_dims(circle_area,"filler")
plot_dims(choice,"filler")

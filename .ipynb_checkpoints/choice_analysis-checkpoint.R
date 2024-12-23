# analysis of choice data 
# setup ============================================================
rm(list=ls())
library(here)
library(tidyverse)
library(fs)

source(here("utility_functions.R"))

# import data ============================================================
d <- here("data","choice","aggregated","choice_all.csv") %>%
  read_csv() %>%
  filter(str_detect(effect,"prac",T)) %>%
  mutate(disp_cond=factor(disp_cond, levels=c("triangle","horizontal")),
         distance=as.factor(distance))

# GET COUNTS ========================================================================
# n subs total
length(unique(d$sub_n))

# n subs by display condition
d %>%
  group_by(disp_cond) %>%
  summarise(n=length(unique(sub_n)))


# mutate data to figure out correct / incorrect ====================================
d1 <- d %>%
  mutate(a1=h1*w1,
         a2=h2*w2,
         a3=h3*w3,
         choice_area=case_when(
           choice==1~a1,
           choice==2~a2,
           choice==3~a3
         )) %>%
  rowwise() %>%
  mutate(max_area=max(c(a1,a2,a3))) %>%
  ungroup() %>%
  mutate(correct=case_when(
    choice_area==max_area~1,
    T~0
  )) 

# analyze prop correct ================================================
compute_prop_cor <- function(dat, pl=F, ...){
  dd <-dat %>%
    group_by(sub_n,correct,...) %>%
    summarise(n=n()) %>%
    group_by(sub_n,...) %>%
    mutate(prop=n/sum(n)) %>%
    ungroup() %>%
    filter(correct==1) 
  if(pl){
    ggplot(dd,aes(prop))+
      geom_histogram()+
      facet_wrap(vars(...))+
      scale_x_continuous(limits=c(.7,1))+
      ggthemes::theme_few()
  }else{
    return(dd)
  }
}

# analyze individuals
compute_prop_cor(d1)
compute_prop_cor(d1,pl=T)+labs(title="")
ggsave(filename = here("plots","choicePhase_all_trials_pcorrect.jpg"),width=4,height=4)
compute_prop_cor(d1, pl=T,effect, distance)
ggsave(filename = here("plots","choicePhase_att_trials_pcorrect.jpg"),width=4,height=4)
compute_prop_cor(d1, pl=T,block_n)
ggsave(filename = here("plots","choicePhase_all_trials_by_block_pcorrect.jpg"),width=4,height=4)
compute_prop_cor(d1, pl=T,disp_cond)
ggsave(filename = here("plots","choicePhase_all_trials_by_disp_cond.jpg"),width=4,height=4)

# prop corr means for att. trials
d1 %>%
  filter(str_detect(effect,"att")) %>%
  compute_prop_cor(F, distance,disp_cond) %>%
  group_by(distance,disp_cond) %>%
  summarise(mcor = mean(prop))

# prop corr means overall
prop_corr_disp_cond <- d1 %>%
  compute_prop_cor(F,disp_cond)
prop_corr_disp_cond %>%
  group_by(disp_cond) %>%
  summarise(mcor = mean(prop),
            se=sd(prop)/sqrt(n()),
            se_lower=mcor-se,
            se_upper=mcor+se)

# analyze attraction trials ============================================================
att_choice <- d1 %>%
  get_att_specs(data="choice")
# INDIVIDUAL SUBJECT CHOICES
att_choice_props <- att_choice %>%
  group_by(sub_n,distance,choice_tdc) %>%
  summarise(n=n()) %>%
  group_by(sub_n,distance) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() 

# INDIVIDUAL SUBJECT CHOICES
att_choice_props_by_set <- att_choice %>%
  group_by(sub_n,distance,set,choice_tdc) %>%
  summarise(n=n()) %>%
  group_by(sub_n,set,distance) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() 

# Mean attraction choice props by distance, collapsed across set
att_choice_props %>%
  left_join(distinct(d1,sub_n,disp_cond)) %>%
  select(-n) %>%
  group_by(choice_tdc,distance,disp_cond) %>%
  summarise(m=mean(prop),
            tcrit=qt(.025,n(),lower.tail=F)*sd(prop)/sqrt(n()),
            lwr=m-tcrit,
            upr=m+tcrit) %>%
  ungroup() %>%
  select(-tcrit) %>%
  ggplot(aes(distance,m, shape=choice_tdc))+
  geom_point(size=2.75)+
  scale_shape_manual(values=c(0,2,4),name="choice")+
  # geom_text(aes(label=rectangle),size=6)+
  # geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper),width=.2,alpha=.6)+
  scale_x_discrete(labels=c("2%","5%","9%","14%"))+
  scale_y_continuous(limits=c(0,.8),breaks=seq(0,.8,.2))+
  labs(y="mean choice prop.")+
  facet_grid(disp_cond~.)+
  ggthemes::theme_few()
ggsave(filename = here("plots","choicePhase_att_trials_mean_choice_props_collapsed.jpg"),width=4,height=4)

# Mean attraction choice props by distance and set
att_mean_choice_props_by_set <- att_choice_props_by_set %>%
  left_join(distinct(d1,sub_n,disp_cond)) %>%
  select(-n) %>%
  group_by(choice_tdc,distance,set,disp_cond) %>%
  summarise(m=mean(prop),
            tcrit=qt(.025,n(),lower.tail=F)*sd(prop)/sqrt(n()),
            lwr=m-tcrit,
            upr=m+tcrit) %>%
  ungroup() %>%
  select(-tcrit) 

att_mean_choice_props_by_set %>%
  ggplot(aes(distance,m))+
  geom_point(aes(shape=choice_tdc),size=2.95,alpha=.8)+
  # geom_errorbar(aes(ymin=lwr,ymax=upr),width=.25)+
  scale_shape_manual(values=c(0,2,4),name="choice")+
  scale_x_discrete(labels=c("2%","5%","9%","14%"))+
  scale_y_continuous(limits=c(0,.8), breaks=seq(0,.8, .2))+
  labs("distance",y="mean proportion")+
  lemon::facet_rep_grid(disp_cond~set, scales = "free_x", repeat.tick.labels = "bottom")+
  ggthemes::theme_few()
ggsave(filename = here("plots","choicePhase_att_trials_mean_choice_props_by_dist.jpg"),width=4,height=4)



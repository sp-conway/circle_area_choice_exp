rm(list=ls())
library(tidyverse)
library(here)
source(here("utility_functions.R"))
which_exp <- "1b"
choice_data <- here("data","choice","aggregated","choice_all.csv") %>%
  read_csv() %>%
  filter(str_detect(effect,"attraction"))
read_two_afc <- function(f,which_exp,collapsed=F){
  d <- read_csv(f) %>%
    filter(str_detect(exp,which_exp))
  if(collapsed){
    d <- d %>%
      filter(tdd!=0) %>%
      rename(distance=tdd,disp_cond=display,
             m_prop_2afc=m_pt) %>%
      mutate(distance=as.integer(distance*100),
             disp_cond=factor(disp_cond,levels=c("triangle","horizontal")))
  }else{
    d <- d %>%
      filter(tdd!=0) %>%
      mutate(set=case_when(
        str_detect(tdo,"h")~"a-b-da",
        T~"a-b-db"
      )) %>%
      rename(distance=tdd,disp_cond=display,m_prop_2afc=m_pt) %>%
      mutate(distance=as.integer(distance*100),
             disp_cond=factor(disp_cond,levels=c("triangle","horizontal"))) %>%
      select(-c(tdo))
  }
}

# model selection identified interaction model for 1b, me model for 1a
two_afc_collapsed <- here("2afc","data_means_discrim_TD_collapsed.csv") %>%
  read_two_afc(which_exp = which_exp,collapsed = T)
two_afc_sep <- here("2afc","data_means_discrim_TD.csv") %>%
  read_two_afc(which_exp = which_exp,collapsed = F)

analyze_choice <- function(d, ...){
  choice_data %>%
    group_by(sub_n,...,choice_tdc) %>%
    summarise(n=n()) %>%
    group_by(sub_n,...) %>%
    mutate(prop=n/sum(n)) %>%
    ungroup() %>%
    select(-n) %>%
    # pivot_wider(names_from = choice_tdc, values_from = prop, values_fill = 0) %>%
    # select(-c(c)) %>%
    group_by(choice_tdc,...) %>%
    summarise(m_prop=mean(prop),
              tcrit=qt(.025,n(),lower.tail=F)*sd(prop)/sqrt(n()),
              lwr=m_prop-tcrit,
              upr=m_prop+tcrit) %>%
    ungroup()
}
choice_summary <- analyze_choice(choice_data,disp_cond,set,distance)
choice_summary_collapsed <- analyze_choice(choice_data,disp_cond,distance)

choice_summary %>%
  left_join(two_afc_sep,by = c("disp_cond","distance","set"),relationship = "many-to-many") %>%
  ggplot(aes(m_prop_2afc,m_prop,col=choice_tdc))+
  # geom_point(alpha=.5,size=2.5)+
  geom_line(aes(col=choice_tdc))+
  geom_errorbar(aes(ymin=lwr,ymax=upr),width=.015,col="black")+
  ggsci::scale_color_startrek(name="choice")+
  # scale_x_continuous(breaks=c(2,5,9,14),limits=c(1.5,14.5),labels=c("2%","5%","9%","14%"))+
  scale_y_continuous(limits=c(0,1),breaks=seq(0,1,.2))+
  labs(y="mean choice prop.",x="mean 2afc td discriminability")+
  facet_grid(disp_cond~set)+
  ggthemes::theme_few()+
  theme(text=element_text(size=15),
        plot.caption = element_text(hjust=1),legend.position = "below")
ggsave(filename=here("plots","choicePhase_att_trials_compare_to_2afc.jpeg"),width=5,height=5) 

choice_summary_collapsed %>%
  left_join(two_afc_collapsed,by = c("disp_cond","distance"),relationship = "many-to-many") %>%
  ggplot(aes(m_prop_2afc,m_prop,col=choice_tdc))+
  # geom_point(alpha=.5,size=2.5)+
  geom_line(aes(col=choice_tdc,linetype=disp_cond))+
  geom_errorbar(aes(ymin=lwr,ymax=upr),width=.0015,col="black",alpha=.5)+
  ggsci::scale_color_startrek(name="choice")+
  # geom_errorbarh(alpha=.5,aes(xmin=hdi_lower,xmax=hdi_upper))+
  coord_fixed(xlim=c(.5,1),ylim=c(0,.65))+
  scale_x_continuous(limits = c(.5,1),labels=drop_leading_zero)+
  scale_y_continuous(labels=drop_leading_zero)+
  # facet_grid(disp_cond~.)+
  labs(x="mean binary td discriminability",y="mean trinary choice")+
  ggthemes::theme_few()+
  theme(text=element_text(size=15),
        plot.caption = element_text(hjust=1))
ggsave(filename=here("plots","choicePhase_att_trials_compare_to_2afc_collapsed.jpeg"),width=6,height=4)

# plotting trinary choice against 2afc data
# using model predicted (bayesian logistic regression) values
# used the exp 1b (exp 1 in paper)
rm(list=ls())
library(tidyverse)
library(here)
source(here("analysis/utility_functions.R"))
choice_data <- here("data","choice","aggregated","choice_all.csv") %>%
  read_csv() %>%
  filter(str_detect(effect,"attraction") & rt>=.1 & rt<=10)

# m14 - the winning model
two_afc_collapsed <- here("data","2afc","m14_2afc_preds_v_data.csv") %>%
  read_csv() %>%
  filter(source=="model" & probe=="td") %>%
  rename(disp_cond=display, # to match current choice data
         distance=tdd,
         discrim=p)

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
choice_summary_collapsed <- analyze_choice(choice_data,disp_cond,distance)

choice_summary_collapsed %>%
  left_join(two_afc_collapsed,by = c("disp_cond","distance"),relationship = "many-to-many") %>%
  ggplot(aes(discrim,m_prop))+
  geom_point(aes(col=choice_tdc))+
  geom_line(aes(col=choice_tdc),linewidth=.5,alpha=.9)+
  geom_errorbarh(aes(xmin=hdi_lower,xmax=hdi_upper),height=.0015,alpha=.6)+
  geom_errorbar(aes(ymin=lwr,ymax=upr),width=.0015,alpha=.6)+
  ggsci::scale_color_startrek(name="stimulus")+
  scale_y_continuous(limits=c(0,.6),breaks=seq(0,.6,.2))+
  labs(y="mean choice prop.",x="td 2afc discriminability")+
  facet_grid(disp_cond~.)+
  ggthemes::theme_few()+
  theme(text=element_text(size=18),
        legend.position = "top")
ggsave(filename=here("analysis/plots/choicePhase_att_trials_compare_to_2afc_collapsed.jpeg"),width=5,height=5)

rm(list=ls())
library(tidyverse)
library(here)
library(glue)
library(fs)
library(qs)
library(latex2exp)


# control params
which_model <- "sigma_constant"
outliers_removed <- T
# which_cond <- "horizontal"
# results directory

read_preds <- function(which_cond,outliers_removed=T){
  results_dir <- here("bayes",glue("sigma_constant/{which_cond}/{tmp}",
                                   tmp=ifelse(outliers_removed,"no_outliers","with_outliers")))
  d <- qread(path(results_dir,"sim_choice_preds_clean_collapsed.qs"))
  return(d)
}

p <- map_dfr(c("triangle","horizontal"),read_preds,outliers_removed=outliers_removed) %>%
  mutate(disp_cond=factor(disp_cond,levels=c("triangle","horizontal")))

p_collapsed <- p %>%
  group_by(sub_n,distance,stim) %>%
  summarise(prop=mean(prop)) %>%
  ungroup() %>%
  left_join(distinct(p,sub_n,disp_cond)) %>%
  group_by(disp_cond,distance,stim) %>%
  summarise(m_prop=mean(prop),
            tt=qt(.975,n()-1),
            ci_lower=m_prop-tt*(sd(prop)/sqrt(n())),
            ci_upper=m_prop+tt*(sd(prop)/sqrt(n()))) %>%
  ungroup() %>%
  rename(choice=stim) %>%
  select(-tt)

ggplot(p_collapsed, aes(distance,m_prop))+
  geom_line(aes(col=choice),linewidth=1,alpha=.9)+
  # geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper),width=.2,alpha=.6)+
  ggsci::scale_color_startrek(name="stimulus")+
  scale_x_continuous(breaks=c(2,5,9,14),limits=c(1.5,14.5),labels=c("2%","5%","9%","14%"))+
  scale_y_continuous(limits=c(0,.6),breaks=seq(0,.6,.2))+
  labs(y="mean choice prop.",x="target-decoy distance")+
  facet_grid(.~disp_cond)+
  ggthemes::theme_few()+
  theme(text=element_text(size=28),
      legend.position = "top")
ggsave(filename = here("plots","bayes_sim_choice_preds_no_dists_collapsed.jpeg"),
       width=12,height=6)


p_by_set <- p %>%
  group_by(sub_n,distance,stim,set) %>%
  summarise(prop=mean(prop)) %>%
  ungroup() %>%
  left_join(distinct(p,sub_n,disp_cond)) %>%
  group_by(disp_cond,distance,set,stim) %>%
  summarise(m_prop=mean(prop),
            tt=qt(.975,n()-1),
            ci_lower=m_prop-tt*(sd(prop)/sqrt(n())),
            ci_upper=m_prop+tt*(sd(prop)/sqrt(n()))) %>%
  ungroup() %>%
  rename(choice=stim) %>%
  select(-tt) %>%
  mutate(set = case_match(
    set,
    "a-b-da" ~ "a-b-da (target tall)",
    "a-b-db" ~ "a-b-db (target wide)",
    .default = set))

ggplot(p_by_set, aes(distance,m_prop))+
  geom_line(aes(col=choice),linewidth=1,alpha=.9)+
  # geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper),width=.2,alpha=.6)+
  ggsci::scale_color_startrek(name="stimulus")+
  scale_x_continuous(breaks=c(2,5,9,14),limits=c(1.5,14.5),labels=c("2%","5%","9%","14%"))+
  scale_y_continuous(limits=c(0,.6),breaks=seq(0,.6,.2))+
  labs(y="mean choice prop.",x="targey-decoy distance")+
  facet_grid(set~disp_cond)+
  ggthemes::theme_few()
ggsave(filename = here("plots","bayes_sim_choice_preds_no_dists_by_set.jpeg"),
       width=4,height=4)

p_diffs <- p %>%
  mutate(
    choice=case_when(
      set=="a-b-da" & stim=="target" ~ "a",
      set=="a-b-da" & stim=="competitor" ~ "b",
      set=="a-b-db" & stim=="target" ~ "b",
      set=="a-b-db" & stim=="competitor" ~ "a",
      T~"d"
    ),
    set=str_replace_all(set,"-","_")
  ) %>%
  select(-stim) %>%
  pivot_wider(names_from = set, values_from = prop) %>%
  filter(choice!="d") %>%
  mutate(delta=case_when(
    choice=="a"~a_b_da-a_b_db,
    choice=="b"~a_b_db-a_b_da
  ),
  diff=case_when(
    choice=="a"~TeX("$p(a|[a,b,d_{a}])-p(a|[a,b,d_{b}])$",output = "character"),
    choice=="b"~TeX("$p(b|[a,b,d_{b}])-p(a|[a,b,d_{a}])$",output = "character")
  )) %>%
  select(-contains("a_b"),-choice) 

plot_delta <- function(d,cond){
  d %>%
    filter(disp_cond==cond) %>%
    ggplot(aes(delta))+
    geom_histogram(fill="lightblue",col="black")+
    geom_vline(xintercept=0,linetype="dashed",col="red")+
    facet_grid(distance~diff,labeller =label_parsed)+
    labs(x="value",title=glue("{cond} condition\nModel"))+
    ggthemes::theme_few()+
    theme(text=element_text(size=15))
  ggsave(filename = here("plots",glue("bayes_sim_choice_preds_{cond}_delta_no_dists.jpeg")),
         width=6,height=7)
  
}
walk(c("triangle","horizontal"),plot_delta,d=p_diffs)
p_diffs
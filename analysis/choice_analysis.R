# analysis of choice data 
# setup ============================================================
rm(list=ls())
library(here)
library(tidyverse)
library(fs)
library(latex2exp)
library(ggsci)
library(glue)

source(here("analysis","utility_functions.R"))

# import data ============================================================
subs_keep <- here("analysis","subs_keep","subs_keep.txt") %>%
  read_lines() %>%
  as.numeric()
d <- here("data","choice","aggregated","choice_all.csv") %>%
  read_csv() %>%
  filter(str_detect(effect,"prac",T) & sub_n %in% subs_keep & rt>=.1 & rt<=10) %>% # important - filtering out RTs
  mutate(disp_cond=factor(disp_cond, levels=c("triangle","horizontal")))

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
ggsave(filename = here("analysis","plots","choicePhase_all_trials_pcorrect.jpg"),width=4,height=4)
compute_prop_cor(d1, pl=T,effect, distance)
ggsave(filename = here("analysis","plots","choicePhase_att_trials_pcorrect.jpg"),width=4,height=4)
compute_prop_cor(d1, pl=T,block_n)
ggsave(filename = here("analysis","plots","choicePhase_all_trials_by_block_pcorrect.jpg"),width=4,height=4)
compute_prop_cor(d1, pl=T,disp_cond)
ggsave(filename = here("analysis","plots","choicePhase_all_trials_by_disp_cond.jpg"),width=4,height=4)

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
  filter(str_detect(effect,"attraction"))
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
  ggplot(aes(distance,m))+
  geom_line(aes(col=choice_tdc),linewidth=1,alpha=.9)+
  geom_errorbar(aes(ymin=lwr,ymax=upr),width=.2,alpha=.6)+
  ggsci::scale_color_startrek(name="stimulus")+
  scale_x_continuous(breaks=c(2,5,9,14),limits=c(1.5,14.5),labels=c("2%","5%","9%","14%"))+
  scale_y_continuous(limits=c(0,.6),breaks=seq(0,.6,.2))+
  labs(y="mean choice prop.",x="target-decoy distance")+
  facet_grid(.~disp_cond)+
  ggthemes::theme_few()+
  theme(text=element_text(size=28),
        legend.position = "top")
ggsave(filename = here("analysis","plots","choicePhase_att_trials_mean_choice_props_collapsed.jpg"),width=12,height=6)

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
  # geom_point(aes(col=choice_tdc),pch=4,alpha=.8)+
  geom_line(aes(col=choice_tdc))+
  geom_errorbar(aes(ymin=lwr,ymax=upr),width=.25)+
  ggsci::scale_color_startrek(name="choice")+
  scale_x_continuous(breaks=c(2,5,9,14),limits=c(1.5,14.5),labels=c("2%","5%","9%","14%"))+
  scale_y_continuous(limits=c(0,1),breaks=seq(0,1,.2))+
  labs(y="mean choice prop.",x="target-decoy distance")+
  facet_grid(disp_cond~set)+
  ggthemes::theme_few()+
  theme(text=element_text(size=15),
        legend.position = "bottom")
ggsave(filename = here("analysis","plots","choicePhase_att_trials_mean_choice_props_by_dist.jpg"),width=4,height=4)

att_mean_choice_props <- att_choice_props %>%
  left_join(distinct(d1,sub_n,disp_cond)) %>%
  select(-n) %>%
  group_by(choice_tdc,distance,disp_cond) %>%
  summarise(m=mean(prop),
            tcrit=qt(.025,n(),lower.tail=F)*sd(prop)/sqrt(n()),
            lwr=m-tcrit,
            upr=m+tcrit) %>%
  ungroup() 
save(att_mean_choice_props_by_set,
     att_mean_choice_props,file=here("data","att_mean_choice_props.RData"))

att_choice %>%
  group_by(sub_n, distance, diag, choice_tdc) %>%
  summarise(N=n()) %>%
  group_by(sub_n,distance,diag) %>%
  mutate(prop=N/sum(N)) %>%
  ungroup() %>%
  left_join(distinct(d, sub_n, disp_cond)) %>%
  group_by(distance,diag, disp_cond, choice_tdc) %>%
  summarise(m=mean(prop)) %>%
  ungroup() %>%
  ggplot(aes(distance,m,col=choice_tdc,shape=as.factor(diag)))+
  geom_point()+
  geom_path()+
  facet_grid(.~disp_cond)+
  ggthemes::theme_few()

# differences ================================================================================================



att_diffs <- att_choice_props_by_set %>%
  select(-n) %>%
  mutate(choice=case_when(
    set=="h" & choice_tdc=="target" ~ "h",
    set=="h" & choice_tdc=="competitor" ~ "w",
    set=="w" & choice_tdc=="target" ~ "w",
    set=="w" & choice_tdc=="competitor" ~ "h",
    T~"d"
  ),set=str_replace_all(set,"-","_")
  ) %>%
  filter(choice!="d") %>%
  select(-choice_tdc) %>%
  pivot_wider(names_from = set, values_from = prop, values_fill = 0) %>%
  mutate(delta=case_when(
    choice=="h"~h-w,
    choice=="w"~w-h
  )) %>%
  left_join(distinct(d,sub_n,disp_cond))

plot_delta <- function(d,cond){
  d %>%
    mutate(diff=case_when(
      choice=="h"~TeX("$p(h|[h,w,d_{h}])-p(h|[h,w,d_{w}])$",output = "character"),
      choice=="w"~TeX("$p(w|[h,w,d_{w}])-p(w|[h,w,d_{h}])$",output = "character")
    )) %>%
    select(-choice) %>%
    filter(disp_cond==cond) %>%
    ggplot(aes(delta))+
    geom_histogram(fill="lightblue",col="black",binwidth = .03)+
    geom_vline(xintercept=0,linetype="dashed",col="red")+
    facet_grid(distance~diff,labeller =label_parsed)+
    labs(x="value",title=glue("{cond} condition\nData"))+
    ggthemes::theme_few()+
    theme(text=element_text(size=15))
  ggsave(filename = here("analysis","plots",glue("choicePhase_data_{cond}_delta_hists.jpeg")),
         width=6,height=7)
  
}
walk(c("triangle","horizontal"),plot_delta,d=att_diffs)
att_mean_diffs <- att_diffs %>%
  mutate(diff=case_when(
    choice=="h"~"p(h|[h,w,dh]) - p(h|[h,w,dw])",
    choice=="w"~"p(w|[h,w,dw]) - p(w|[h,w,dh])"
  )) %>%
  select(-choice) %>%
  group_by(disp_cond,distance,diff) %>%
  summarise(m=mean(delta),
            ci_lower=m-qt(.975,n()-1)* ( sd(delta)/sqrt(n()) ),
            ci_upper=m+qt(.975,n()-1)* ( sd(delta)/sqrt(n()) )) %>%
  ungroup()
att_mean_diffs %>%
  mutate(disp_cond=factor(disp_cond,levels=c("triangle","horizontal"))) %>%
  ggplot(aes(distance,m,fill=diff))+
  geom_col(position="dodge",width=1)+
  geom_hline(yintercept=0,linetype="dashed",alpha=.7)+
  geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper),
                position = position_dodge(1),
                width=0.08)+
  ggsci::scale_fill_tron(name="")+
  scale_x_continuous(breaks=c(2,5,9,14),limits=c(1.5,14.5),labels=c("2%","5%","9%","14%"))+
  labs(x="target-decoy distance",y="mean diff")+
  facet_grid(disp_cond~.)+
  ggthemes::theme_few()
ggsave(filename = here("analysis","plots",glue("choicePhase_delta_means.jpeg")),
       width=6,height=7)

# STATS ON RST FOR DISSERTATION ===============================================================
compute_ast <- function(dat, cond){
  dat %>%
    filter(disp_cond==cond & effect=="attraction") %>%
    group_by(sub_n,distance,set,choice_tdc) %>%
    summarise(N=n()) %>%
    ungroup() %>%
    mutate(choice_tdc=str_sub(choice_tdc,1,1)) %>%
    pivot_wider(names_from = c(set,choice_tdc),
                values_from = N, names_sep = "_",
                values_fill = 0) %>%
    mutate(ast= .5*( ( (h_t)/(h_t+h_c+h_d) ) + ( (w_t)/(w_t+w_c+w_d) ) ) ) %>%
    select(sub_n,distance,ast) %>%
    mutate(disp_cond=cond) %>%
    relocate(disp_cond,.after=sub_n)
}
ast <- map(list("triangle","horizontal"),compute_ast,dat=d) %>%
  list_rbind()

cousineau_correction <- function(data,cond,return_means){
  dd <- filter(data,disp_cond==cond)
  N <- length(unique(dd$sub_n))
  M <- 4 # n distance
  corr_factor <- M/(M-1)
  X <- dd %>%
    group_by(distance) %>%
    mutate(ast_dist_m=mean(ast)) %>%
    ungroup() %>%
    mutate(ast_norm=sqrt(corr_factor)*(ast-ast_dist_m)+ast_dist_m)
  if(return_means){
    X <- X %>%
      group_by(distance) %>%
      summarise(ast_norm_m=mean(ast_norm),
                se=sd(ast)/sqrt(n()),
                ci_lower=ast_norm_m-qt(.975,n()-1)*se,
                ci_upper=ast_norm_m+qt(.975,n()-1)*se) %>%
      ungroup() %>%
      mutate(disp_cond=cond)
  }else{
    X <- X %>% 
      mutate(disp_cond=cond)
  }
  return(X)
}

m_ast <- map(list("horizontal","triangle"),~cousineau_correction(ast,.x,return_means=T)) %>%
  list_rbind()
m_ast %>%
  mutate(disp_cond=factor(disp_cond,levels=c("triangle","horizontal"))) %>%
  ggplot(aes(distance,ast_norm_m))+
  geom_path()+
  geom_hline(yintercept=.5,linetype="dashed",alpha=.5)+
  geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper),width=.2)+
  scale_x_continuous(breaks=c(2,5,9,14),limits=c(1.5,14.5),labels=c("2%","5%","9%","14%"))+
  scale_y_continuous(n.breaks = 5,limits=c(.3,.6))+
  facet_grid(.~disp_cond)+
  labs(x="TDD",y="mean AST")+
  ggthemes::theme_few()+
  theme(text=element_text(size=15))
ggsave(filename = here("analysis","plots","choicePhase_mean_ast.jpeg"),width=5,height=4)
ast_corr <- map(list("horizontal","triangle"),~cousineau_correction(ast,.x,return_means=F)) %>%
  list_rbind() %>%
  mutate(distance=as.factor(distance),
         disp_cond=as.factor(disp_cond))
do_t <- function(dat, dist){
  dat1 <- filter(dat,distance==dist)
  X <- mean(dat1$ast_norm)-.5
  N <- length(dat1$ast_norm)
  SE <- sd(dat1$ast_norm)/sqrt(N)
  t <- X/SE
  # browser()
  p <- pt(abs(t), N-1,lower.tail = F)*2 # both sides correction 
  d <- tibble(
    distance=dist,
    disp_cond=unique(dat1$disp_cond),
    p=p
  )
  return(d)
}
ast_anova_horiz <- aov(ast_norm~distance,data=filter(ast_corr,disp_cond=="horizontal"))
summary(ast_anova_horiz)


ast_anova_tri <- aov(ast_norm~distance,data=filter(ast_corr,disp_cond=="triangle"))
summary(ast_anova_tri)
all_t <- bind_rows(map(c(2,5,9,14),~do_t(filter(ast_corr,disp_cond=="horizontal"),dist=.x)) %>% list_rbind(),
         map(c(2,5,9,14),~do_t(filter(ast_corr,disp_cond=="triangle"),dist=.x)) %>% list_rbind())
all_t[which(all_t$p<(.05/8)),]  

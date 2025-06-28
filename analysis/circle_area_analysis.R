# setup ===========================================================================
rm(list=ls())
library(here)
library(tidyverse)
library(fs)
library(latex2exp)
library(glue)
library(ggsci)
library(patchwork)

# a function needed to get attraction trial specifications
source(here("analysis/utility_functions.R"))

# whether not to use MY (sean's) data when I ran myself through the experiment for testing purposess
sean_data <- F

# minimum radius
min_rad <- 5
min_area <- pi*min_rad^2
min_log_area <- log(min_area)

# rsq threshold
rsqt <- .05 # quantile threshold to remove ppts by

# threshold to remove outlier trials
z_thresh <- 3.5 
# import data ================================================================================

# all data
if(sean_data){
  d <- here("sean_test_data","circle_area","aggregated","sean_circle_area_clean.csv") %>%
    read_csv()
}else{
  d <- here("data","circle_area","aggregated","circle_area_all.csv") %>%
    read_csv() %>%
    mutate(disp_cond=factor(disp_cond,levels=c("triangle","horizontal")))
}


# practice trials
prac <- d %>%
  filter(str_detect(effect,"practice"))

# calibration trials
calib <- d %>%
  filter(str_detect(effect,"calibration"))

# filler trials
fill <- d %>%
  filter(str_detect(effect,"filler"))

# catch trials
catch <- d %>%
  filter(str_detect(effect,"catch"))

# all experimental trials
d_exp <- d %>%
  filter(str_detect(effect,"practice|calibration",negate=T))

# quick count of number of subjects per condition
d %>%
  distinct(sub_n,disp_cond) %>%
  group_by(disp_cond) %>%
  summarise(n=n())

# general analysis ======================================================================
d_exp %>%
  pivot_longer(c(circle1_area,circle2_area,circle3_area)) %>%
  ggplot(aes(value))+
  geom_histogram(fill="lightblue",col="black")+
  labs(x="area (px)")+
  facet_wrap(vars(disp_cond))+
  ggthemes::theme_few()+
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 90))
if(!sean_data){
  ggsave(filename = here("analysis","plots","circleAreaPhase_area_all_hist_with_outliers.jpg"),width=5,height=4)
}

q99 <- quantile(d_exp$rt, probs=.99)

d_exp %>%
  filter(rt<=q99) %>%
  ggplot(aes(rt))+
  geom_histogram(fill="lightblue",col="black")+
  scale_x_continuous(limits=c(0,40),breaks = seq(0,40,5))+
  facet_wrap(vars(disp_cond))+
  labs(x="rt (secs)",caption=glue("Note: RTs>{round(q99,digits=2)} (the 99th percentile) were removed here for \nvisualization purposes."))+
  ggthemes::theme_few()+
  theme(plot.caption = element_text(hjust=0),
        text=element_text(size=14))
if(!sean_data){
  ggsave(filename = here("analysis","plots","circleAreaPhase_rt_all_hist_with_outliers.jpg"),width=5,height=4)
}

# LOG EVERYTHING ====================================================================
d_exp_log <- d_exp %>%
  mutate(across(c(contains("rect"),contains("circle")),log))

# Remove trials where no adjustments were made ==========================================================
# calling them "null" trials
drop_null <- function(data){
  data1 <- data %>%
    rowwise() %>%
    mutate(drop=any(c(circle1_area,circle2_area,circle3_area)==min_log_area)) %>%
    ungroup()
  cat("dropping",sum(data1$drop),"null trials","\n",sep=" ") 
  data2 <- data1 %>%
    filter(!drop) %>%
    select(-drop)
  return(data2)
}

d_exp_log_null_trials_removed <- d_exp_log %>%
  drop_null()

# Remove outlier participants ================================================================================
# Remove all participants with lowest 5% of rsqs 
# only use filler trials
do_regress <- function(data){
  sub_n <- unique(data$sub_n)
  cat("\n==========\n",sub_n,"\n==========\n")
  m <- lm(circle~rect,data=data)
  d <- tibble(sub_n=sub_n,
              rsq=summary(m)$r.squared,
              int=m$coefficients[1],
              slope=m$coefficients[2],
              disp_cond=unique(data$disp_cond))
  return(d)
}

d_fill_log_diff <- d_exp_log_null_trials_removed %>%
  filter(effect=="filler") %>%
  select(-contains("rad")) %>%
  pivot_longer(c(contains("circle"),contains("rect"))) %>%
  mutate(name=str_remove(name,"_area"),
         stim=str_extract(name,"[:digit:]"),
         type=str_extract(name,"[:alpha:]{1,}")) %>%
  select(-name) %>%
  pivot_wider(names_from = type,
              values_from = value)
sub_regress <- map_dfr(split(d_fill_log_diff, d_fill_log_diff$sub_n),do_regress)
sub_regress %>%
  reframe(across(c(int,slope,rsq),~quantile(.x, probs=seq(0,1,.1)))) %>%
  mutate(q=seq(0,1,.1)) %>%
  relocate(q,.before = everything())
ggplot(sub_regress,aes(rsq))+
  geom_histogram(fill="lightblue",col="black")+
  labs(x=TeX("$R^2$"))+
  scale_x_continuous(limits=c(0,1))+
  ggthemes::theme_few()
if(!sean_data){
  ggsave(filename = here("analysis","plots","circleAreaPhase_rsq_hist_with_outliers.jpeg"),width=4,height=4)
}


ggplot(sub_regress,aes(slope))+
  geom_histogram(fill="lightblue",col="black")+
  labs(x=TeX("$\\beta_{log(true\\,area)}$"))+
  # scale_x_continuous(limits=c(0,1))+
  ggthemes::theme_few()
if(!sean_data){
  ggsave(filename = here("analysis","plots","circleAreaPhase_slope_hist_with_outliers.jpeg"),width=4,height=4)
}


ggplot(sub_regress,aes(int))+
  geom_histogram(fill="lightblue",col="black")+
  labs(x=TeX("$\\beta_{0}$"))+
  scale_x_continuous(limits=c(0,1))+
  ggthemes::theme_few()
if(!sean_data){
  ggsave(filename = here("analysis","plots","circleAreaPhase_int_hist_with_outliers.jpeg"),width=4,height=4)
}

find_thresh <- function(data, p){
  th <- unname(quantile(data$rsq,probs = p))
  cat("\nR^2 threshold =",th,"\n")
  return(th)
} 

thresh <- ifelse(sean_data, 0, find_thresh(sub_regress,rsqt))

remove_subs_by_rsq <- function(dd, regress){
  dd1 <- dd %>%
    left_join(select(regress,sub_n,rsq),by="sub_n") %>%
    filter(rsq > thresh)
  cat("\nRemoved",
      length(unique(dd$sub_n)) - length(unique(dd1$sub_n)),
      "subjects","\n",sep=" ")
  return(dd1)
}

d_exp_log_subs_removed <- d_exp_log_null_trials_removed %>%
  remove_subs_by_rsq(regress=sub_regress)

# analyze rsquareds for remaining subjects ==========================================================================================
d_exp_log_subs_removed %>%
  distinct(sub_n,disp_cond,rsq) %>%
  summarise(m=mean(rsq),
            s=sd(rsq))
           

# count of subjects by condition after removing =============================================
d_exp_log_subs_removed %>%
  distinct(disp_cond,sub_n) %>%
  group_by(disp_cond) %>%
  summarise(n=n())

# write text file with usable subject numbers for choice analysis =============================================
write_lines(unique(d_exp_log_subs_removed$sub_n),
           file=here("analysis","subs_keep","subs_keep.txt"))

# finding outlier trials from participants we're keeping in data, attraction trials =================================================================================
d_att_log_subs_removed <- d_exp_log_subs_removed %>%
  get_att_specs(data_shape = "wide")  # this function also removes non-attraction trials

zscore <- function(d){
  d %>%
    select(sub_n,computer_n,effect,set,distance,diag,block_n,trial_n,t,d,c) %>%
    pivot_longer(c(t,d,c),names_to = "stim",values_to = "a") %>%
    group_by(sub_n,diag) %>%
    mutate(za=(a-mean(a))/sd(a)) %>%
    ungroup() %>%
    select(-a) %>%
    pivot_wider(names_from = stim,
                values_from = za) %>%
    left_join(distinct(d, sub_n,disp_cond)) %>%
    relocate(disp_cond,.after=sub_n)
}

find_outliers <- function(d, d_ref, z_thresh){
  # d2 <- d1 %>%
  #   select(sub_n,set,distance,diag,block_n,trial_n,t,d,c) %>%
  #   pivot_longer(c(t,d,c),names_to = "stim",values_to = "a") %>%
  #   group_by(sub_n,diag) %>%
  #   mutate(za=(a-mean(a))/sd(a)) %>%
  #   ungroup() %>%
  #   select(-a) %>%
  #   pivot_wider(names_from = stim,
  #               values_from = za) %>%
  #   left_join(distinct(d, sub_n,disp_cond)) %>%
  #   relocate(disp_cond,.after=sub_n)
  # cv <- cov(d2[,c("t","c","d")])
  # m <- c(mean(d2$t),mean(d2$c),mean(d2$d))
  # 
  # d2$dens <- pmap_dbl(list(d2$t,d2$c,d2$d),function(t,c,d) dmvnorm(x=c(t,c,d),mean=m,sigma=cv))
  # d3 <- d2 %>%
  #   mutate(q=ecdf(d)(d),
  #          drop=q<=.025) 
  # print(glue("{sum(d3$drop)}/{nrow(d3)} trials were dropped."))
  # d4 <- d3 %>%
  #   filter(!drop) %>%
  #   select(-c(t,d,c,drop,dens,q))
  # browser()
  dd <- d %>%
    rowwise() %>%
    mutate(drop_trial=any( c(abs(t)>z_thresh,
                       abs(d)>z_thresh,
                       abs(c)>z_thresh))) %>%
    ungroup()
  # browser()
  # return(dd %>%
  #   group_by(sub_n) %>%
  #   summarise(n_drop=sum(drop)))
  # print(glue("{sum(dd$drop)}/{nrow(dd)} trials were dropped."))
  # ddd <- filter(dd, !drop) %>%
  #   pivot_wider(names_from = name,values_from = z) %>%
  #   select(-drop)
  return(
    dd 
  )
}
d_att_log_subs_removed_z <- zscore(d_att_log_subs_removed)

d_att_log_subs_removed_z %>%
  pivot_longer(c(t,d,c)) %>%
  ggplot(aes(value))+
  geom_histogram()+
  facet_grid(name~.)+
  ggthemes::theme_few()

d_att_log_subs_removed_find_outliers_z <- d_att_log_subs_removed_z %>%
  find_outliers(d_att_log_subs_removed, z_thresh)

# number of outliers
d_att_log_subs_removed_find_outliers_z %>%
  group_by(sub_n) %>%
  summarise(N_drop=sum(drop_trial)) %>%
  arrange(desc(N_drop)) %>%
  group_by(N_drop) %>% 
  summarise(n=n())
d_att_log_subs_removed_find_outliers_z %>%
  group_by(sub_n) %>%
  summarise(N_drop=sum(drop_trial)) %>%
  ungroup() %>%
  summarise(m=median(N_drop))

sum(d_att_log_subs_removed_find_outliers_z$drop_trial)

d_att_log_subs_removed_find_outliers <- d_att_log_subs_removed_find_outliers_z %>%
  select(-c(t,d,c)) %>%
  right_join(d_att_log_subs_removed, by=c("sub_n","computer_n","set","effect","trial_n","block_n","diag","distance","disp_cond"))

# function to plot correlations =======================================================
plot_cors <- function(d, x, y, filename, save=T, lims=NULL, droparg=NULL){
  d <- d %>%
    mutate(distance=as.character(distance),
           distance=factor(str_glue("{distance}% TDD"),
                           levels=c("2% TDD","5% TDD","9% TDD","14% TDD")))
  # browser()
  if(is.null(droparg)){
    p <- d %>%
      ggplot(aes({{x}},{{y}}))+
      geom_point(shape=".",alpha=.4)+
      geom_abline(slope=1,intercept=0,linetype="dashed",alpha=.5)+
      facet_grid(distance~disp_cond)+
      ggthemes::theme_few()
  }else{
    # colarg <- enquo(colarg)
    p <- d %>%
      ggplot(aes({{x}},{{y}},shape=!!sym(droparg),col=!!sym(droparg)))+
      geom_point(alpha=.4)+
      geom_abline(slope=1,intercept=0,linetype="dashed",alpha=.5)+
      scale_shape_manual(values=c(4,1))+
      scale_color_manual(values=c("grey","red"))+
      facet_grid(distance~disp_cond)+
      ggthemes::theme_few()
  }
  if(!is.null(lims)){
    p <- p + coord_fixed(xlim=lims, ylim=lims)
  }else{
    p <- p + coord_fixed()
  }
  if(save) ggsave(p, filename=filename,width=4,height=6)
  return(p)
}

# plotting scatterplots with outliers included =======================================================
if(!sean_data){
  d_att_log_subs_removed_find_outliers %>%
    plot_cors(t,c,filename=here("analysis","plots","circleAreaPhase_cor_plot_tc_with_outliers.jpg"),save=!sean_data,lims=c(7,11),droparg="drop_trial")
  d_att_log_subs_removed_find_outliers %>%
    plot_cors(t,d,filename=here("analysis","plots","circleAreaPhase_cor_plot_td_with_outliers.jpg"),save=!sean_data,lims=c(7,11),droparg="drop_trial")
  d_att_log_subs_removed_find_outliers %>%
    plot_cors(c,d,filename=here("analysis","plots","circleAreaPhase_cor_plot_cd_with_outliers.jpg"),save=!sean_data,lims=c(7,11),droparg="drop_trial")
}

# ACTUALLY remove the outlier trials ==============================================================================
# number of trials dropped per subjects
d_att_log_subs_removed_find_outliers %>%
  group_by(sub_n) %>%
  summarise(n_drop=sum(drop_trial)) %>%
  group_by(n_drop) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  rename(n_trials_dropped=n_drop,
         n_subjects=n) %>%
  relocate(n_subjects,.before = everything())
d_att_log_cleaned <- d_att_log_subs_removed_find_outliers %>%
  filter(!drop_trial) %>%
  select(-drop_trial)

# number of trials after removing outlier trials ====================================
d_att_log_cleaned %>%
  group_by(disp_cond) %>%
  summarise(n=n())

# plotting scatterplots with outliers removed ============================================================
if(!sean_data){
  p_tc <- d_att_log_cleaned %>%
    plot_cors(t,c,filename=here("analysis","plots","circleAreaPhase_cor_plot_tc_no_outliers.jpg"),save=!sean_data,lims=c(7,11))+
    labs(x="estimated target area (log)",y="estimated competitor area (log)",
         title="target-competitor trials")
  p_td <- d_att_log_cleaned %>%
    plot_cors(t,d,filename=here("analysis","plots","circleAreaPhase_cor_plot_td_no_outliers.jpg"),save=!sean_data,lims=c(7,11))+
    labs(x="estimated target log area (log)",y="estimated decoy area (log)",
         title="target-decoy trials")
  p_cd <- d_att_log_cleaned %>%
    plot_cors(c,d,filename=here("analysis","plots","circleAreaPhase_cor_plot_cd_no_outliers.jpg"),save=!sean_data,lims=c(7,11))+
    labs(x="estimated competitor area (log)",y="estimated decoy area (log)",
         title="competitor-decoy trials")
  p_all <- (p_td | p_cd | p_tc)
  p_all
  ggsave(filename=here("analysis","plots","circleAreaPhase_cor_plot_all_no_outliers.jpg"),
         width=8,height=5)
}


get_cors <- function(d,...){
  # compute correlations 
  # first removing subject level effects by computing subject - diagonal z scores
  cc <- d %>% 
    pivot_longer(c(t,d,c),names_to = "stim", values_to = "ca") %>%
    group_by(sub_n,diag) %>%
    mutate(s=sd(ca),
           m=mean(ca),
           caz=(ca-m)/s) %>%
    select(-ca) %>%
    pivot_wider(names_from=stim, values_from=caz) %>%
    group_by(...) %>%
    summarise(r_td=round(cor(t,d),digits=3),
              r_tc=round(cor(t,c),digits=3),
              r_cd=round(cor(c,d),digits=3)) %>%
    ungroup()
  print(cc)
  return(cc)
}

if(!sean_data){
  get_cors(d_att_log_cleaned, disp_cond) %>%
    write_excel_csv(file=here("cors","cors_collapsed_no_outliers.csv"))
  get_cors(d_att_log_cleaned, disp_cond, distance)%>%
    write_excel_csv(file=here("cors","cors_by_dist_no_outliers.csv"))
  sub_level_cors <- get_cors(d_att_log_cleaned, sub_n) %>%
    left_join(distinct(d_att_log_cleaned,sub_n,disp_cond))
  
  agg_cors <- get_cors(d_att_log_cleaned, disp_cond)
  sub_level_cors %>%
    pivot_longer(c(r_td,r_tc,r_cd),names_to = "pair",values_to = "cor") %>%
    left_join(pivot_longer(agg_cors,c(r_td,r_tc,r_cd),
                           names_to = "pair",
                           values_to = "agg_cor"), 
              by=c("pair","disp_cond"),
              relationship = "many-to-one") %>%
    mutate(pair=case_when(
      pair=="r_td"~TeX("$r_{td}$",output = "character"),
      pair=="r_tc"~TeX("$r_{tc}$",output = "character"),
      pair=="r_cd"~TeX("$r_{cd}$",output = "character"),
    )) %>%
    ggplot(aes(cor))+
    geom_histogram(fill="lightblue",col="black")+
    facet_grid(pair~disp_cond,labeller=label_parsed)+
    geom_vline(aes(xintercept=agg_cor),linetype="dashed",col="red")+
    labs(caption="Dashed red lines are aggregate correlations.")+
    ggthemes::theme_few()+
    theme(text=element_text(size=14),
          plot.caption = element_text(hjust=0))
  ggsave(filename = here("analysis","plots","circleAreaPhase_td_tc_cd_indiv_cors_log_area_no_outliers.jpeg"),
         width = 5, height=5)
}

# means ============================================================
mean_pars_by_set <- d_att_log_cleaned %>% 
  pivot_longer(c(t,d,c),names_to = "stim",
               values_to = "ca") %>%
  mutate(stim_name=str_replace_all(stim,c("^c$"="competitor","^t$"="target","^d$"="decoy"))) %>%
  group_by(disp_cond,distance,set,stim_name) %>%
  summarise(m_ca=mean(ca),
            n=n(),
            s=sd(ca),
            se=s/sqrt(n),
            se_lower=m_ca-se,
            se_upper=m_ca+se)  %>%
  ungroup()
mean_pars_collapsed <- d_att_log_cleaned %>% 
  pivot_longer(c(t,d,c),names_to = "stim",
               values_to = "ca") %>%
  mutate(stim_name=str_replace_all(stim,c("^c$"="competitor","^t$"="target","^d$"="decoy"))) %>%
  group_by(disp_cond,distance,stim_name) %>%
  summarise(m_ca=mean(ca),
            n=n(),
            s=sd(ca),
            se=s/sqrt(n),
            se_lower=m_ca-se,
            se_upper=m_ca+se)  %>%
  ungroup()
mean_pars_by_set %>%
  # mutate(distance=as.factor(distance)) %>%
  ggplot(aes(distance, m_ca, col=stim_name))+
  geom_point()+
  geom_line()+
  ggsci::scale_color_startrek(name="stimulus")+
  scale_x_continuous(breaks=c(2,5,9,14),limits=c(1.5,14.5),labels=c("2%","5%","9%","14%"))+
  # scale_y_continuous(limits=c(0,1),breaks=seq(0,1,.2))+
  geom_errorbar(aes(ymin=se_lower,ymax=se_upper),width=.25)+
  # scale_color_manual(values=ggsci::pal_startrek()(3), labels=c("a","b","d"),name="rectangle")+
  labs(x="target-decoy distance",y="mean judged log area (px)")+
  lemon::facet_rep_grid(disp_cond~set, scales = "free_x", repeat.tick.labels = "bottom")+
  ggthemes::theme_few()+
  theme(legend.position = "bottom")
if(!sean_data){
  ggsave(filename = here("analysis","plots","circleAreaPhase_mean_ca_by_set_distance_log_no_outliers.jpg"),width=5,height=4)
}

mean_pars_collapsed %>%
  ggplot(aes(distance, m_ca, col=stim_name))+
  geom_line(aes(col=stim_name),linewidth=1,alpha=.9)+
  geom_errorbar(aes(ymin=se_lower,ymax=se_upper),width=.25,col="black")+
  ggsci::scale_color_startrek(name="stimulus")+
  scale_x_continuous(breaks=c(2,5,9,14),limits=c(1.5,14.5),labels=c("2%","5%","9%","14%"))+
  scale_y_continuous(limits=c(9.46,9.61),breaks=round(seq(9.46,9.61,length.out=4),
                                                      digits=2))+
  labs(x="target-decoy distance",y="mean judged log area (px)")+
  lemon::facet_rep_grid(.~disp_cond, scales = "free_x", repeat.tick.labels = "bottom")+
  ggthemes::theme_few() +
  theme(legend.position = "bottom",
        text=element_text(size=28))
if(!sean_data){
  ggsave(filename = here("analysis","plots","circleAreaPhase_mean_ca_by_distance_log_no_outliers.jpg"),width=12,height=6)
}

mean_pars_collapsed_rem_subs <- d_att_log_cleaned %>% 
  pivot_longer(c(t,d,c),names_to = "stim",
               values_to = "ca") %>%
  mutate(stim_name=str_replace_all(stim,c("^c$"="competitor","^t$"="target","^d$"="decoy"))) %>%
  group_by(sub_n,diag) %>%
  mutate(m=mean(ca)) %>%
  ungroup() %>%
  mutate(ca=ca-m) %>%
  group_by(disp_cond,distance,stim_name) %>%
  summarise(m_ca=mean(ca),
            n=n(),
            s=sd(ca),
            se=s/sqrt(n),
            se_lower=m_ca-se,
            se_upper=m_ca+se)  %>%
  ungroup()

mean_pars_collapsed_rem_subs %>%
  ggplot(aes(distance, m_ca, col=stim_name))+
  geom_line(aes(col=stim_name),linewidth=1,alpha=.9)+
  geom_errorbar(aes(ymin=se_lower,ymax=se_upper),width=.25,col="black")+
  ggsci::scale_color_startrek(name="stimulus")+
  scale_x_continuous(breaks=c(2,5,9,14),limits=c(1.5,14.5),labels=c("2%","5%","9%","14%"))+
  # scale_y_continuous(limits=c(9.46,9.61),breaks=round(seq(9.46,9.61,length.out=4),
  #                                                     digits=2))+
  labs(x="target-decoy distance",y="mean judged log area (px)")+
  lemon::facet_rep_grid(.~disp_cond, scales = "free_x", repeat.tick.labels = "bottom")+
  ggthemes::theme_few() +
  theme(legend.position = "bottom",
        text=element_text(size=28))
if(!sean_data){
  ggsave(filename = here("analysis","plots","circleAreaPhase_mean_ca_by_distance_log_no_outliers.jpg"),width=12,height=6)
}

# diff analysis =================================================================================
d_att_log_cleaned_diff <- d_att_log_cleaned %>%
  mutate(
    t_rect=case_when(
      t_identity==1~log(h1*w1),
      t_identity==2~log(h2*w2),
      t_identity==3~log(h3*w3),
    ),
    c_rect=case_when(
      c_identity==1~log(h1*w1),
      c_identity==2~log(h2*w2),
      c_identity==3~log(h3*w3),
    ),
    d_rect=case_when(
      d_identity==1~log(h1*w1),
      d_identity==2~log(h2*w2),
      d_identity==3~log(h3*w3),
    ),
  ) %>%
  mutate(`target - decoy_true`=t_rect-d_rect,
         `competitor - decoy_true`=c_rect-d_rect,
         `target - competitor_true`=t_rect-c_rect,
         `target - decoy_est`=t-d,
         `competitor - decoy_est`=c-d,
         `target - competitor_est`=t-c) %>%
  select(-c(t_rect,d_rect,c_rect,
            t,c,d)) %>%
  pivot_longer(c(contains("true"),contains("est")), values_to = "diff") %>%
  separate(name,into=c("pair","type"),sep="_") %>%
  pivot_wider(names_from = type,values_from = diff)

d_att_log_cleaned_diff %>%
  group_by(sub_n,pair,true,distance) %>%
  summarise(m_est=mean(est)) %>%
  ungroup() %>%
  left_join(distinct(d_att_log_cleaned_diff,sub_n,disp_cond)) %>%
  mutate(true=round(true,digits=3),
         distance=case_when(
           true==0~"0%",
           T~str_glue("{distance}%")),
         distance=factor(distance,levels=c("0%", "2%","5%","9%","14%")),
         pair=factor(pair,levels=c("target - decoy",
                                   "competitor - decoy",
                                   "target - competitor"))) %>%
  ggplot(aes(as.factor(true), m_est, fill=distance))+
  geom_hline(yintercept=0,linetype="dashed",alpha=.5)+
  geom_boxplot(alpha=.5,outlier.alpha = .6 ,outlier.shape = ".")+
  scale_y_continuous(limits=c(-.25,.4))+
  facet_wrap(vars(pair),nrow = 3)+
  ggsci::scale_fill_simpsons(name="TDD")+
  labs(x="actual difference (log area)",y="mean estimated difference (log area)")+
  ggthemes::theme_few()+
  theme(axis.text.x = element_text(angle=90),
        text=element_text(size=13))
if(!sean_data){
  ggsave(filename = here("analysis","plots","circleAreaPhase_boxplot_meanlogdiffs_no_outliers.jpeg"),
         width=6,height=5)
}

# SAVE LOG TRANSFORMED, OUTLIER-REMOVED DATA ==================================================================
d_att_log_cleaned_for_modeling <- d_att_log_cleaned %>% 
  select(-c(effect,h1,h2,h3,w1,w2,w3)) %>% 
  relocate(rt,.after=everything()) %>%
  relocate(c(block_n,trial_n),.after=disp_cond) 
if(!sean_data){
  d_att_log_cleaned_for_modeling %>%
    write_csv(here("data","circle_area","aggregated","circle_area_logtransformed_cleaned_no_outliers.csv"))
  d_att_log_cleaned_for_modeling %>%
    select(-c(contains("identity"),rt,computer_n)) %>%
    mutate(across(c(t,c,d),exp)) %>%
    write_csv(here("data","circle_area","aggregated","circle_area_raw_cleaned_no_outliers_for_andrew.csv"))
}else{
  d_att_log_cleaned_for_modeling %>%
    write_csv(here("sean_test_data","circle_area","aggregated","sean_circle_area_logtransformed_cleaned_for_modeling.csv"))
}

# SAVE LOG TRANSFORMED, DATA with NO OUTLIERS REMOVED ==================================================================
# to do analysis both ways
d_att_everything <- d_exp_log %>%
  filter(effect=="attraction") %>% 
  select(-c(effect,h1,h2,h3,w1,w2,w3)) %>% 
  relocate(rt,.after=everything()) %>%
  relocate(c(block_n,trial_n),.after=disp_cond) 

if(!sean_data){
  d_att_everything %>%
    write_csv(here("data","circle_area","aggregated","circle_area_logtransformed_cleaned_with_outliers.csv"))
}


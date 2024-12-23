# setup ===========================================================================
rm(list=ls())

library(here)
library(tidyverse)
library(fs)
library(glue)
library(mvtnorm)
library(ggsci)
library(patchwork)
# library(rstanarm)
# library(bayesplot)

# a function needed to get attraction trial specifications
source(here("utility_functions.R"))

sean_data <- F

# all data
if(sean_data){
  d <- here("sean_test_data","circle_area","aggregated","sean_circle_area_clean.csv") %>%
    read_csv()
}else{
  d <- here("data","circle_area","aggregated","circle_area_all.csv") %>%
    read_csv()
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
  ggsave(filename = here("plots","area_all_hist.jpg"),width=5,height=4)
  
}

d_exp %>%
  pivot_longer(c(circle1_area,circle2_area,circle3_area)) %>%
  ggplot(aes(value))+
  geom_histogram(fill="lightblue",col="black")+
  labs(x="area (px)")+
  facet_grid(effect~disp_cond, scales="free_y")+
  ggthemes::theme_few()+
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 90))
if(!sean_data){
  ggsave(filename = here("plots","area_all_by_effect_hist.jpg"),width=5,height=4)
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
  ggsave(filename = here("plots","rt_all_hist.jpg"),width=5,height=4)
}

d_exp %>%
  select(-c(h1,w1,h2,w2,h3,w3),-contains("_rad")) %>%
  pivot_longer(contains("_area")) %>%
  mutate(name=str_remove(name,"_area")) %>%
  separate(name, into = c("shape","position"),sep="(?<=[A-Za-z])(?=[0-9])") %>%
  pivot_wider(names_from = shape, values_from = value) %>%
  ggplot(aes(rect,circle,col=effect))+
  geom_point(alpha=.25)+
  coord_fixed(xlim=c(0,66000),ylim=c(0,66000))+
  facet_wrap(vars(disp_cond))+
  ggthemes::theme_few()
if(!sean_data){
  ggsave(filename = here("plots","area_all_scatterplot.jpg"),width=5,height=4)
}

# attraction trials =================================================================================
d_exp_att <- d_exp %>%
  get_att_specs() %>% # this function removes non-attraction trials
  mutate(m=mean(a),s=sd(a)) %>%
  select(-c(s,m,drop))

plot_hist <- function(d, filename, do_log=F, save=T){
  if(do_log){
    d$a <- log(d$a)
  }
  p <- d %>%
    ggplot(aes(a))+
    geom_histogram(fill="lightblue",col="black")+
    facet_grid(stim~disp_cond)+
    labs(x=ifelse(do_log,"area (px)","log area (px)"))+
    ggthemes::theme_few()+
    theme(text = element_text(size=14))
  if(save) ggsave(p, filename=filename, width=5, height=4)
  p
}

d_exp_att %>%
  plot_hist(filename=here("plots","attraction_area_hist.jpg"),save=!sean_data)
d_exp_att %>%
  plot_hist(filename=here("plots","attraction_logarea_hist.jpg"),do_log=T,save=!sean_data)

d_exp_att_wide <- d_exp_att %>%
  pivot_wider(names_from = stim, values_from = a)

plot_cors <- function(d, x, y, filename, do_log=F, save=T){
  # browser()
  p <- d %>%
    ggplot(aes({{x}},{{y}}))+
    geom_point(alpha=.5)+
    facet_grid(disp_cond~distance)+
    ggthemes::theme_few()+
    coord_fixed()+
    theme(axis.text.x = element_text(angle=ifelse(do_log,0,90)))
  ggsave(p, filename=filename,width=4,height=6)
  return(p)
}

d_exp_att_wide %>%
  plot_cors(t,c,filename=here("plots","cor_plot_raw_tc.jpg"),save=!sean_data)
d_exp_att_wide %>%
  plot_cors(t,d,filename=here("plots","cor_plot_raw_td.jpg"),save=!sean_data)
d_exp_att_wide %>%
  plot_cors(c,d,filename=here("plots","cor_plot_raw_cd.jpg"),save=!sean_data)
d_exp_att_wide %>%
  plot_cors(t,c,filename=here("plots","cor_plot_log_tc.jpg"),do_log=T,save=!sean_data)
d_exp_att_wide %>%
  plot_cors(t,d,filename=here("plots","cor_plot_log_td.jpg"),do_log=T,save=!sean_data)
d_exp_att_wide %>%
  plot_cors(c,d,filename=here("plots","cor_plot_log_cd.jpg"),do_log=T,save=!sean_data)

# transforming data ========================================================================
zscore_data <- function(d, d_exp){
  d %>%
    select(sub_n,set,distance,diag,block_n,trial_n,t,d,c) %>%
    pivot_longer(c(t,d,c),names_to = "stim",values_to = "a") %>%
    group_by(sub_n,diag) %>%
    mutate(z=(a-mean(a))/sd(a)) %>%
    ungroup() %>%
    select(-a) %>%
    pivot_wider(names_from = stim,
                values_from = z) %>%
    left_join(distinct(d_exp, sub_n,disp_cond)) %>%
    relocate(disp_cond,.after=sub_n)
}
d_exp_att_wide_log <- d_exp_att_wide %>%
  mutate(across(c(t,d,c),log))
d_exp_att_wide_log_zscore <- zscore_data(d_exp_att_wide_log,d_exp)

# dropping outliers - USING LOG SCALED DATA ===========================================================================


drop_outliers <- function(dd){
  cv <- cov(dd[,c("t","c","d")])
  m <- c(mean(dd$t),mean(dd$c),mean(dd$d))
  
  dd$dens <- pmap_dbl(list(dd$t,dd$c,dd$d),function(t,c,d) dmvnorm(x=c(t,c,d),mean=m,sigma=cv))
  dd_1 <- dd %>%
    mutate(q=ecdf(d)(d),
           drop=q<=.02) 
  print(glue("{sum(dd_1$drop)}/{nrow(dd_1)} trials were dropped."))
  return(
    dd_1 %>%
      filter(!drop) %>%
      select(-c(q,dens,drop))
  )
}

d_exp_att_wide_log_zscore_outl_rem <- d_exp_att_wide_log_zscore %>%
  drop_outliers()

# quick check on w vs h ================================================================
d_exp_att_wide_log_zscore_outl_rem %>%
  mutate(w=case_when(
    set=="a-b-da"~c,
    set=="a-b-db"~t
  ),
  h=case_when(
    set=="a-b-da"~t,
    set=="a-b-db"~c
  ),
  dw=case_when(
    set=="a-b-db"~d,
    T~NA
  ),
  dh=case_when(
    set=="a-b-da"~d,
    T~NA
  )) %>%
  group_by(distance) %>%
  summarise(mu_w=mean(w),
            mu_h=mean(h),
            mu_dw=mean(dw,na.rm=T),
            mu_dh=mean(dh,na.rm=T),
            mu_w_h_diff=mean(w-h))

# replot correlations w/ outliers removed, data transformed=========================================================

d_exp_att_wide_log_zscore_outl_rem %>%
  plot_cors(c,d,filename=here("plots","cor_plot_log_zscore_outliers_removed_cd.jpg"),do_log=F,save=!sean_data)
d_exp_att_wide_log_zscore_outl_rem %>%
  plot_cors(t,d,filename=here("plots","cor_plot_log_zscore_outliers_removed_td.jpg"),do_log=F,save=!sean_data)
par(pty='s',mfrow=c(3,1))
plot(d_exp_att_wide_log_zscore_outl_rem$t, d_exp_att_wide_log_zscore_outl_rem$d, pch=".",xlab="t",ylab="d")
plot(d_exp_att_wide_log_zscore_outl_rem$t, d_exp_att_wide_log_zscore_outl_rem$c, pch=".",xlab="t",ylab="c")
plot(d_exp_att_wide_log_zscore_outl_rem$c, d_exp_att_wide_log_zscore_outl_rem$d, pch=".",xlab="c",ylab="d")
d_exp_att_wide_log_zscore_outl_rem %>%
  plot_cors(t,c,filename=here("plots","cor_plot_log_zscore_outliers_removed_tc.jpg"),do_log=F,save=!sean_data)

plot(d_exp_att_wide_log_zscore_outl_rem$t, d_exp_att_wide_log_zscore_outl_rem$d)
# get distribution parameters w/ outliers removed, data transformed ============================================================
mean_pars <- d_exp_att_wide_log_zscore_outl_rem %>% 
  pivot_longer(c(t,d,c),names_to = "stim",
               values_to = "ca") %>%
  mutate(stim_by_type=case_when(
    stim=="t" & set=="a-b-da"~"a",
    stim=="c" & set=="a-b-da"~"b",
    stim=="t" & set=="a-b-db"~"b",
    stim=="c" & set=="a-b-db"~"a",
    stim=="d" ~ "d"
  )) %>%
  select(-stim) %>%
  group_by(disp_cond,distance,set,stim_by_type) %>%
  summarise(m_ca=mean(ca),
            se=sd(ca)/sqrt(n()),
            se_lower=m_ca-se,
            se_upper=m_ca+se)  %>%
  ungroup()
mean_pars
mean_pars %>%
  mutate(distance=as.factor(distance)) %>%
  ggplot(aes(distance, m_ca, shape=stim_by_type,col=stim_by_type))+
  geom_point(size=2.5,alpha=.75)+
  # geom_errorbar(aes(ymin=se_lower,ymax=se_upper),width=.25)+
  scale_shape_manual(values=c(0,2,4), labels=c("a","b","d"),name="rectangle")+
  scale_color_manual(values=ggsci::pal_startrek()(3), labels=c("a","b","d"),name="rectangle")+
  scale_x_discrete(labels=c("2%","5%","9%","14%"))+
  labs(x="target-decoy distance",y="mean judged area")+
  lemon::facet_rep_grid(disp_cond~set, scales = "free_x", repeat.tick.labels = "bottom")+
  ggthemes::theme_few()  
if(!sean_data){
  ggsave(filename = here("plots","mean_ca_by_set_distance.jpg"),width=5,height=4)
}


d_exp_att_wide_log_zscore_outl_rem %>%
  group_by(disp_cond) %>%
  summarise(r_td=cor(t,d),
            r_tc=cor(t,c),
            r_cd=cor(c,d))
cov_pars <- d_exp_att_wide_log_zscore_outl_rem %>%
  group_by(disp_cond) %>%
  summarise(var_t=var(t),
            var_d=var(d),
            var_c=var(c),
            cov_td=cov(t,d),
            cov_tc=cov(t,c),
            cov_cd=cov(c,d)) %>%
  ungroup()

if(!sean_data){
  save(mean_pars, cov_pars, file=here("tmp_for_brownbag","att_pars_tmp.RData"))
}

# some regression modeling ======================================================================
d_exp_att_long_log_zscore_outl_rem <- d_exp_att_wide_log_zscore_outl_rem %>%
  pivot_longer(c(t,d,c),names_to = "opt", values_to = "ca") %>%
  mutate(w=case_when(
    set=="a-b-da" & opt %in% c("t","d")~0,
    set=="a-b-da" & opt == "c" ~ 1,
    set=="a-b-db" & opt %in% c("t","d")~1,
    set=="a-b-db" & opt == "c" ~ 0
  ),
  dist=case_when(
    opt %in% c("t","c")~0,
    T~distance
  ),
  across(c(w,dist),as.factor)) %>%
  select(-distance)

fit_model <- function(d, cond, n_iter, n_cores=4){
  # browser()
  mstring <- glue("bayes_mixeff_{cond}")
  direc <- here(glue("{mstring}_{Sys.Date()}"))
  dir_create(direc)
  dd <- d %>%
    filter(str_detect(disp_cond,cond)) %>%
    mutate(sub_n=as.factor(sub_n))
  ff <- path(direc,"fit.RData")
  if(!file_exists(ff)){
    m <- stan_lmer(ca~w+dist+(0+dist|sub_n)+(0+w|sub_n), data=dd, iter=n_iter, chains=n_cores, cores=n_cores)
    save(m, dd, file=ff)
  }else{
    load(ff)
    color_scheme_set("red")
    theme_set(ggthemes::theme_few())
    try({
      mcmc_trace(m, pars="(Intercept)")
      ggsave(path(direc,"int_trace.jpeg"),width=4,height=4)
    })
    try({
      mcmc_trace(m, pars="w1")
      ggsave(path(direc,"bw_trace.jpeg"),width=4,height=4)
    })
    try({
      mcmc_trace(m, pars=c("dist2","dist5","dist9","dist14"))
      ggsave(path(direc,"bdist_trace.jpeg"),width=4,height=4)
    })
    try({
      mcmc_hist(m, pars="(Intercept)")
      ggsave(path(direc,"int_hist.jpeg"),width=4,height=4)
    })
    try({
      mcmc_hist(m, pars="w1")+
        scale_x_continuous(limits=c(-.25, .2))
      ggsave(path(direc,"bw_hist.jpeg"),width=4,height=4)
    })
    try({
      mcmc_hist(m, pars=c("dist2","dist5","dist9","dist14"))+
        scale_x_continuous(limits=c(-.4, .1))
      ggsave(path(direc,"bdist_hist.jpeg"),width=4,height=4)
    })
    try({
      mcmc_dens_overlay(m, pars="(Intercept)")
      ggsave(path(direc,"int_dens.jpeg"),width=4,height=4)
    })
    try({
      mcmc_dens_overlay(m, pars="w1")+
        scale_x_continuous(limits=c(-.25, .2))
      ggsave(path(direc,"bw_dens.jpeg"),width=4,height=4)
    })
    try({
      mcmc_dens_overlay(m, pars=c("dist2","dist5","dist9","dist14"))+
        scale_x_continuous(limits=c(-.4, .1))
      ggsave(path(direc,"bdist_dens.jpeg"),width=4,height=4)
    })
    try({
      ca_rep <- posterior_predict(m, draws=500)
      ca <- dd$ca
      ppc_dens_overlay(ca, ca_rep)
      ggsave(path(direc,"ppc.jpeg"),width=4,height=4)
    })
  }
  return(m)
}

if(!sean_data){
  fit_h <- fit_model(d_exp_att_long_log_zscore_outl_rem, "horizontal", 10000, 8)
  fit_t <- fit_model(d_exp_att_long_log_zscore_outl_rem, "triangle", 10000, 8)
}

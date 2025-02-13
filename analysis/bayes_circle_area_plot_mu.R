rm(list=ls())
library(tidyverse)
library(glue)
library(here)
which_model <- "sigma_constant"
f_h <- here("analysis","bayes",which_model,"horizontal","no_outliers","mu_avgs.RData")
f_t <- here("analysis","bayes",which_model,"triangle","no_outliers","mu_avgs.RData")

load_mu <- function(f, cond){
  load(f)
  mu_avgs_by_sub_set_distance_diag$disp_cond <- cond
  mu_avgs_by_sub_distance$disp_cond <- cond
  mu_avgs_fully_collapsed_w_hdis$disp_cond <- cond
  mu_avgs_by_sub_set_distance_diag$source <- "model"
  mu_avgs_by_sub_distance$source <- "model"
  mu_avgs_fully_collapsed_w_hdis$source <- "model"
  
  return(list(mu_avgs_by_sub_set_distance_diag,
              mu_avgs_by_sub_distance,
              mu_avgs_fully_collapsed_w_hdis))
}

mu_all <- map2(c(f_h,f_t),c("horizontal","triangle"),load_mu)

mu_avgs_by_sub_set_distance_diag <- bind_rows(mu_all[[1]][[1]],
                                              mu_all[[2]][[1]])
mu_avgs_by_sub_distance <- bind_rows(mu_all[[1]][[2]],
                                     mu_all[[2]][[2]])
mu_avgs_fully_collapsed <- bind_rows(mu_all[[1]][[3]],
                                     mu_all[[2]][[3]])

get_data <- function(which_dat){
  d <- tibble()
  for(cond in c("horizontal","triangle")){
    load(here("analysis","bayes","sigma_constant",cond,"no_outliers","dat_for_model.RDS"))
    d <- bind_rows(d, mutate(dat_all_clean, disp_cond=cond))
  }
  return(d)
}

plot_collapsed <- function(mu_avgs_fully_collapsed){
  d <- get_data()
  d %>%
    pivot_longer(c(t,c,d),names_to = "stim") %>%
    group_by(disp_cond, distance, stim) %>%
    summarise(m=mean(value)) %>%
    ungroup() %>%
    mutate(source="data") %>%
    bind_rows(mu_avgs_fully_collapsed) %>%
    ggplot(aes(distance,m,col=stim,shape=source))+
    geom_errorbar(aes(ymin=hdi_lower,ymax=hdi_upper,col=stim))+
    geom_point(alpha=.5)+
    scale_x_continuous(breaks=c(2,5,9,14),limits=c(0,16),labels=c("2%","5%","9%","14%"))+
    scale_y_continuous(limits=c(-.1,.05))+
    scale_shape_manual(values=c(1,4),name="")+
    ggsci::scale_color_tron(labels=c("competitor","decoy","target"))+
    labs(x="tdd",y="mean log area")+
    facet_wrap(vars(disp_cond))+
    ggthemes::theme_few()
}

plot_collapsed(mu_avgs_fully_collapsed)
ggsave(filename=here("analysis","plots",glue("bayes_circle_area_mu_{which_model}_model_v_data_collapsed.jpeg")),
       width=6,height=5,dpi=800)
plot_by_sub_set_distance_diag <- function(mu_avgs_by_sub_set_distance_diag){
  d <- get_data()
  d %>%
    pivot_longer(c(t,c,d),names_to = "stim") %>%
    group_by(sub_n, set, distance, diag, stim) %>%
    summarise(m=mean(value)) %>%
    ungroup() %>%
    left_join(distinct(d,sub_n,disp_cond)) %>%
    mutate(source="data") %>%
    bind_rows(mu_avgs_by_sub_set_distance_diag) %>%
    pivot_wider(names_from = source, values_from = c(m,hdi_lower,hdi_upper)) %>%
    ggplot(aes(m_data,m_model,col=stim))+
    geom_point(alpha=.3,shape=".")+
    geom_errorbar(aes(ymin=hdi_lower_model,ymax=hdi_upper_model))+
    geom_abline(slope=1,intercept=0,alpha=.5,linetype="dashed")+
    coord_fixed(xlim=c(-1.5,1.5),ylim=c(-1.5,1.5))+
    ggsci::scale_color_tron()+
    labs(x="data",y="model")+
    facet_wrap(vars(disp_cond))+
    ggthemes::theme_few()+
    theme(text=element_text(size=18))
}
plot_by_sub_set_distance_diag(mu_avgs_by_sub_set_distance_diag)
ggsave(filename=here("analysis","plots",glue("bayes_circle_area_mu_{which_model}_model_v_data_by_sub_set_distance_diag.jpeg")),
       width=6,height=5,dpi=800)

plot_by_sub_distance <- function(mu_avgs_by_sub_distance){
  d <- get_data()
  d %>%
    pivot_longer(c(t,c,d),names_to = "stim") %>%
    group_by(sub_n, distance, stim) %>%
    summarise(m=mean(value)) %>%
    ungroup() %>%
    left_join(distinct(d,sub_n,disp_cond)) %>%
    mutate(source="data") %>%
    bind_rows(mu_avgs_by_sub_distance) %>%
    pivot_wider(names_from = source, values_from = c(m,hdi_lower,hdi_upper)) %>%
    ggplot(aes(m_data,m_model,col=stim))+
    geom_point(alpha=.3)+
    # geom_errorbar(aes(ymin=hdi_lower_model,ymax=hdi_upper_model),alpha=.2)+
    geom_abline(slope=1,intercept=0,alpha=.5,linetype="dashed")+
    ggsci::scale_color_tron()+
    labs(x="data",y="model")+
    coord_fixed(xlim=c(-1,1),ylim=c(-1,1))+
    facet_wrap(vars(disp_cond))+
    ggthemes::theme_few()+
    theme(text=element_text(size=18))
}

plot_by_sub_distance(mu_avgs_by_sub_distance)
ggsave(filename=here("analysis","plots",glue("bayes_circle_area_mu_{which_model}_model_v_data_by_sub_distance.jpeg")),
       width=6,height=5,dpi=800)

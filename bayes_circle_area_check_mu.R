# setup =================================================================================
rm(list=ls())
library(here)
library(tidyverse)
library(fs)
library(glue)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(qs)
# install_cmdstan(dir=here())
# set_cmdstan_path(here("cmdstan-2.35.0"))
# set_cmdstan_path("/Users/seanconway/cmdstan")

# Control parameters
outliers_removed <- T

for(which_model in c("sigma_constant","sigma_varying")){
  for(which_cond in c("triangle","horizontal")){
    
    results_dir <- here("bayes",glue("{which_model}/{which_cond}/{tmp}",tmp=ifelse(outliers_removed,"no_outliers","with_outliers")))
    # number of chains/cores, warm up, iterations
    if(which_model=="sigma_constant"){
      n_chain <- 5
      n_iter <- 4000
    }else if(which_model=="sigma_varying"){ # want more samples for this model
      n_chain <- 6
      n_iter <- 7000
    }

    # number of total draws
    n_draws <- n_chain*n_iter
    
    mu <- qread(path(results_dir,"mu.qs"))
    dat_all <- qread(path(results_dir,"dat_for_model.qs"))
    df <- dat_all[[1]]
    
    mu_df <- tibble(t=as.vector(t(mu[,,1])),
                    c=as.vector(t(mu[,,2])),
                    d=as.vector(t(mu[,,3]))) %>%
      mutate(iter=sort(rep(1:nrow(df),n_draws)),
             sub_n=rep(df$sub_n,n_draws),
             diag=rep(df$diag,n_draws),
             set=rep(df$set,n_draws),
             distance=rep(df$distance,n_draws))
    
    mu_df_summary_by_sub <- mu_df %>%
      group_by(sub_n,diag,set,distance) %>%
      summarise(m_t=mean(t),
                lower_t=HDInterval::hdi(t)[1],
                upper_t=HDInterval::hdi(t)[2],
                m_c=mean(c),
                lower_c=HDInterval::hdi(c)[1],
                upper_c=HDInterval::hdi(c)[2],
                m_d=mean(d),
                lower_d=HDInterval::hdi(d)[1],
                upper_d=HDInterval::hdi(d)[2]) %>%
      ungroup() %>%
      mutate(source="model")
    
    mu_df_summary_collapsed <- mu_df %>%
      group_by(diag,set,distance) %>%
      summarise(m_t=mean(t),
                lower_t=HDInterval::hdi(t)[1],
                upper_t=HDInterval::hdi(t)[2],
                m_c=mean(c),
                lower_c=HDInterval::hdi(c)[1],
                upper_c=HDInterval::hdi(c)[2],
                m_d=mean(d),
                lower_d=HDInterval::hdi(d)[1],
                upper_d=HDInterval::hdi(d)[2]) %>%
      ungroup() %>%
      mutate(source="model")
    
    dat_model_by_sub <- df %>%
      group_by(sub_n,diag,set,distance) %>%
      summarise(m_t=mean(t),
                m_c=mean(c),
                m_d=mean(d)) %>%
      ungroup() %>% 
      mutate(source="data") %>%
      bind_rows(mu_df_summary_by_sub)
    
    dat_model_collapsed <- df %>%
      group_by(diag,set,distance) %>%
      summarise(m_t=mean(t),
                m_c=mean(c),
                m_d=mean(d)) %>%
      ungroup() %>% 
      mutate(source="data") %>%
      bind_rows(mu_df_summary_collapsed)
    dat_model_by_sub_tidy <- dat_model_by_sub %>%
      pivot_longer(c(m_t,m_c,m_d,lower_d,
                     upper_d,
                     lower_t,
                     upper_t,
                     lower_c,
                     upper_c),names_to="tmp") %>%
      separate(tmp,into=c("measure","stim"),sep="_") %>%
      pivot_wider(names_from=c(source,measure),
                  values_from=value)
    
    dat_model_collapsed_tidy <- dat_model_collapsed %>%
      pivot_longer(c(m_t,m_c,m_d,lower_d,
                     upper_d,
                     lower_t,
                     upper_t,
                     lower_c,
                     upper_c),names_to="tmp") %>%
      separate(tmp,into=c("measure","stim"),sep="_") %>%
      pivot_wider(names_from=c(source,measure),
                  values_from=value)
    
    dat_model_by_sub_tidy %>%
      ggplot(aes(data_m,model_m,col=stim))+
      geom_abline(slope=1,intercept=0,alpha=.25,linetype="dashed")+
      geom_point(alpha=.6)+
      geom_errorbar(aes(ymin=model_lower,ymax=model_upper),alpha=.2)+
      coord_fixed(xlim=c(-1,1),ylim=c(-1,1))+
      labs(x="data means",y="model means",title=glue("{which_cond} condition"),
           subtitle = which_model)+
      ggthemes::theme_few()+
      ggsci::scale_color_startrek(name="")+
      theme(text = element_text(size=15))
    ggsave(filename = path(results_dir,"mu_check_by_sub.jpeg"),width=5,height=5)
    
    dat_model_collapsed_tidy %>%
      ggplot(aes(data_m,model_m,col=stim))+
      geom_abline(slope=1,intercept=0,alpha=.25,linetype="dashed")+
      geom_point(alpha=.6)+
      geom_errorbar(aes(ymin=model_lower,ymax=model_upper),alpha=.2)+
      coord_fixed(xlim=c(-1,1),ylim=c(-1,1))+
      labs(x="data means",y="model means",title=glue("{which_cond} condition"),
           subtitle = which_model)+
      ggthemes::theme_few()+
      ggsci::scale_color_startrek(name="")+
      theme(text = element_text(size=15))
    ggsave(filename = path(results_dir,"mu_check_collapsed.jpeg"),width=5,height=5)
    rm(mu)
    rm(mu_df)
    rm(mu_df_summary_collapsed)
    rm(mu_df_summary_by_sub)
  }
}

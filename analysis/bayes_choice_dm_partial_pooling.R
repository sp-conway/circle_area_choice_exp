# bayesian modeling of choice data
# using multinomial dirichlet model
# setup ====================================================================================================
rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(glue)
library(cmdstanr)
library(bayesplot)
library(posterior)

# control settings
which_model <- "bayes_choice_dm_partial_pooling"

# sampler settings
n_chain <- n_core <- 4
n_iter <- 2000

# set path to cmdstan
set_cmdstan_path(here("cmdstan-2.36.0"))

for(which_cond in c("triangle","horizontal")){
    
  # paths etc
  stan_dir <- here("analysis","bayes_choice","stan")
  stan_file <- path(stan_dir,glue("{which_model}.stan"))
  results_dir <- here("analysis","bayes_choice",which_model,which_cond)
  dir_create(results_dir)

# load and prep data ====================================================================================================
  d <- here("data","choice","aggregated","choice_all.csv") %>%
      read_csv() %>%
      filter(str_detect(effect,"attraction") & 
               str_detect(disp_cond,which_cond)) %>%
      mutate(choice_tdc=factor(choice_tdc,
                               levels=c("target","competitor","decoy")))
  d1 <- d %>%
    group_by(sub_n,distance,choice_tdc) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    pivot_wider(names_from = choice_tdc, values_from = n, values_fill = 0)
  n_subs <- length(unique(d$sub_n))
  d_counts <- array(NA_integer_,dim=c(n_subs,4,3))
  sub_ns <- sort(unique(d1$sub_n))
  for(s in 1:n_subs){
    print(s)
    for(dd in 1:4){
      d_tmp <- filter(d1,sub_n==sub_ns[s] & distance==c(2,5,9,14)[dd])
      d_counts[s,dd,] <- c(d_tmp$target,
                           d_tmp$competitor,
                           d_tmp$decoy)
    }
  }
  stan_data <- list(
    D=4,
    O=3,
    S=n_subs,
    d_counts=d_counts
  )
  
  # compile and fit model =========================================================================================
  m <- cmdstan_model(stan_file)
  fit <- m$sample(data=stan_data,
                  chains=n_chain,
                  parallel_chains=n_chain,
                  refresh=100,
                  iter_sampling=n_iter,
                  output_dir=results_dir,
                  output_basename="bayes_choice")
  draws <- fit$draws()
  color_scheme_set("red")
  mcmc_trace(draws,"lp__")
  ggsave(filename=path(results_dir,"lp__trace.jpeg"),width=8,height=8)
  mcmc_trace(draws,regex_pars = "p_m")
  ggsave(filename=path(results_dir,"p_m_trace.jpeg"),width=8,height=8)
  mcmc_trace(draws,regex_pars = "alpha_mu")
  ggsave(filename=path(results_dir,"alpha_mu_trace.jpeg"),width=8,height=8)
  mcmc_trace(draws,regex_pars = "alpha_sigma")
  ggsave(filename=path(results_dir,"alpha_sigma_trace.jpeg"),width=8,height=8)
  
  fit_summary <- summarise_draws(draws, mean,~quantile(.x, probs = c(.025, .975)))
  model_preds <- fit_summary %>%
    filter(str_detect(variable,"p_m")) %>%
    rename(hdi_lower=`2.5%`,
           hdi_upper=`97.5%`,
           m=mean) %>%
    mutate(choice=case_when(
      str_detect(variable,"1\\]")~"t",
      str_detect(variable,"2\\]")~"c",
      str_detect(variable,"3\\]")~"d"
    ),
    distance=case_when(
      str_detect(variable,"\\[1")~2,
      str_detect(variable,"\\[2")~5,
      str_detect(variable,"\\[3")~9,
      str_detect(variable,"\\[4")~14
    )) %>%
    mutate(source="model") %>%
    dplyr::select(-variable)
  
  mdat <- d1 %>%
    rename(t=target,
           c=competitor,
           d=decoy) %>%
    pivot_longer(-c(distance,sub_n),names_to = "choice",values_to = "n") %>%
    group_by(sub_n,distance) %>%
    mutate(prop=n/sum(n)) %>%
    group_by(distance,choice) %>%
    summarise(m=mean(prop)) %>%
    ungroup() %>%
    mutate(source="data") %>%
    bind_rows(model_preds)
  
  mdat %>%
    ggplot(aes(distance,m,col=choice,shape=source))+
    geom_point(alpha=.5)+
    geom_line()+
    geom_errorbar(aes(ymin=hdi_lower,ymax=hdi_upper),width=.25)+
    scale_shape_manual(values=c(1,4),name="")+
    ggsci::scale_color_startrek(name="stimulus")+
    scale_x_continuous(breaks=c(2,5,9,14),limits=c(1.5,14.5),labels=c("2%","5%","9%","14%"))+
    scale_y_continuous(limits=c(0,1),breaks=seq(0,1,.2))+
    labs(x="tdd",y="mean choice proportion")+
    ggthemes::theme_few()+
    theme(text=element_text(size=18))
  ggsave(filename=path(results_dir,"data_model_preds.jpeg"),width=5,height=4)
  
  
  model_preds_by_sub <- fit_summary %>%
    filter(str_detect(variable,"theta")) %>%
    mutate(sub_index=as.numeric(str_extract(variable,"(?<=theta\\[)[:digit:]{1,}(?=,)")),
           distance=c(2,5,9,14)[as.numeric(str_extract(variable,"(?<=,)[:digit:]{1}(?=,)"))],
           choice_n=as.numeric(str_extract(variable,"(?<=,)[:digit:]{1}(?=\\])")),
           choice=case_when(
             choice_n==1~"t",
             choice_n==2~"c",
             choice_n==3~"d"
           ),
           sub_n=sub_ns[sub_index]) %>%
    dplyr::select(-c(sub_index,variable,choice_n)) %>%
    rename(hdi_lower=`2.5%`,
           hdi_upper=`97.5%`,
           m=mean) %>%
    mutate(source="model") 
  mdat_by_sub <- d1 %>%
    rename(t=target,
           c=competitor,
           d=decoy) %>%
    pivot_longer(-c(distance,sub_n),names_to = "choice",values_to = "n") %>%
    group_by(sub_n,distance) %>%
    mutate(m=n/sum(n)) %>%
    ungroup() %>%
    mutate(source="data") %>%
    dplyr::select(-c(n)) %>%
    bind_rows(model_preds_by_sub)
  
  mdat_by_sub %>% 
    pivot_wider(names_from = source, 
                values_from = c(m,hdi_lower,hdi_upper)) %>%
    ggplot(aes(m_data,m_model,col=choice))+
    geom_point(alpha=.5)+
    geom_errorbar(aes(ymin=hdi_lower_model,ymax=hdi_upper_model))+
    coord_fixed(xlim=c(0,1),ylim=c(0,1))+
    facet_grid(distance~.)+
    ggthemes::theme_few()
  ggsave(filename=path(results_dir,"data_model_sub_preds.jpeg"),width=5,height=6)
}



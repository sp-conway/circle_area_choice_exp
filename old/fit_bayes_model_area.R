# setup ===========================================================================
rm(list=ls())
library(tidyverse)
library(rstan)
library(glue)
library(fs)
library(here)
library(bayesplot)

# # ONLY UNCOMMENT THIS LINE IF FITTING ON UNITY CLUSTER
# options(mc.cores = parallel::detectCores())

# model settings
cv <- F # whether to estimate variance-covariance matrix
include_diag <- F # whether to include effect of diagonal (no longer necessary after data have been logged and z-scored by subject and diagonal)

# MCMC settings
if(cv){
  n_iter <- 15000
  n_chain <- 4
}else{
  n_iter <- 10000
  n_chain <- 4
}

# both between-subjects conditions
conds <- c("triangle","horizontal")

# data load, modification, and modeling ================================================================================
for(cond in conds){
  bayes_folder <- here("bayes_modeling")
  dir_create(bayes_folder)
  results_path <- path(bayes_folder,
                       "results",
                       glue("{cond}{cv_setting}{diag_setting}",
                            cv_setting=ifelse(cv, "_cv_",""),
                            diag_setting=ifelse(include_diag,"","_no_diag")))
  dir_create(results_path)
  fit_file <- path(results_path,"fit.RData")
  
  dat <- here("data","circle_area","aggregated","circle_area_attraction_log_zscore_outliers_removed.csv") %>%
    read_csv() %>%
    filter(str_detect(disp_cond,cond)) %>%
    arrange(sub_n,block_n,trial_n)
  n_subs <- length(unique(dat$sub_n))
  
  if(cv){
    
  }else{
    dat1 <- dat %>%
      pivot_longer(c(t,c,d),names_to = "stim",values_to = "a") %>%
      mutate(w=case_when(
        str_detect(set,"a-b-da") & str_detect(stim,"d|t")~0,
        str_detect(set,"a-b-da") & str_detect(stim,"c")~1,
        str_detect(set,"a-b-db") & str_detect(stim,"d|t")~1,
        str_detect(set,"a-b-db") & str_detect(stim,"c")~0,
      ),
      dist2=if_else(distance==2 & str_detect(stim,"d"),1,0),
      dist5=if_else(distance==5 & str_detect(stim,"d"),1,0),
      dist9=if_else(distance==9 & str_detect(stim,"d"),1,0),
      dist14=if_else(distance==14 & str_detect(stim,"d"),1,0),
      diag2=if_else(diag==2,1,0),
      diag3=if_else(diag==3,1,0))
    
    if(include_diag){
      d <- as.matrix(dat1[,c("dist2","dist5","dist9","dist14","diag2","diag3","w")])
      mm_ref <- dat %>%
        pivot_longer(c(t,c,d),names_to = "stim",values_to = "a") %>%
        distinct(set, stim, distance, diag) %>%
        mutate(w=case_when(
          str_detect(set,"a-b-da") & str_detect(stim,"d|t")~0,
          str_detect(set,"a-b-da") & str_detect(stim,"c")~1,
          str_detect(set,"a-b-db") & str_detect(stim,"d|t")~1,
          str_detect(set,"a-b-db") & str_detect(stim,"c")~0,
        ),
        dist2=if_else(distance==2 & str_detect(stim,"d"),1,0),
        dist5=if_else(distance==5 & str_detect(stim,"d"),1,0),
        dist9=if_else(distance==9 & str_detect(stim,"d"),1,0),
        dist14=if_else(distance==14 & str_detect(stim,"d"),1,0),
        diag2=if_else(diag==2,1,0),
        diag3=if_else(diag==3,1,0))
      means_tmp <- dat %>%
        pivot_longer(c(t,c,d),names_to = "stim",values_to = "a") %>%
        group_by(set, stim, diag, distance) %>% 
        summarise(m=mean(a)) %>%
        ungroup()
      mm_ref_means <- mm_ref %>%
        left_join(means_tmp)
      mm <- as.matrix(mm_ref[,c("dist2","dist5","dist9","dist14","diag2","diag3","w")])
    }else{
      d <- as.matrix(dat1[,c("dist2","dist5","dist9","dist14","w")])
      mm_ref <- dat %>%
        pivot_longer(c(t,c,d),names_to = "stim",values_to = "a") %>%
        distinct(set, stim, distance) %>%
        mutate(w=case_when(
          str_detect(set,"a-b-da") & str_detect(stim,"d|t")~0,
          str_detect(set,"a-b-da") & str_detect(stim,"c")~1,
          str_detect(set,"a-b-db") & str_detect(stim,"d|t")~1,
          str_detect(set,"a-b-db") & str_detect(stim,"c")~0,
        ),
        dist2=if_else(distance==2 & str_detect(stim,"d"),1,0),
        dist5=if_else(distance==5 & str_detect(stim,"d"),1,0),
        dist9=if_else(distance==9 & str_detect(stim,"d"),1,0),
        dist14=if_else(distance==14 & str_detect(stim,"d"),1,0))
      means_tmp <- dat %>%
        pivot_longer(c(t,c,d),names_to = "stim",values_to = "a") %>%
        group_by(set, stim, distance) %>% 
        summarise(m=mean(a)) %>%
        ungroup()
      mm_ref_means <- mm_ref %>%
        left_join(means_tmp)
      mm <- as.matrix(mm_ref[,c("dist2","dist5","dist9","dist14","w")])
    }
    a <- dat1$a
    N <- nrow(d)
    K <- ncol(d)
    N_unique <- nrow(mm)
    data_all <- list(N=N,
                     d=d,
                     K=K,
                     a=a,
                     N_unique=N_unique,
                     mm=mm)
    model_path <- path(bayes_folder,
                       "stan_code",
                       glue("bayes_model_area{cv_setting}{diag_setting}.stan",
                            cv_setting=ifelse(cv, "_cv",""),
                            diag_setting=ifelse(include_diag,"","_no_diag")))
                       # glue("bayes_model_area{cv_setting}.stan",
                       #      cv_setting=ifelse(cv, "_cv","")))
    if(!file_exists(fit_file)){
      mod <- stan_model(model_path)
      fit <- sampling(mod, data=data_all, iter=n_iter, chains=n_chain, cores=n_chain)
      save(list=ls(), file=path(results_path,"fit.RData"))
    }else{
      load(fit_file)
    }
    posterior <- as.array(fit)
    color_scheme_set("red")
    
    try({
      stan_rhat(fit)
      ggsave(filename = path(results_path,"rhat.jpg"),width=4,height=4)
    })
    try({
      stan_trace(fit, c("b_0"))
      ggsave(filename = path(results_path,"b_0_trace.jpg"),width=4,height=4)
    })
    try({
      stan_trace(fit, c("b_dist2","b_dist5","b_dist9","b_dist14"))
      ggsave(filename = path(results_path,"b_dist_trace.jpg"),width=4,height=4)
    })
    if(include_diag){
      try({
        stan_trace(fit, c("b_diag2","b_diag3"))
        ggsave(filename = path(results_path,"b_diag_trace.jpg"),width=4,height=4)
      })
      try({
        stan_hist(fit, c("b_diag2","b_diag3"))
        ggsave(filename = path(results_path,"b_diag_hist.jpg"),width=4,height=4)
      })
    }
    try({
      stan_trace(fit, c("b_w"))
      ggsave(filename = path(results_path,"b_w_trace.jpg"),width=4,height=4)
    })
    try({
      stan_trace(fit, c("b_sigma","sigma"))
      ggsave(filename = path(results_path,"sigma_trace.jpg"),width=4,height=4)
    })
    try({
      stan_hist(fit, c("b_0"))
      ggsave(filename = path(results_path,"b_0_hist.jpg"),width=4,height=4)
    })
    try({
      stan_hist(fit, c("b_sigma","sigma"))
      ggsave(filename = path(results_path,"sigma_hist.jpg"),width=4,height=4)
    })
    try({
      stan_hist(fit, c("b_dist2","b_dist5","b_dist9","b_dist14"))+
        scale_x_continuous(limits = c(-.35,.15))
      ggsave(filename = path(results_path,"b_dist_hist.jpg"),width=4,height=4)
    })
    try({
      mcmc_areas_ridges(posterior, c("b_dist2","b_dist5","b_dist9","b_dist14"))+
        ggthemes::theme_few()
      ggsave(filename = path(results_path,"b_dist_dens.jpg"),width=4,height=4)
    })
    try({
      stan_hist(fit, c("b_w"))
      ggsave(filename = path(results_path,"b_w_hist.jpg"),width=4,height=4)
    })
    try({
      p <- mcmc_pairs(posterior, c("b_dist2","b_dist5","b_dist9","b_dist14"))
      ggsave(p,filename = path(results_path,"b_dist_pairsplot.jpg"),width=7,height=7)
    })
    arep <- extract(fit,"arep")$arep
    arep_m <- apply(arep, 2, mean)
    try({
      mm_ref_means %>%
        mutate(mrep=arep_m) %>%
        ggplot(aes(m,mrep,col=stim))+
        geom_point(size=2.5,alpha=.6)+
        geom_abline(slope=1,intercept=0,linetype="dashed",alpha=.5)+
        coord_fixed(xlim = c(-.35,.35),ylim=c(-.35,.35))+
        labs(x="data means",y="model means")+
        ggthemes::theme_few()+
        theme(text = element_text(size=14))
      ggsave(filename = path(results_path,"ppc_means.jpg"),width=4,height=4)
    })
  }
}
  
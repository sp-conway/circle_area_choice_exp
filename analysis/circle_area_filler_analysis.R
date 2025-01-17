rm(list=ls())
library(here)
library(tidyverse)
library(fs)
library(latex2exp)
library(bayesplot)
library(glue)
library(rstanarm)
library(ggsci)

# controls
testing <- F

n_iter <- ifelse(testing,100,2000)


clean_data <- function(cond){
  # minimum radius
  min_rad <- 5
  min_area <- pi*min_rad^2
  min_log_area <- log(min_area)
  subs_keep <- here("analysis","subs_keep","subs_keep.txt") %>%
    read_lines() %>%
    as.numeric()
  d <- here("data","circle_area","aggregated","circle_area_all.csv") %>%
    read_csv() %>%
    filter(str_detect(effect,"filler")) %>%
    filter(str_detect(disp_cond,cond) & sub_n %in% subs_keep) %>%
    mutate(asp1=if_else(h1>w1,h1/w1,w1/h1),
           asp2=if_else(h2>w2,h2/w2,w2/h2),
           asp3=if_else(h3>w3,h3/w3,w3/h3)) %>%
    select(-c(computer_n,effect,contains("_rad"),rt,h1,h2,h3,w1,w2,w3,set,distance,diag)) %>%
    pivot_longer(c(contains("circle"),contains("rect"),contains("asp"))) %>%
    mutate(name=str_remove(name,"_area")) %>%
    separate(name,into=c("var","stim"),sep = "(?=[0-9])") %>% 
    pivot_wider(names_from = var,
                values_from = value) %>%
    filter(circle!=min_area) %>%
    mutate(circle=log(circle),
           rect=log(rect),
           asp=log(asp),
           aspsq=asp^2) %>%
    group_by(sub_n,trial_n,block_n) %>%
    mutate(circle=circle-mean(circle)) %>%
    ungroup() %>%
    mutate(rect=rect-mean(rect),
           sub_n=as.factor(sub_n))
  
}


for(cond in c("triangle","horizontal")){
  d <- clean_data(cond)
  save_dir <- here("analysis","filler_anaysis",cond)
  dir_create(save_dir)
  save_file <- path(save_dir,glue("m_{cond}_fit.RData"))
  if(!file_exists(save_file)){
    m <- stan_glmer(circle~rect+asp+aspsq+(1+rect+asp+aspsq|sub_n),data=d,
                    chains=4,iter=n_iter,cores=4)
    if(!testing){
      save(m,file=save_file)
      fit_summary <- summary(m)
      save(m,file=path(save_dir,glue("m_{cond}_fit_summary.RData")))
    }
    
  }else{
    if(!testing){
      load(save_file)
      posterior <- as.array(m)
      color_scheme_set("gray")
      p <- mcmc_trace(posterior,pars = "(Intercept)")
      p
      ggsave(p,filename=path(save_dir,glue("{cond}_int_trace.jpeg")),width=5,height=4)
      p <- mcmc_trace(posterior,pars = c("rect","asp","aspsq"))
      p
      ggsave(p,filename=path(save_dir,glue("{cond}_coeffs_trace.jpeg")),width=5,height=4)
      p <- mcmc_trace(posterior,regex_pars = "Sigma")
      p
      ggsave(p,filename=path(save_dir,glue("{cond}_Sigmas_trace.jpeg")),width=5,height=4)

      p <- mcmc_hist(posterior,pars = "(Intercept)")
      p
      ggsave(p,filename=path(save_dir,glue("{cond}_int_hist.jpeg")),width=5,height=4)
      p <- mcmc_hist(posterior,pars = c("rect","asp","aspsq"),facet_args=list(nrow=3))
      p
      ggsave(p,filename=path(save_dir,glue("{cond}_coeffs_hist.jpeg")),width=5,height=4)
      p <- mcmc_hist(posterior,regex_pars = "Sigma")
      p
      ggsave(p,filename=path(save_dir,glue("{cond}_Sigmas_hist.jpeg")),width=5,height=4)
      
      p <- mcmc_areas_ridges(posterior,pars = "(Intercept)",prob = .95)
      p
      ggsave(p,filename=path(save_dir,glue("{cond}_int_areas_ridges.jpeg")),width=5,height=4)
      p <- mcmc_areas_ridges(posterior,pars = c("rect","asp","aspsq"),prob = .95)+
        scale_y_discrete(labels=c(
          TeX("$\\beta_{r}$"),
          TeX("$\\beta_{a}$"),
          TeX("$\\beta_{a^2}$")))+
        labs(x="estimate",y="parameter")+
        scale_x_continuous(breaks=seq(-.25,.75,.25),limits=c(-.25,.75))+
        theme(text=element_text(size=15))
      p
      ggsave(p,filename=path(save_dir,glue("{cond}_coeffs_areas_ridges.jpeg")),width=5,height=4)
      p <- mcmc_areas_ridges(posterior,regex_pars = "Sigma",prob = .95)
      p
      ggsave(p,filename=path(save_dir,glue("{cond}_Sigmas_areas_ridges.jpeg")),width=5,height=4,dpi=500)
      
      yrep <- posterior_predict(m, draws=1000)
      p <- ppc_dens_overlay(d$circle,yrep)
      p
      ggsave(p,filename=path(save_dir,glue("{cond}_ppc.jpeg")),width=5,height=4,dpi=500)
    
      b0 <- as.data.frame(m)[,"(Intercept)"]
      b1 <- as.data.frame(m)[,"rect"]
      mm <- tibble(b0=b0,b1=b1)
      ggplot(d, aes(rect,circle))+
        geom_point(shape=".")+
        geom_abline(data=slice(mm,sample(1:nrow(mm),50,T)),
                    aes(intercept = b0,slope=b1),alpha=.05,col="red",linetype="dashed")+
        coord_fixed(xlim = c(-2.5,2),ylim = c(-2.5,2))+
        labs(x="rect log area (mean centered)",y="circle log area (mean centered)")+
        ggthemes::theme_few()
      ggsave(filename=path(save_dir,glue("{cond}_filler_scatter.jpeg")),
             width=4,height=4)
      jpeg(filename=path(save_dir,glue("{cond}_filler_pairs.jpeg")),
           width = 6,height=6,units = "in",res = 800)
      pairs(d[,c("rect","asp","aspsq")],pch=".")
      dev.off()
    }
  }
}

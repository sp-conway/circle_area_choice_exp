rm(list=ls())
library(tidyverse)
library(here)
library(glue)
library(mvtnorm)
library(fs)

which_model <- "sigma_constant"
N <- 20000
load_and_run_model <- function(N,cond,outl="no_outliers"){
  print(cond)
  dir <- here("analysis","bayes",which_model,cond,outl)
  load(path(dir,"mu_avgs.RData"))
  load(path(dir,"fit_summary.RData"))
  s <- fit_summary %>%
    filter(str_detect(variable,"s\\[")) %>%
    pull(mean)
  cors <- fit_summary %>%
    filter(str_detect(variable,"cor\\[")) %>%
    mutate(
      pair=case_when(
        str_detect(variable,"cor\\[1,2\\]")~"tc",
        str_detect(variable,"cor\\[1,3\\]")~"td",
        str_detect(variable,"cor\\[2,3\\]")~"cd"
      )
    ) %>%
    filter(!is.na(pair)) %>%
    select(pair,mean)
  a <- ( s %*% t(s) )
  cv <- matrix(c(1, cors$mean[1], cors$mean[2],
                   cors$mean[1], 1, cors$mean[3],
                   cors$mean[2], cors$mean[3], 1), nrow=3, ncol=3,byrow=T)*a
  dists <- unique(mu_avgs_fully_collapsed_w_hdis$distance)
  sim <- tibble()
  for(d in dists){
    print(d)
    mu_tmp <- mu_avgs_fully_collapsed_w_hdis %>%
      filter(distance==d) 
    mu_tmp1 <- c(mu_tmp %>%
            filter(stim=="t") %>%
            pull(m),
            mu_tmp %>%
            filter(stim=="c") %>%
            pull(m),
            mu_tmp %>%
              filter(stim=="d") %>%
              pull(m))
    x <- rmvnorm(N,mu_tmp1,cv)
    mx <- apply(x, 1, which.max)
    p <- c(sum(mx==1)/N,
           sum(mx==2)/N,
           sum(mx==3)/N)
    sim <- bind_rows(sim,
                     tibble(choice_tdc=c("target","competitor","decoy"),
                            disp_cond=cond,
                            distance=d,
                            p=p))
  }
  return(sim)
}

model_sims <- map(c("horizontal","triangle"),~load_and_run_model(N,.x)) %>%
  list_rbind()
model_sims %>%
  mutate(disp_cond=factor(disp_cond,levels=c("triangle","horizontal"))) %>%
  ggplot(aes(distance,p))+
  geom_point(aes(col=choice_tdc),size=2,alpha=.8)+
  geom_line(aes(col=choice_tdc),linewidth=1,alpha=.9)+
  ggsci::scale_color_startrek(name="stimulus")+
  scale_x_continuous(breaks=c(2,5,9,14),limits=c(1.5,14.5),labels=c("2%","5%","9%","14%"))+
  scale_y_continuous(limits=c(0,.6),breaks=seq(0,.6,.2))+
  labs(y="mean choice prop.",x="target-decoy distance")+
  facet_grid(.~disp_cond)+
  ggthemes::theme_few()+
  theme(text=element_text(size=28),
        legend.position = "top")
ggsave(filename=here("analysis","plots",glue("bayes_circle_area_sim_choice_{which_model}.jpeg")),width=12,height=6)

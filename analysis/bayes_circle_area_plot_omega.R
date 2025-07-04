rm(list=ls())
library(tidyverse)
library(here)
library(glue)
library(HDInterval)
library(latex2exp)
which_model <- "sigma_constant_target_effect"

get_omega <- function(cond,which_model){
  f <- here("analysis","bayes",which_model,cond,"no_outliers","cors.RData")
  load(f)
  o_m <- apply(cors, 3, mean)
  o_l <- apply(cors, 3, function(x) hdi(x)[1])
  o_u <- apply(cors, 3, function(x) hdi(x)[2])
  tibble(
    par=c("td","cd","tc"),
    m=o_m,
    lower=o_l,
    upper=o_u,
    disp_cond=cond
  )
}

omega <- map(c("triangle","horizontal"),get_omega, which_model) %>%
  list_rbind() %>%
  mutate(par=factor(par,levels=c("td","cd","tc")))
ggplot(omega,aes(m,par,col=disp_cond))+
  geom_point(shape=20)+
  geom_errorbar(aes(xmin=lower,xmax=upper),width=0)+
  # scale_x_continuous(limits=c(.5,.7),breaks=seq(.5,.7,.1))+
  scale_y_discrete(labels=c(
    TeX("$\\rho_{td}$"),
    TeX("$\\rho_{cd}$"),
    TeX("$\\rho_{tc}$")))+
  # facet_grid(.~disp_cond)+
  labs(x="estimate",y="parameter")+
  scale_color_manual(values=c("black","gray"),name="condition")+
  ggthemes::theme_few()+
  theme(legend.position="inside",legend.position.inside = c(0.8, 0.6),
        text=element_text(size=16))
ggsave(filename=here("analysis","plots",glue("bayes_circle_area_{which_model}_omega_plot.jpeg")),
       width=5,height=3,units = "in")

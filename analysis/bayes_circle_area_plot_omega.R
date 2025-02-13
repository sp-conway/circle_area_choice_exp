rm(list=ls())
library(tidyverse)
library(here)
library(HDInterval)
library(latex2exp)
f_h <- here("analysis","bayes","sigma_constant","horizontal","no_outliers","cors.RData")
f_t <- here("analysis","bayes","sigma_constant","triangle","no_outliers","cors.RData")

get_omega <- function(f,cond){
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

omega <- map2(c(f_h,f_t),c("horizontal","triangle"),get_omega) %>%
  list_rbind()
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
  theme(legend.position="inside",legend.position.inside = c(0.8, 0.37),
        text=element_text(size=12))
ggsave(filename=here("analysis","plots","bayes_circle_area_omega_plot.jpeg"),
       width=4,height=2,units = "in")

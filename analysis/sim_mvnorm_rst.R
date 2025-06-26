rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(matrixcalc)
library(latex2exp)
library(mvtnorm)

save_dir <- here("rst_sim")
dir_create(save_dir)
save_file <- here(save_dir,"rst_sim.RData")
if(!file_exists(save_file)){
  n <- 10000
  tc_cor <- dc_cor <- seq(-1,1,.005)
  td_cor <- seq(-1,1,.005)
  s <- c(1,1,1)
  a <- ( s %*% t(s) )
  mu <- c(1,1,.75)
  
  rst <- numeric(length(tc_cor)*length(td_cor))
  
  i <- 0
  for(diff_cor in tc_cor){
    for(same_cor in td_cor){
      i <- i + 1
      cat(i,"/",length(rst),"\n")
      tmp_cv <- matrix(c(1, diff_cor, same_cor,
                         diff_cor, 1, diff_cor,
                         same_cor, diff_cor, 1), nrow = 3, ncol = 3,byrow = T)*a
      if(is.positive.semi.definite(tmp_cv)){
        tmp_sim <- rmvnorm(n, mu, tmp_cv)
        tmp_max <- apply(tmp_sim, 1, which.max)
        rst[i] <- sum(tmp_max==1) / (sum(tmp_max==1)+sum(tmp_max==2))
      }else{
        rst[i] <- NA_real_
      }
    }
  }
  save(list=ls(),file=save_file)
}else{
  load(save_file)
}

td_tc_data <- crossing(
  tc_cor=tc_cor,
  td_cor=td_cor
) %>%
  mutate(rst=rst) 
td_tc_data %>%
  ggplot(aes(tc_cor,td_cor))+
  geom_raster(aes(fill = rst), interpolate=TRUE) +
  coord_fixed(xlim=c(-1,1),ylim=c(-1,1))+
  scale_fill_gradient2(low="navy", mid="white", high="red", 
                       midpoint=.5, limits=c(.4,.75),name="RST")+#range(short$pi0)) +
  labs(x=TeX(r"($\rho_{TC}$)"),
       y=TeX(r"($\rho_{TD}$)"))+
  ggthemes::theme_few()+
  theme(text=element_text(size=15))
ggsave(filename=path(save_dir,"rst_sim_td_tc.jpg"),width=5,height=5,dpi=500)


td_dc_data <- crossing(
  tc_cor=tc_cor,
  cd_cor=dc_cor
) %>%
  mutate(rst=rst) 
td_dc_data %>%
  ggplot(aes(tc_cor,cd_cor))+
  geom_raster(aes(fill = rst), interpolate=TRUE) +
  coord_fixed(xlim=c(-1,1),ylim=c(-1,1))+
  scale_fill_gradient2(low="navy", mid="white", high="red", 
                       midpoint=.5, limits=c(.4,.75),name="RST")+#range(short$pi0)) +
  labs(x=TeX(r"($\rho_{Target/Competitor}$)"),
       y=TeX(r"($\rho_{Competitor/Decoy}$)"))+
  ggthemes::theme_few()+
  theme(text=element_text(size=15))
ggsave(filename=path(save_dir,"rst_sim_td_dc.jpg"),width=5,height=5,dpi=500)


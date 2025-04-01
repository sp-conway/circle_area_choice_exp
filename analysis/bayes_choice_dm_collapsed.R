# bayesian modeling of choice data
# using multinomial dirichlet model
# fully collapsing choice data
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
which_model <- "bayes_choice_dm_collapsed"
which_cond <- "horizontal"

# sampler settings
n_chain <- n_core <- 4
n_iter <- 1000

# set path to cmdstan
set_cmdstan_path(here("cmdstan-2.36.0"))

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
  group_by(distance,choice_tdc) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  pivot_wider(names_from = choice_tdc, values_from = n)
d_counts <- as.matrix(d1[,c("target","competitor","decoy")])
stan_data <- list(
  D=4,
  K=3,
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
mcmc_trace(draws,regex_pars = "alpha")
mcmc_trace(draws,regex_pars = "theta\\[1")
mcmc_trace(draws,regex_pars = "theta\\[2")
mcmc_trace(draws,regex_pars = "theta\\[3")
mcmc_trace(draws,regex_pars = "theta\\[4")

fit_summary <- summarise_draws(draws, mean,~quantile(.x, probs = c(.025, .975))) %>%
  filter(str_detect(variable,"theta")) %>%
  rename(hdi_lower=`2.5%`,
         hdi_upper=`97.5%`,
         m=mean)
model_preds <- fit_summary %>%
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
  pivot_longer(-distance,names_to = "choice",values_to = "n") %>%
  group_by(distance) %>%
  mutate(m=n/sum(n)) %>%
  ungroup() %>%
  mutate(source="data") %>%
  dplyr::select(-n) %>%
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

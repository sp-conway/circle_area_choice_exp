library(MCMCpack)
N <- 100
alpha <- c(1,1,1)
sample_dirichlet <- function(N, alpha){
  X <- rdirichlet(N,alpha)
  tibble(
    A=X[,1],
    B=X[,2],
    C=X[,3]
  ) %>%
    pivot_longer(everything())
}

D <- sample_dirichlet(N,alpha)

plot_dirichlet <- function(samps,alpha){
  samps %>%
    ggplot(aes(value,fill=name))+
    geom_histogram(alpha=.5,position="identity")+
    scale_x_continuous(limits=c(0,1))+
    labs(title=paste0(alpha,collapse = ","))+
    ggthemes::theme_few()+
    theme(plot.title=element_text(hjust=0.5))
}

plot_dirichlet(D,alpha)

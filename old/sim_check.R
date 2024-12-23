rm(list=ls())
library(mvtnorm)
N <- 10000
tc_cor_hor <- cd_cor_hor <- tc_cor_tri <- cd_cor_tri <- .55
td_cor_hor <- .64
td_cor_tri <- .61
mu <- c(1, 1, .75)
s <- c(1,1,1)
a <- ( s %*% t(s) )
cv_hor <- matrix(c(1, tc_cor_hor, td_cor_hor,
                   tc_cor_hor, 1, cd_cor_hor,
                   td_cor_hor, cd_cor_hor, 1), nrow = 3, ncol = 3,byrow = T)*a
cv_tri <- matrix(c(1, tc_cor_tri, td_cor_tri,
                   tc_cor_tri, 1, cd_cor_tri,
                   td_cor_tri, cd_cor_tri, 1), nrow = 3, ncol = 3,byrow = T)*a

do_sample <- function(N, mu, cv){
  s <- rmvnorm(N, mu, cv)
  ch <- as.character(apply(s, 1, which.max))
  ch <- stringr::str_replace_all(ch, c("1"="t","2"="c","3"="c"))
}
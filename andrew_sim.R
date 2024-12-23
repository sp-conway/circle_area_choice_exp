which_td_dist <- 1 # 1=low, 2=med, 3=high

sd_global <- 0.11 # global sd (moves whole trial)
mu_t_c <- 0.00 # mean of t and c
sd_t_c_d <- 0.10 # sd for t,c,d indep
if (which_td_dist==1) {
  # low td dist
  sd_td_shape <- 0.20 # shape of td difference gamma
  sd_td_rate <- 10.00 # rate of td difference gamma
  prob_disc_corr <- 0.15 # proportion dominance noticed correctly (t>d)
} else if (which_td_dist==2) {
  # med td dist
  sd_td_shape <- 0.60 # shape of td difference gamma
  sd_td_rate <- 10.00 # rate of td difference gamma
  prob_disc_corr <- 0.45 # proportion dominance noticed correctly (t>d)
} else {
  # high td dist
  sd_td_shape <- 1.00 # shape of td difference gamma
  sd_td_rate <- 10.00 # rate of td difference gamma
  prob_disc_corr <- 0.85 # proportion dominance noticed correctly (t>d)
}
# mu_d <- mu_t_c # if they don't notice dominance, d mean assumed the
#same as t and c
mu_d <- mu_t_c - sd_td_shape/sd_td_rate # if they don't notice
#dominance, d mean assumed lower than t and c (indep perception;
                                             # shape/rate = mean of gamma)
prob_disc_err <- 0.00 # proportion dominance noticed incorrectly (d>t)

n <- 10000

d <- data.frame(t=NULL, c=NULL, d=NULL)
for (i in 1:n) {
  
  d[i,'t'] <- rnorm(1, mu_t_c, sd_t_c_d)
  d[i,'c'] <- rnorm(1, mu_t_c, sd_t_c_d)
  
  if (runif(1) < prob_disc_corr) {
    # notice correct dominance
    d[i,'d'] <- d[i,'t']-rgamma(1,shape=sd_td_shape,rate=sd_td_rate)
  } else if (runif(1) < prob_disc_err) {
    # notice incorrect dominance
    d[i,'d'] <- d[i,'t']+rgamma(1,shape=sd_td_shape,rate=sd_td_rate)
  } else {
    # don't notice dominance
    d[i,'d'] <- rnorm(1,mu_d,sd_t_c_d)
  }
  
  # global higher/lower on this trial
  d[i,] <- d[i,] + rnorm(1,0,sd_global)
}

# plot
par(mfrow=c(1,1), pty='s')

upper_fcn=function(x,y,...) {
  points(x,y,...)
  abline(a=0,b=1,col='gray',lty=2)
}  

lower_fcn=function(x,y,...) {
  r <- cor(x,y)
  txt <- paste0('r=',format(c(r, 0.123456789), digits = 2)[1])
  text(0,0,txt,cex=2)
}

pairs(d,
      lower.panel=lower_fcn, upper.panel=upper_fcn,
      labels=c('T', 'C', 'D'),
      xlim=c(-1,1), ylim=c(-1,1),
      at=c(-1, 0, 1),
      cex=.1, cex.axis=1.5)
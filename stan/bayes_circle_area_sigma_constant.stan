data {
  // scalars
  int<lower=0> n_trials;
  int<lower=0> n_subs;
  int<lower=0> n_trials_unique;
  
  // vector of all subject ids for indexing of random effects
  array[n_trials] int sub_n_new;
  array[n_trials] int sub_n_new_unique;
  
  // target wide=1, tall=0
  vector[n_trials] wt;
  
  // competitor wide=1, tall=0
  vector[n_trials] wc;
  
  // decoy wide=1, tall=0
  vector[n_trials] wd;
  
  // middle diag =1, else=0
  vector[n_trials] diag2;
  
  // upper diag=1, else=0
  vector[n_trials] diag3;
  
  // if distance = 5, 1 else 0
  vector[n_trials] dist5;
  
  // if distance = 9, 1 else 0
  vector[n_trials] dist9;
  
  // if distance = 14, 1 else 0
  vector[n_trials] dist14;
  
  // if DECOY distance = 2, 1 else 0
  vector[n_trials] dist2d;
  
  // trial-by-trial matrix of Target-Competitor-Decoy log area centered values
  matrix[n_trials,3] la_cent;
  
  // target wide=1, tall=0
  vector[n_trials_unique] wt;
  
  // competitor wide=1, tall=0
  vector[n_trials_unique] wc;
  
  // decoy wide=1, tall=0
  vector[n_trials_unique] wd;
  
  // middle diag =1, else=0
  vector[n_trials_unique] diag2;
  
  // upper diag=1, else=0
  vector[n_trials_unique] diag3;
  
  // if distance = 5, 1 else 0
  vector[n_trials_unique] dist5;
  
  // if distance = 9, 1 else 0
  vector[n_trials_unique] dist9;
  
  // if distance = 14, 1 else 0
  vector[n_trials_unique] dist14;
  
  // if DECOY distance = 2, 1 else 0
  vector[n_trials_unique] dist2d;
  
}

parameters {
  // fixed intercept
  real b0;
  
  // fixed effect of wide
  real bw;
  
  // fixed effect of middle diagonal
  real bdiag2;
  
  // fixed effect of upper diagonal
  real bdiag3;
  
  // fixed effect of distance=5
  real bdist5;
  
  // fixed effect of distance=9
  real bdist9;
  
  // fixed effect of distance=14
  real bdist14;
  
  // fixed effect of decoy distance=2
  real bdist2d;
  
  // fixed effect of decoy distance=5
  real bdist5d;
  
  // fixed effect of decoy distance=9
  real bdist9d;
  
  // fixed effect of decoy distance=14
  real bdist14d;
  
  // random subject-level intercepts
  vector[n_subs] b0_s;
  
  // Std. Dev. of random-subject level intercepts
  real <lower=0>sigma_b0_s;
  
  // correlation mat in chol. form
  cholesky_factor_corr[3] omega;
  
  // variances
  vector<lower=0>[3] s;
}

transformed parameters{
  // Covariance matrix
  matrix[3,3] S = diag_pre_multiply(s,omega);
  matrix[n_trials,3] mu;
  mu[,1] = (b0+b0_s[sub_n_new]) + (bw*wt) + (bdiag2*diag2) + (bdiag3*diag3) + (bdist5*dist5) + (bdist9*dist9) + (bdist14*dist14);
  mu[,2] = (b0+b0_s[sub_n_new]) + (bw*wc) + (bdiag2*diag2) + (bdiag3*diag3) + (bdist5*dist5) + (bdist9*dist9) + (bdist14*dist14);
  mu[,3] = (b0+b0_s[sub_n_new]) + (bw*wd) + (bdiag2*diag2) + (bdiag3*diag3) + (bdist2d*dist2d) + ( (bdist5+bdist5d)*dist5 ) + ( (bdist9+bdist9d)*dist9 ) + ( (bdist14+bdist14d)*dist14 );
  
}


model {
  // fixed effects
  b0 ~ normal(0,5);
  bw ~ normal(0,5);
  bdiag2 ~ normal(0,5);
  bdiag3 ~ normal(0,5);
  bdist5 ~ normal(0,5);
  bdist9 ~ normal(0,5);
  bdist14 ~ normal(0,5);
  bdist2d ~ normal(0,5);
  bdist5d ~ normal(0,5);
  bdist9d ~ normal(0,5);
  bdist14d ~ normal(0,5);
  
  // random effects
  sigma_b0_s ~ cauchy(0,2.5);
  b0_s ~ normal(0,sigma_b0_s);
  
  // correlation
  omega ~ lkj_corr_cholesky(1);
  
  // standard deviations
  s ~ cauchy(0,2.5);
  
  // ACTUAL DATA
  for(i in 1:n_trials){
    la_cent[i,] ~ multi_normal_cholesky(mu[i,],S);
  }
}

generated quantities{
  // correlation matrix
  matrix [3,3] cor=multiply_lower_tri_self_transpose(omega);
  matrix[n_trials_unique,3] mu_unique;
  mu_unique[,1] = (b0+b0_s[sub_n_new_unique]) + (bw*wt_unique) + (bdiag2*diag2_unique) + (bdiag3*diag3_unique) + (bdist5*dist5_unique) + (bdist9*dist9_unique) + (bdist14*dist14_unique);
  mu_unique[,2] = (b0+b0_s[sub_n_new_unique]) + (bw*wc_unique) + (bdiag2*diag2_unique) + (bdiag3*diag3_unique) + (bdist5*dist5_unique) + (bdist9*dist9_unique) + (bdist14*dist14_unique);
  mu_unique[,3] = (b0+b0_s[sub_n_new_unique]) + (bw*wd_unique) + (bdiag2*diag2_unique) + (bdiag3*diag3_unique) + (bdist2d*dist2d_unique) + ( (bdist5+bdist5d)*dist5_unique ) + ( (bdist9+bdist9d)*dist9_unique ) + ( (bdist14+bdist14d)*dist14_unique );
  
  array[n_trials_unique] matrix[N,3] samp;
  int N=2000;
  matrix[n_trials_unique,N] pred_max;
  real max_value;
  for(i in 1:n_trials_unique){
    for(j in 1:N){
      samp[i]=multi_normal_cholesky_rng(mu_unique[i,],S);
      real max_value = -900000;
      max_index=0;
      for(k in 1:3){
        if(samp[i][j,k]>max_value){
          max_value=samp[i][j,k];
          max_index=k
        }
      }
      pred_max[i,j] = max_index;
    }
  }
}

// dirichlet-multinomial model, collapsed over set
// fitting by subject
data {
  int<lower=0> S; // N subjects
  int<lower=0> O; // N options
  int<lower=0> D; // N distances
  array[S,D,O] int d_counts;
}

parameters{
  array[D] vector<lower=0> [O] alpha_mu;
  array[D] vector<lower=0> [O] alpha_sigma;
  array[S,D] vector<lower=0> [O] alpha;
  array[S,D] simplex[O] theta;
}


model {
  for(d in 1:D){
    for(o in 1:O){
      alpha_mu[d][o] ~ gamma(5,.01);
      alpha_sigma[d][o] ~ gamma(5,.01);
    }
  }
  for(s in 1:S){
    for(d in 1:D){
      for(o in 1:O){
        alpha[s,d][o] ~ normal(alpha_mu[d][o],alpha_sigma[d][o]);
      }
      theta[s,d] ~ dirichlet(alpha[s,d]);
      d_counts[s,d,] ~ multinomial(theta[s,d]);
    }
  }
}

generated quantities{
  matrix[D,O] p_m;
  for(d in 1:D){
    for(o in 1:O){
      real sum_theta=0;
      for(s in 1:S){
        sum_theta += theta[s,d][o];
      }
      p_m[d,o]=sum_theta/S;
    }
  }
}


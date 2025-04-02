data {
  int<lower=0> S; // N subjects
  int<lower=0> O; // N options
  int<lower=0> D; // N distances
  array[S,D,O] int d_counts;
}

parameters{
  array[D] vector<lower=0> [O] alpha_g;
  array[S,D] vector<lower=0> [O] alpha_p;
  vector<lower=0,upper=1>[S] omega;
  array[S,D] simplex[O] theta;
}

transformed parameters{
  array[S,D] vector<lower=0> [O] alpha_w;
  for(s in 1:S){
    for(d in 1:D){
      for(o in 1:O){
        alpha_w[s,d][o] = omega[s]*alpha_g[d][o] + (1-omega[s])*alpha_p[s,d][o];
      }
    }
  }
}

model {
  for(d in 1:D){
    for(o in 1:O){
      alpha_g[d][o] ~ lognormal(1,1);
    }
  }
  for(s in 1:S){
    for(d in 1:D){
      for(o in 1:O){
        alpha_p[s,d][o] ~ lognormal(1,1);
      }
      theta[s,d] ~ dirichlet(alpha_w[s,d]);
      d_counts[s,d,] ~ multinomial(theta[s,d]);
    }
    omega[s] ~ beta(1,1);
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


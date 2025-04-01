// dirichlet-multinomial model, fully collapsed 
data {
  int<lower=0> K; // N options
  int<lower=0> D; // N distances
  array[D,K] int d_counts;
}

parameters {
  array[D] vector<lower=0> [K] alpha;
  array[D] simplex[K] theta;
}


model {
  for(i in 1:D){
    alpha[i] ~ lognormal(1,1);
    theta[i] ~ dirichlet(alpha[i]);
    d_counts[i,]~multinomial(theta[i]);
  }
}


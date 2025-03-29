data {
  int<lower=0> N; // n trials
  int<lower=0> J; // N subjects
  array[N,3] int choice_counts;
  array[N] int sub_ns_new;
  array[N] int distance5;
  array[N] int distance9;
  array[N] int distance14;
  array[N] int tw;
  array[N] int cw;
  array[N] int dw;
}

parameters {
  real b_0; 
  real b_comp;
  real b_target;
  real b_distance5;
  real b_distance9;
  real b_distance14;
  
  real b_comp_distance5;
  real b_comp_distance9;
  real b_comp_distance14;
  
  real b_target_distance5;
  real b_target_distance9;
  real b_target_distance14;
  real b_w;
  real<lower=0.00001> b_0_s_sigma;
  real<lower=0.00001> b_w_s_sigma;
  real<lower=0.00001> b_comp_s_sigma;
  real<lower=0.00001> b_target_s_sigma;
  real<lower=0.00001> b_distance_s_sigma;
  real<lower=0.00001> b_targetcomp_distance_intxn_s_sigma;

  vector[J] b_0_s;
  vector[J] b_comp_s;
  vector[J] b_target_s;
  vector[J] b_distance5_s;
  vector[J] b_distance9_s;
  vector[J] b_distance14_s;
  vector[J] b_w_s;
  
  vector[J] b_comp_distance5_s;
  vector[J] b_comp_distance9_s;
  vector[J] b_comp_distance14_s;
  
  vector[J] b_target_distance5_s;
  vector[J] b_target_distance9_s;
  vector[J] b_target_distance14_s;
}

transformed parameters{
  matrix[N,3] p;
  for(i in 1:N){
    p[i,]=to_row_vector(softmax(
    to_vector([
      (b_0+b_0_s[sub_ns_new[i]])+
      (b_distance5+b_distance5_s[sub_ns_new[i]])*distance5[i]+
      (b_distance9+b_distance9_s[sub_ns_new[i]])*distance9[i]+
      (b_distance14+b_distance14_s[sub_ns_new[i]])*distance14[i]+
      (b_w+b_w_s[sub_ns_new[i]])*tw[i]+
      (b_target_distance5+b_target_distance5_s[sub_ns_new[i]])*distance5[i]+
      (b_target_distance9+b_target_distance9_s[sub_ns_new[i]])*distance9[i]+
      (b_target_distance14+b_target_distance14_s[sub_ns_new[i]])*distance14[i]+
      (b_target+b_target_s[sub_ns_new[i]]),
      
      (b_0+b_0_s[sub_ns_new[i]])+
      (b_distance5+b_distance5_s[sub_ns_new[i]])*distance5[i]+
      (b_distance9+b_distance9_s[sub_ns_new[i]])*distance9[i]+
      (b_distance14+b_distance14_s[sub_ns_new[i]])*distance14[i]+
      (b_w+b_w_s[sub_ns_new[i]])*cw[i]+
      (b_comp_distance5+b_comp_distance5_s[sub_ns_new[i]])*distance5[i]+
      (b_comp_distance9+b_comp_distance9_s[sub_ns_new[i]])*distance9[i]+
      (b_comp_distance14+b_comp_distance14_s[sub_ns_new[i]])*distance14[i]+
      (b_comp+b_comp_s[sub_ns_new[i]]),
      
      (b_0+b_0_s[sub_ns_new[i]])+
      (b_w+b_w_s[sub_ns_new[i]])*dw[i]
      ])));
  }
}

model {
  b_0~normal(0,5); 
  b_comp~normal(0,5);
  b_target~normal(0,5);
  b_distance5~normal(0,5);
  b_distance9~normal(0,5);
  b_distance14~normal(0,5);
  
  b_comp_distance5~normal(0,5);
  b_comp_distance9~normal(0,5);
  b_comp_distance14~normal(0,5);
  
  b_target_distance5~normal(0,5);
  b_target_distance9~normal(0,5);
  b_target_distance14~normal(0,5);
  b_w~normal(0,5);
  
  b_0_s_sigma~cauchy(0,.25);
  b_w_s_sigma~cauchy(0,2.5);
  b_comp_s_sigma~cauchy(0,2.5);
  b_target_s_sigma~cauchy(0,2.5);
  b_distance_s_sigma~cauchy(0,2.5);
  b_targetcomp_distance_intxn_s_sigma~cauchy(0,.25);

  b_0_s~normal(0,b_0_s_sigma);
  b_comp_s~normal(0,b_comp_s_sigma);
  b_target_s~normal(0,b_target_s_sigma);
  b_distance5_s~normal(0,b_distance_s_sigma);
  b_distance9_s~normal(0,b_distance_s_sigma);
  b_distance14_s~normal(0,b_distance_s_sigma);
  b_w_s~normal(0,b_w_s_sigma);
  
  b_comp_distance5_s~normal(0, b_targetcomp_distance_intxn_s_sigma);
  b_comp_distance9_s~normal(0, b_targetcomp_distance_intxn_s_sigma);
  b_comp_distance14_s~normal(0, b_targetcomp_distance_intxn_s_sigma);
  
  b_target_distance5_s~normal(0, b_targetcomp_distance_intxn_s_sigma);
  b_target_distance9_s~normal(0, b_targetcomp_distance_intxn_s_sigma);
  b_target_distance14_s~normal(0, b_targetcomp_distance_intxn_s_sigma);
  for(i in 1:N){
    choice_counts[i,] ~ multinomial(to_vector(p[i,]));
  }
}

generated quantities{
  matrix[N,3] choice_counts_rep;
  for(i in 1:N){
    choice_counts_rep[i,]=to_row_vector(multinomial_rng(to_vector(p[i,]),sum(choice_counts[i,])));
  }
}


// A Poisson regression with effect of rodent_at_house, wet season, and 
// varying intercepts by site, visit, and house, to be used with 
// house-level data

data {
  
  int<lower=0> N;
  
  array[N] int n_Mna;
  array[N] int rodent_at_house;
  array[N] int wet_season;
  array[N] real log_tot_traps;
  
  int<lower=0> N_site;
  array[N] int<lower=1, upper=N_site> site;
  
  int<lower=0> N_visit;
  array[N] int<lower=1, upper=N_visit> visit;
  
  int<lower=0> N_house;
  array[N] int<lower=1, upper=N_house> house;
}

parameters {
  
  real a; // intercept
  real bR; // rodent effect
  real bW; // wet season effect
  
  // varying intercepts by site (centered parameterization)
  vector[N_site] a_site_z;
  real<lower=0> sigma_site;
  
  // varying intercepts by visit (centered parameterization)
  vector[N_visit] a_visit_z;
  real<lower=0> sigma_visit;
  
  // varying intercepts by house (centered parameterization)
  vector[N_house] a_house_z;
  real<lower=0> sigma_house;
}

model {
  
  vector[N_site] a_site;
  vector[N_visit] a_visit;
  vector[N_house] a_house;
  vector[N] lambda;
  
  a ~ normal(-3.1, 1.1);
  bR ~ normal(0, 1);
  bW ~ normal(0, 1);
  
  a_site_z ~ normal(0, 1);
  sigma_site ~ exponential(1);
  
  a_visit_z ~ normal(0, 1);
  sigma_visit ~ exponential(1);
  
  a_house_z ~ normal(0, 1);
  sigma_house ~ exponential(1);
  
  for(i in 1:N_site) {
    
    a_site[i] = a_site_z[i] * sigma_site;
  }
  
  for(i in 1:N_visit) {
    
    a_visit[i] = a_visit_z[i] * sigma_visit;
  }
  
  for(i in 1:N_house) {
    
    a_house[i] = a_house_z[i] * sigma_house;
  }
  
  for (i in 1:N) {
    
    lambda[i] = a + bR * rodent_at_house[i] + bW * wet_season[i] + 
    a_site[site[i]] + a_visit[visit[i]] + a_house[house[i]] +
    log_tot_traps[i];
    n_Mna[i] ~ poisson_log(lambda[i]);
  }
}

generated quantities {
  
  vector[N_site] a_site;
  vector[N_visit] a_visit;
  vector[N_house] a_house;
  vector[N] lambda;
  
  for(i in 1:N_site) {
    
    a_site[i] = a_site_z[i] * sigma_site;
  }
  
  for(i in 1:N_visit) {
    
    a_visit[i] = a_visit_z[i] * sigma_visit;
  }
  
  for(i in 1:N_house) {
    
    a_house[i] = a_house_z[i] * sigma_house;
  }

  for (i in 1:N) {
    
    lambda[i] = a + bR * rodent_at_house[i] + bW * wet_season[i] +
    a_site[site[i]] + a_visit[visit[i]] + a_house[house[i]];
  }
}

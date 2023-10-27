// A Poisson regression with effect of Rra_at_site, wet season, and 
// varying intercepts by site, to be used with visit-level data

data {
  
  int<lower=0> N;
  
  array[N] int n_Mna;
  array[N] int Rra_at_site;
  array[N] int wet_season;
  array[N] real log_tot_traps;
  
  int<lower=0> N_site;
  array[N] int<lower=1, upper=N_site> site;
}

parameters {
  
  real a; // intercept
  real bR; // Rattus effect
  real bW; // wet season effect
  
  // varying intercepts by site
  vector[N_site] a_site;
  real<lower=0> sigma_site;
}

model {
  
  vector[N] lambda;
  
  a ~ normal(-3.1, 1.1);
  bR ~ normal(0, 1);
  bW ~ normal(0, 1);
  
  a_site ~ normal(0, sigma_site);
  sigma_site ~ exponential(1);
  
  for (i in 1:N) {
    
    lambda[i] = a + bR * Rra_at_site[i] + bW * wet_season[i] + a_site[site[i]] + 
    log_tot_traps[i];
    n_Mna[i] ~ poisson_log(lambda[i]);
  }
}

generated quantities {
  
  vector[N] lambda;
  
  for (i in 1:N) {
    
    lambda[i] = a + bR * Rra_at_site[i] + bW * wet_season[i] + a_site[site[i]];
  }
}

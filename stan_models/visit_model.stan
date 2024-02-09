// A Poisson regression with effect of rodent_at_site, wet season, and 
// varying intercepts by site and visit, to be used with visit-level data

data {
  
  int<lower=0> N;
  
  array[N] int n_Mna;
  array[N] int rodent_at_site;
  array[N] int wet_season;
  array[N] real log_tot_traps;
  
  int<lower=0> N_site;
  array[N] int<lower=1, upper=N_site> site;
  
  int<lower=0> N_visit;
  array[N] int<lower=1, upper=N_visit> visit;
}

parameters {
  
  real a; // intercept
  real bR; // rodent presence effect
  real bW; // wet season effect
  
  // varying intercepts by site
  vector[N_site] a_site;
  real<lower=0> sigma_site;
  
  // varying intercepts by visit
  vector[N_visit] a_visit;
  real<lower=0> sigma_visit;
}

model {
  
  vector[N] lambda;
  
  a ~ normal(-3.1, 1.1);
  bR ~ normal(0, 1);
  bW ~ normal(0, 1);
  
  a_site ~ normal(0, sigma_site);
  sigma_site ~ exponential(1);
  
  a_visit ~ normal(0, sigma_visit);
  sigma_visit ~ exponential(1);
  
  for (i in 1:N) {
    
    lambda[i] = a + bR * rodent_at_site[i] + bW * wet_season[i] + 
    a_site[site[i]] + a_visit[visit[i]] +
    log_tot_traps[i];
    n_Mna[i] ~ poisson_log(lambda[i]);
  }
}

generated quantities {
  
  vector[N] lambda;
  
  for (i in 1:N) {
    
    lambda[i] = a + bR * rodent_at_site[i] + bW * wet_season[i] + 
    a_site[site[i]] + a_visit[visit[i]];
  }
}

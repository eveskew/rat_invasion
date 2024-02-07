// A Poisson regression with effect of Pro_at_site, wet season, and 
// varying intercepts by site, to be used as an index of spillover risk with
// visit-level data

data {
  
  int<lower=0> N;
  
  array[N] int n_Mna_pos_lassa;
  array[N] int Pro_at_site;
  array[N] int wet_season;
  array[N] real log_tot_traps;
  
  int<lower=0> N_site;
  array[N] int<lower=1, upper=N_site> site;
}

parameters {
  
  real a; // intercept
  real bP; // Pro effect
  real bW; // wet season effect
  
  // varying intercepts by site (centered parameterization)
  vector[N_site] a_site_z;
  real<lower=0> sigma_site;
}

model {
  
  vector[N_site] a_site;
  vector[N] lambda;
  
  a ~ normal(-3.1, 1.1);
  bP ~ normal(0, 1);
  bW ~ normal(0, 1);
  
  a_site_z ~ normal(0, 1);
  sigma_site ~ exponential(1);
  
  for(i in 1:N_site) {
    
    a_site[i] = a_site_z[i] * sigma_site;
  }
  
  for (i in 1:N) {
    
    lambda[i] = a + bP * Pro_at_site[i] + bW * wet_season[i] + a_site[site[i]] + 
    log_tot_traps[i];
    n_Mna_pos_lassa[i] ~ poisson_log(lambda[i]);
  }
}

generated quantities {
  
  vector[N_site] a_site;
  vector[N] lambda;
  
  for(i in 1:N_site) {
    
    a_site[i] = a_site_z[i] * sigma_site;
  }
  
  for (i in 1:N) {
    
    lambda[i] = a + bP * Pro_at_site[i] + bW * wet_season[i] + a_site[site[i]];
  }
}

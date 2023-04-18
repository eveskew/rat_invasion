// An intercept-only Poisson regression for comparison to the other
// site-level models

data {
  
  int<lower=0> N;
  
  array[N] int n_Mna;
  array[N] real log_tot_traps;
}

parameters {
  
  real a; // intercept
}

model {
  
  vector[N] lambda;
  
  a ~ normal(-2, 1);
  
  for (i in 1:N) {
    
    lambda[i] = a + log_tot_traps[i];
    n_Mna[i] ~ poisson_log(lambda[i]);
  }
}

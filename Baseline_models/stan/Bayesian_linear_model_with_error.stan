  data {
    int<lower = 0> N;
    vector[N] Temperature;
    vector[N] Temperature_true;
    vector[N] Temperature_error;
    vector[N] D47;
    vector[N] D47_true;
    vector[N] D47_error;
  }

  parameters {
    real alpha;
    real beta;
    real sigma;
  }
  
  model {
    D47 ~ normal(D47_true, D47_error);
    Temperature ~ normal(Temperature_true, Temperature_error);
    Temperature_true ~ normal(0, 0.001);
    alpha ~ normal(0.231, 0.065);
    beta ~ normal(0.039, 0.004);
    sigma ~ gamma(0.001, 0.001);
    D47 ~ normal(alpha + beta * Temperature, sigma);
  }
  
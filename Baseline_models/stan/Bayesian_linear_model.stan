  data {
    int<lower = 0> N;
    vector[N] Temperature;
    vector[N] D47;
  }

  parameters {
    real alpha;
    real beta;
    real sigma;
  }
  
  model {
    alpha ~ normal(0.231, 0.065);
    beta ~ normal(0.039, 0.004);
    sigma ~ gamma(0.001, 0.001);
    D47 ~ normal(alpha + beta * Temperature, sigma);
  }
  
  data {
    int<lower = 0> N;
    vector[N] Temperature;
    vector[N] Temperature_true;
    vector[N] Temperature_error;
    vector[N] D47;
    vector[N] D47_true;
    vector[N] D47_error;
    vector[N] y_new;
  }

  parameters {
    real alpha;
    real beta;
    real sigma;
  }

  model {
    D47 ~ normal(D47_true, D47_error);
    D47_true ~ normal(alpha + beta * Temperature, sigma);
    Temperature ~ normal(Temperature_true, Temperature_error);
    Temperature_true ~ normal(0, 0.001);
    alpha ~ normal(0.231, 0.065);
    beta ~ normal(0.039, 0.004);
    sigma ~ gamma(0.001, 0.001);
  }
  
  generated quantities {
    vector[N] Temperature_new;
    for (n in 1:N)
      Temperature_new[n] = normal_rng((y_new[n] - alpha) * inv(beta)), sigma);
  }


calCoef <- function(data, seed, replicate){
  OLS_result <- fit_model(seed, OLS_boot, data, D47~Temperature, replicate)
  york_result <- fit_model(seed, york_boot, data, D47~Temperature, replicate)
  deming_result <- fit_model(seed, deming_boot, data, D47~Temperature, replicate)
  bayesian_resule <- Bayesian_fit(data, seed)
  LSMC_result <- LSMC_fit(data, seed)
  ODR_result <- fit_model(seed, ODR_boot, data, D47~Temperature, replicate)
  QR_result <- fit_model(seed, QR_boot, data, D47~Temperature, replicate)
  rlm_result <- fit_model(seed, rlm_boot, data, D47~Temperature, replicate)
  TS_result <- fit_model(seed, TS_boot, data, D47~Temperature, replicate)
  
  return(list(OLS = OLS_result, 
              york = york_result, 
              deming = deming_result,
              bayesian = bayesian_resule, 
              LSMC = LSMC_result, 
              ODR = ODR_result, 
              QR = QR_result, 
              RLM = rlm_result, 
              TS = TS_result))
}

# helper functions
OLS_boot <- function(formula, data, indices){
  d <- data[indices, ]
  OLS <- lm(formula, d)
  return (c(coef(OLS)))
}

york_boot <- function(formula, data, indices){
  d <- data[indices, ]
  york_data <- cbind(d$Temperature, d$TempError, d$D47, d$D47error)
  fit_model <- york(york_data)
  return (c(fit_model$a['a'], fit_model$b['b']))
}

deming_boot <- function(formula, data, indices){
  d <- data[indices, ]
  fit_model <- deming(formula, data=d, xstd=abs(TempError), ystd=abs(D47error)) 
  return (c(fit_model$coefficients[1], fit_model$coefficients[2]))
}

ODR_boot <- function(formula, data, indices){
  d <- data[indices, ]
  fit_model <- odregress(d$Temperature, d$D47)
  return (c(fit_model$coef[2], fit_model$coef[1]))
}

QR_boot <- function(formula, data, indices){
  d <- data[indices, ]
  fit_model <- rq(formula, data=d)
  return (c(coef(fit_model)))
}

rlm_boot <- function(formula, data, indices){
  d <- data[indices, ]
  fit_model <- rlm(formula, data=d)
  return (c(coef(fit_model)))
}

TS_boot <- function(formula, data, indices){
  d <- data[indices, ]
  fit_model <- theilsen(formula, data=d)
  return (c(coef(fit_model)))
}

LSMC_fit <- function(data, seed){
  set.seed(seed)
  N <- nrow(data)           #Sample Size
  M <- 1000                 #Number of experiments/iterations
  
  ## Storage
  intercept_DT <- rep(0, M)
  slope_DT <- rep(0, M)
  
  ## begin Monte Carlo
  for (i in 1:M) {
    U_i = rnorm(N, 0, sqrt(abs(data$D47error)))
    x_i = rnorm(N, data$x_TRUE, sqrt(abs(data$TempError)))
    # Y_i = alpha_TRUE + beta_TRUE * x_i + U_i
    Y_i = rnorm(N, data$y_TRUE, sqrt(abs(data$D47error)))
    
    # Formulate data.table
    data_i = data.table(Y = Y_i, X = x_i)
    
    # Run regressions
    ols_i <- lm(data = data_i, Y ~ X)
    
    
    # Extract coefficient and save
    slope_DT[i] <- coef(ols_i)[2]
    intercept_DT[i] <- coef(ols_i)[1]
  }
  
  return (cbind.data.frame(alpha = intercept_DT, 
                           beta = slope_DT))
}

Bayesian_fit <- function(data, seed){
  rstan_options(auto_write = TRUE)
  stan_date <- list(N = nrow(data), 
                    Temperature = data$Temperature, 
                    Temperature_true = data$x_TRUE,
                    Temperature_error = abs(data$TempError),
                    D47 = data$D47,
                    D47_true = data$y_TRUE,
                    D47_error = abs(data$D47error))
  
  Bayesian_model <- stan(file = "./Baseline_models/stan/Bayesian_linear_model_with_error.stan", data = stan_date, iter = 2000, chains = 4, seed = seed)
  
  Bayesian_result <- rstan::extract(Bayesian_model)
  
  return(cbind.data.frame(alpha = Bayesian_result$alpha, 
                          beta = Bayesian_result$beta,
                          tau = Bayesian_result$sigma))
}

fit_model <- function(seed, statistic, dataSet, formula, replicate){
  set.seed(seed)
  results <- boot(data = dataSet,
                  statistic = statistic,
                  R = replicate,
                  formula = formula)
  
  return (cbind.data.frame(alpha = results$t[,1],
                           beta = results$t[,2]))
}

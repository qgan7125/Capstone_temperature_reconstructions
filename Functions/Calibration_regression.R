simulateODR_measured <- function(data, 
                                 replicates, 
                                 samples = NULL, 
                                 D47error = "D47error"){
  do.call(rbind,lapply(1:replicates, function(x){
    dataSub <- data[sample(seq_along(data[,1]), if(is.null(samples)){nrow(data)}else{samples}, replace = T),]
    Reg <- odregress(dataSub$Temperature, dataSub$D47)
    cbind.data.frame("alpha"=Reg$coeff[2],"beta"=Reg$coeff[1])
  }))
}

simulateQR_measured <- function(data, 
                                replicates, 
                                samples = NULL, 
                                D47error = "D47error"){
  do.call(rbind,lapply(1:replicates, function(x){
    dataSub <- data[sample(seq_along(data[,1]), if(is.null(samples)){nrow(data)}else{samples}, replace = T),]
    Reg <- rq(D47 ~ Temperature, data = dataSub)
    cbind.data.frame("alpha"=Reg$coeff[1],"beta"=Reg$coeff[2])
  }))
}

simulateRLM_measured <- function(data, 
                                 replicates, 
                                 samples = NULL, 
                                 D47error = "D47error"){
  do.call(rbind,lapply(1:replicates, function(x){
    dataSub <- data[sample(seq_along(data[,1]), if(is.null(samples)){nrow(data)}else{samples}, replace = T),]
    Reg <- rlm(D47 ~ Temperature, data = dataSub)
    cbind.data.frame("alpha"=Reg$coeff[1],"beta"=Reg$coeff[2])
  }))
}

simulateTS_measured <- function(data, 
                                replicates, 
                                samples = NULL, 
                                D47error = "D47error"){
  do.call(rbind,lapply(1:replicates, function(x){
    dataSub <- data[sample(seq_along(data[,1]), if(is.null(samples)){nrow(data)}else{samples}, replace = T),]
    Reg <- theilsen(D47 ~ Temperature, data = dataSub)
    cbind.data.frame("alpha"=Reg$coeff[1],"beta"=Reg$coeff[2])
  }))
}

simulateLSMC_measured <- function(data, 
                                  replicates, 
                                  samples = NULL, 
                                  D47error = "D47error"){
  do.call(rbind,lapply(1:replicates, function(x){
    dataSub <- data.frame(rep(0, nrow(data)))
    dataSub$Temperature <- rnorm(if(is.null(samples)){nrow(data)}else{samples}, 
                                 data$x_TRUE,
                                 sd(data$TempError))
    dataSub$D47 <- rnorm(if(is.null(samples)){nrow(data)}else{samples}, 
                         data$y_TRUE,
                         sd(data$D47error))
    Reg <- lm(D47 ~ Temperature, data = dataSub)
    cbind.data.frame("alpha"=Reg$coeff[1],"beta"=Reg$coeff[2])
  }))
}

getCals <- function(data, 
                    replicates, 
                    samples = NULL, 
                    D47error = "D47error", 
                    n.iter = 5000, 
                    priors = "Informative"){
  
  LMcals <<- simulateLM_measured(data, replicates = replicates, samples = samples, D47error = D47error)
  yorkcals <<- simulateYork_measured(data, replicates = replicates, samples = samples, D47error = D47error)
  demingcals <<- simulateDeming(data, replicates = replicates, samples = samples, D47error = D47error)
  bayeslincals <<- fitClumpedRegressions(data, n.iter = n.iter, priors = priors, samples = samples, D47error = D47error)
  LSMCcals <<- simulateLSMC_measured(data, replicates = replicates, samples = samples, D47error = D47error)
  ODRcals <<- simulateODR_measured(data, replicates = replicates, samples = samples, D47error = D47error)
  QRcals <<- simulateQR_measured(data, replicates = replicates, samples = samples, D47error = D47error)
  RLMcals <<- simulateRLM_measured(data, replicates = replicates, samples = samples, D47error = D47error)
  TScals <<- simulateTS_measured(data, replicates = replicates, samples = samples, D47error = D47error)
  
  list(LMcals = LMcals,
       yorkcals = yorkcals,
       demingcals = demingcals,
       bayeslincals = bayeslincals,
       LSMCcals = LSMCcals,
       ODRcals = ODRcals,
       QRcals = QRcals,
       RLMcals = RLMcals,
       TScals = TScals
  )
}
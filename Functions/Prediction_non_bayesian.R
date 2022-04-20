predictTc <<- function(calData, 
                       targety, 
                       obCal){
  calData <<- calData
  obCal<<-obCal
  std <- function(x) sd(x)/sqrt(length(x))
  
  mod <- stats::nls(formula = D47  ~ a + b1*T2,
                     data = calData, 
                     start = list(a = mean(obCal$alpha),b1 = mean(obCal$beta)),
                     lower = c(a = (mean(obCal$alpha)-std(obCal$alpha)*1.96),b1 = (mean(obCal$beta)-std(obCal$beta)*1.96)),
                     upper = c(a = (mean(obCal$alpha)+std(obCal$alpha)*1.96),b1 = (mean(obCal$beta)+std(obCal$beta)*1.96)),
                     algorithm = "port") 
  
  estimate <<- investr::invest(mod, y0 = targety,
                               interval = "percentile", 
                               seed = 3,  nsim=1000,
                               extendInt="yes", progress=F, 
                               lower=-100,
                               upper=100)
  
  preds <- cbind.data.frame(D47= mean( targety ), 
                            D47se=sd(targety)/sqrt(length(targety)),
                            temp=estimate$estimate, se=estimate$se, lwr=estimate$lower, upr=estimate$upper
  )
  
  predsUp<- sqrt(10^6/(preds$temp+preds$se))-273.15
  predsPoint <- sqrt(10^6/preds$temp)-273.15
  
  preds <- cbind.data.frame(D47= preds$D47, 
                            D47se= preds$D47se, 
                            temp=predsPoint, 
                            se=predsPoint-predsUp, 
                            lwr=predsPoint-((predsPoint-predsUp)*1.96), 
                            upr=predsPoint+((predsPoint-predsUp)*1.96)
  )
  
}

BayesianPredictions <- function(bayeslincals, 
                                D47Pred,
                                D47Prederror,
                                materialsPred, 
                                nsamp=500){
  
  
  BLM1<-paste("model{
  for(i in 1:N){ 
    x[i] ~ dnorm(11, 0.394)
    y[i] ~ dnorm(mu2[i], tau)
    x2[i] <- sqrt((beta * 10^6) / (y[i] - alpha)) - 273.15
    mu2[i] <- alpha  + beta * x[i]
    
    terr[i] ~ dunif(0.00001, yp[i])
    tp[i] <- y[i]+terr[i]
    xp[i] ~ dnorm(11, 0.394)
    yp[i] ~ dnorm(mu2p[i], tau)
    x2p[i] <- sqrt((beta * 10^6) / (tp[i] - alpha)) - 273.15
    mu2p[i] <- alpha  + beta * xp[i]
    unc[i] <- x2[i]-x2p[i]
    pred[i] ~ dnorm(x2[i], pow(unc[i],-2))
  }
  
}")
  
  postBLM<- bayeslincals$BLM1_fit$BUGSoutput$sims.matrix
  postPredBLM1 <- do.call(rbind,lapply(sample(1:nrow(postBLM), nsamp), function(j){
    tryCatch({
      LM_No_error_Data <- list(
        N=length(D47Pred),
        y=D47Pred,
        yp = D47Prederror,
        alpha=postBLM[j,'alpha'],
        beta=postBLM[j,'beta'],
        tau=postBLM[j,'tau']
      )
      
      BLM1_fit_NoErrors <- jags(data = LM_No_error_Data,
                                parameters = c("pred"),
                                model = textConnection(BLM1), n.chains = 3,
                                n.iter =  2000)
      
      
      
      cbind.data.frame(D47Pred, 
                       D47Prederror, 
                       Tc=BLM1_fit_NoErrors$BUGSoutput$mean$pred,
                       se=BLM1_fit_NoErrors$BUGSoutput$sd$pred
      )
      
    }, error=function(e){c(NA,NA)})
  }))
  postPredBLM1 <-aggregate(postPredBLM1[, 3:4], list(postPredBLM1$D47Pred, postPredBLM1$D47Prederror), mean)
  
}
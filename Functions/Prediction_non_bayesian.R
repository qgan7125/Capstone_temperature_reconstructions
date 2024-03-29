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


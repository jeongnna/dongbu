  # FUNCTIONS

read.easy.csv<-function(filename){
  A<-read.csv(filename, header=TRUE, stringsAsFactors=FALSE)
  return(A)
}

# Function: KISVALUE date -> proper date character vector?? ????. 

set.dates<-function(dates){
  x<-as.character(dates)
  x<-paste(substr(x, start=1, stop=4), "-",
           substr(x, start=5, stop=6), "-",
           substr(x, start=7, stop=8), sep = "")
  return(x)
}

# ???? m_lag, v_lag?? ???? theta?? ???? ?Լ? -> ??��??��. model?? "gjrGARCH", "eGARCH"?? ????.
get_theta <- function(data=NULL, m_lag=NULL, v_lag=NULL, model, dist) {
  len <- length(data)
  if (is.null(data)){
    theta <- ugarchspec(variance.model = list(model = model,
                                              garchOrder = c(1,1)),
                        mean.model = list(armaOrder = c(1,1),
                                          include.mean = TRUE),
                        distribution.model = dist)
  } else {
    if (m_lag <= v_lag) {
      X1 <- data[(v_lag - m_lag + 2):(len - m_lag)]
      Y1 <- data[2:(len - v_lag)]
      X2 <- data[(v_lag - m_lag + 1):(len - 1 - m_lag)]
      Y2 <- data[1:(len - 1 - v_lag)]
    } else {
      X1 <- data[2:(len - m_lag)]
      Y1 <- data[(m_lag - v_lag + 2):(len - v_lag)]
      X2 <- data[1:(len - 1 - m_lag)]
      Y2 <- data[(m_lag - v_lag + 1):(len - 1 - v_lag)]
    }
    
    X <- cbind(X1, X2)
    Y <- cbind(Y1, Y2)
    
    theta <- ugarchspec(variance.model = list(model = model,
                                              garchOrder = c(1,1), 
                                              external.regressors = X),
                        mean.model = list(armaOrder = c(1,1),
                                          include.mean = TRUE,
                                          external.regressors = Y),
                        distribution.model = dist)
  }
  
  theta
}

# ugarchfit ?Լ??? ??Ÿ argument??�� ?̸? ?????ؼ? ?????ϰ? ???? ?Լ?
ugarchfit_customized <- function(data, m_lag=NULL, v_lag=NULL, theta) {
  len <- length(data)
  if (is.null(m_lag)){
    ugarchfit(spec = theta,
              data = data,
              out.sample = 0,
              solver = "hybrid",
              solver.control = list(tol = 1e-12),
              fit.control = list(stationarity = 1))
  } else {
    ugarchfit(spec = theta,
              data = data[(max(m_lag, v_lag) + 3):len],
              out.sample = 0,
              solver = "hybrid",
              solver.control = list(tol = 1e-12),
              fit.control = list(stationarity = 1))
  }
}

# 1 step ahead forecast ?Լ? for Egarch model
## Mu, Sigmas?? ???? ???ͷ??? ???????? ��??��???? ????. true.mu???? ??�� ???ͷ?.
## ???????? true???? sqrt(Parkinson stat)�� ???⿡ ??ǲ?ϴ? ?????? ?̾Ƴ??? ????.
forecast.Egarch<-function(fitted, m_lag=NULL, v_lag=NULL, int.data, ext.data=NULL){
  if (length(fitted@fit)!=27){
    return(list("NOPE"))
  }
  len<-length(int.data); last<-length(fitted@fit$var)
  
  coefs<-coef(fitted); Mu<-c(); Sigma<-fitted@fit$var[last]; Sigmas<-c()
  res<-fitted@fit$residuals[last]
  if (is.null(ext.data)){
    for (i in (last+1):len){
      cond.mu<-coefs["mu"]+coefs["ar1"]*int.data[(i-1)]+coefs["ma1"]*res
      Mu<-c(Mu, cond.mu)
      Sigma<-exp(coefs["omega"]+coefs["alpha1"]*(res)+
                   coefs["beta1"]*log(Sigma)+coefs["gamma1"]*(abs(res)-(2/pi)))
      Sigmas<-c(Sigmas, Sigma)
      res<-int.data[i]-cond.mu
    }
  } else{
    for (i in (last+1):len){
      cond.mu<-coefs["mu"]+coefs["ar1"]*int.data[(i-1)]+coefs["mxreg1"]*ext.data[(i-m_lag)]+
        coefs["mxreg2"]*ext.data[(i-m_lag-1)]+coefs["ma1"]*res
      Mu<-c(Mu, cond.mu)
      Sigma<-exp(coefs["omega"]+coefs["alpha1"]*(res)+
                   coefs["beta1"]*log(Sigma)+coefs["gamma1"]*(abs(res)-(2/pi))
                 +coefs["vxreg1"]*ext.data[(i-m_lag)]+coefs["vxreg2"]*ext.data[(i-v_lag-1)])
      Sigmas<-c(Sigmas, Sigma)
      res<-int.data[i]-cond.mu
    }
  }
  
  Sigmas<-sqrt(Sigmas); true.mu<-int.data[(last+1):len]; 
  forecast<-data.frame(Mu, Sigmas, true.mu)
  return(list(forecast))
}

# 1 step ahead forecast ?Լ? for GJRgarch model
forecast.GJR<-function(fitted, m_lag=NULL, v_lag=NULL, int.data, ext.data=NULL){
  if (length(fitted@fit)!=27){
    return(list("NOPE"))
  }
  len<-length(int.data); last<-length(fitted@fit$var)
  
  coefs<-coef(fitted); Mu<-c(); Sigma<-fitted@fit$var[last]; Sigmas<-c()
  res<-fitted@fit$residuals[last]
  if (is.null(ext.data)){
    for (i in (last+1):len){
      cond.mu<-coefs["mu"]+coefs["ar1"]*int.data[(i-1)]+coefs["ma1"]*res
      Mu<-c(Mu, cond.mu)
      Sigma<-coefs["omega"]+coefs["alpha1"]*(res)^2+
                   coefs["beta1"]*Sigma +coefs["gamma1"]*sum(res<0)*(res)^2
      Sigmas<-c(Sigmas, Sigma)
      res<-int.data[i]-cond.mu
    }
  } else{
    for (i in (last+1):len){
      cond.mu<-coefs["mu"]+coefs["ar1"]*int.data[(i-1)]+coefs["mxreg1"]*ext.data[(i-m_lag)]+
        coefs["mxreg2"]*ext.data[(i-m_lag-1)]+coefs["ma1"]*res
      Mu<-c(Mu, cond.mu)
      Sigma<-coefs["omega"]+coefs["alpha1"]*(res)^2+
                   coefs["beta1"]*Sigma+coefs["gamma1"]*sum(res<0)*(res)^2
                 +coefs["vxreg1"]*ext.data[(i-m_lag)]+coefs["vxreg2"]*ext.data[(i-v_lag-1)]
      Sigmas<-c(Sigmas, Sigma)
      res<-int.data[i]-cond.mu
    }
  }
  
  Sigmas<-sqrt(Sigmas); true.mu<-int.data[(last+1):len]; 
  forecast<-data.frame(Mu, Sigmas, true.mu)
  return(list(forecast))
}

# input forecast object & get MSE, MAE for conditional mean & variance
# park data length should be identical to int.data length of forecast.E/G function input
get_fpm<-function(forecast, park.data){
  forecast<-as.data.frame(forecast)
  if (ncol(forecast)!=3){
    A<-rep("converge.error", times=4); names(A)=c("mu.MSE", "mu.MAE", "s.MSE", "s.MAE")
    return(A)
  }
  mu.MSE<-(forecast$Mu-forecast$true.mu)^2 %>% mean()
  mu.MAE<-abs(forecast$Mu-forecast$true.mu) %>% mean(); 
  len<-length(forecast$true.mu); len.data<-length(park.data)
  
  true.sig<-park.data[(len.data-len+1):len.data]
  s.MSE<-(forecast$Sigmas-true.sig)^2 %>% mean()
  s.MAE<-abs(forecast$Sigmas-true.sig) %>% mean()
  FPMs<-data.frame(mu.MSE, mu.MAE, s.MSE, s.MAE)
  return(FPMs)
}
forecast<-E.solo.fore$view$smtown; stockname<-user[1]; park.data<-Data$smtown$park[Data[[1]]$daily_views>0]

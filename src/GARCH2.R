# ???? m_lag, v_lag?? ???? theta?? ???? ?Լ? -> ??��??��. model?? "gjrGARCH", "eGARCH"?? ????.
get_theta <- function(data = NULL, m_lag = NULL, v_lag = NULL, model, dist) {
  var_model <- list(model = model, garchOrder = c(1, 1))
  mean_model <- list(armaOrder = c(1, 1), include.mean = TRUE)
  if (!is.null(data)) {
    len <- length(data)
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
    var_model$external.regressors <- cbind(X1, X2)
    mean_model$external.regressors <- cbind(Y1, Y2)
  }
  ugarchspec(
    variance.model = var_model,
    mean.model = mean_model,
    distribution.model = dist
  )
}


# ugarchfit ?Լ??? ??Ÿ argument??�� ?̸? ?????ؼ? ?????ϰ? ???? ?Լ?
ugarchfit_customized <- function(data, m_lag = NULL, v_lag = NULL, theta) {
  if (!is.null(m_lag)) {
    len <- length(data)
    data <- data[(max(m_lag, v_lag) + 3):len]
  }
  ugarchfit(
    spec = theta,
    data = data,
    out.sample = 0,
    solver = "hybrid",
    solver.control = list(tol = 1e-12),
    fit.control = list(stationarity = 1)
  )
}


# 1 step ahead forecast ?Լ? for Egarch model
## Mu, Sigmas?? ???? ???ͷ??? ???????? ��??��???? ????. true.mu???? ??�� ???ͷ?.
## ???????? true???? sqrt(Parkinson stat)�� ???⿡ ??ǲ?ϴ? ?????? ?̾Ƴ??? ????.
forecast.Egarch <- function(fitted, m_lag = NULL, v_lag = NULL,
                            int.data, ext.data = NULL) {
  if (length(fitted@fit) != 27) {
    return(list("NOPE"))
  }
  
  len <- length(int.data)
  last <- length(fitted@fit$var)
  
  coefs <- coef(fitted)
  Mu <- NULL
  Sigma <- fitted@fit$var[last]
  Sigmas <- NULL
  res <- fitted@fit$residuals[last]
  
  for (i in (last + 1):len) {
    cond_mu <- sum(
        coefs["mu"],
        coefs["ar1"] * int.data[(i - 1)],
        coefs["ma1"] * res
    )
    Sigma <- exp(sum(
      coefs["omega"],
      coefs["alpha1"] * (res),
      coefs["beta1"] * log(Sigma),
      coefs["gamma1"] * (abs(res) - (2 / pi))
    ))
    if (!is.null(ext.data)) {
      cond_mu <- cond_mu + sum(
        coefs["mxreg1"] * ext.data[(i - m_lag)],
        coefs["mxreg2"] * ext.data[(i - m_lag - 1)]
      )
      Sigma <- Sigma * exp(sum(
        coefs["vxreg1"] * ext.data[(i - m_lag)],
        coefs["vxreg2"] * ext.data[(i - v_lag - 1)]
      ))
    }
    Mu <- c(Mu, cond_mu)
    Sigmas <- c(Sigmas, Sigma)
    res <- int.data[i] - cond_mu
  }
  
  Sigmas <- sqrt(Sigmas)
  true.mu <- int.data[(last + 1):len] 
  forecast <- tibble(Mu, Sigmas, true.mu)
  return (list(forecast))
}


# 1 step ahead forecast ?Լ? for GJRgarch model
forecast.GJR <- function(fitted, m_lag = NULL, v_lag = NULL,
                         int.data, ext.data = NULL) {
  if (length(fitted@fit) != 27) {
    return(list("NOPE"))
  }
  
  len <- length(int.data)
  last <- length(fitted@fit$var)
  
  coefs <- coef(fitted)
  Mu <- NULL
  Sigma <- fitted@fit$var[last]
  Sigmas <- NULL
  res <- fitted@fit$residuals[last]
  
  for (i in (last + 1):len) {
    cond_mu <- sum(
        coefs["mu"],
        coefs["ar1"] * int.data[i - 1],
        coefs["ma1"] * res
    )
    Sigma <- sum(
      coefs["omega"],
      coefs["alpha1"] * res^2,
      coefs["beta1"] * Sigma,
      coefs["gamma1"] * sum(res < 0) * (res)^2,
    )
    if (!is.null(ext.data)) {
      cond_mu <- cond_mu + sum(
        coefs["mxreg1"] * ext.data[i - m_lag],
        coefs["mxreg2"] * ext.data[i - m_lag - 1]
      )
      Sigma <- Sigma + sum(
        coefs["vxreg1"] * ext.data[i - m_lag],
        coefs["vxreg2"] * ext.data[i - v_lag - 1]
      )
    }
    Mu <- c(Mu, cond_mu)
    Sigmas <- c(Sigmas, Sigma)
    res <- int.data[i] - cond_mu
  }
  
  Sigmas <- sqrt(Sigmas)
  true.mu <- int.data[(last + 1):len]
  list(tibble(Mu, Sigmas, true.mu))
}


# input forecast object & get MSE, MAE for conditional mean & variance
# park data length should be identical to int.data length of forecast.E/G function input
get_fpm <- function(forecast, park.data) {
  forecast <- as.data.frame(forecast)
  if (is.null(forecast$Mu)) {
    # A <- rep("converge.error", 4)
    # names(A) = c("mu.MSE", "mu.MAE", "s.MSE", "s.MAE")
    # return (A)
    return (c(
      "mu.MSE" = "converge.error",
      "mu.MAE" = "converge.error",
      "s.MSE" = "converge.error",
      "s.MAE" = "converge.error"
    ))
  }
  
  mu.MSE <- mean((forecast$Mu - forecast$true.mu)^2)
  mu.MAE <- mean(abs(forecast$Mu - forecast$true.mu))
  len <- length(forecast$true.mu)
  len.data <- length(park.data)
  
  true.sig <- park.data[(len.data - len + 1):len.data]
  s.MSE <- mean((forecast$Sigmas - true.sig)^2)
  s.MAE <- mean(abs(forecast$Sigmas - true.sig))
  
  tibble(mu.MSE, mu.MAE, s.MSE, s.MAE)
}


auto.arma.easy <- function(data) {
  auto.arima(data, d = 0, D = 0, max.order = 10, stationary = TRUE, 
             seasonal = FALSE, allowmean = TRUE, approximation = FALSE,
             stepwise = TRUE, max.d = 0, max.D = 0,
             ic = c("aicc", "aic", "bic"), test = "adf", lambda = NULL) 
}

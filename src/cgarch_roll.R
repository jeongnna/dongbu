library(tidyverse)
library(rugarch)
library(rmgarch)
library(tseries)
source("src/GARCH2.R")


# input (multivariate time series data, number of observations to set parameters from,
# distribution & variation model for ugarch
# outputs cgarch fit object
cgarch_auto <- function(data, set.length, dist.model, var.model, time.v, copula) {
  # data <- 
  #   data %>%
  #   as_tibble() %>%
  #   setNames(1:ncol(data))
  data <- as.data.frame(data)
  names(data) <- 1:ncol(data)
  ugarch <- get_theta(model = var.model, dist = dist.model)
  multi <- multispec(replicate(n = ncol(data), expr = ugarch))
  spec  <-  cgarchspec(uspec = multi, VAR = FALSE, robust = FALSE, lag = 0, lag.max = NULL,
                       lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL,
                       robust.control = list(gamma = 0.25, delta = 0.01, nc = 10, ns = 500),
                       dccOrder = c(1, 1), asymmetric = TRUE,
                       distribution.model = list(copula = c(copula),
                                                 method = c("Kendall"), time.varying = time.v,
                                                 transformation = c("parametric")),
                       start.pars = list(), fixed.pars = list()) 
  c.fitted <- cgarchfit(spec = spec, data = data, out.sample = (nrow(data) - set.length),
                        spd.control = list(upper = 0.95, lower = 0.05, type = "mle", kernel = "normal"),
                        cluster = NULL, fit.control = list(eval.se = FALSE))
  u.fitted <- list()
  for (i in 1:ncol(data)) {
    u.fitted[[i]] <- ugarchfit_customized(data = data[,i], theta = ugarch)
  }
  list(MGARCH = c.fitted, UGARCH = u.fitted)
}


# input (multivariate time series data, number of observations to set parameters from,
# distribution & variation model for ugarch
# outputs a rolling forecast of 1step ahead covariance matrices.
# cgarch_vcov <- function(data, dist.model = "sstd", var.model = "eGARCH",
#                         time.v = FALSE, copula = "mvt") {
#   data <- as.data.frame(data)
#   fitted <- cgarch_auto(data = data, set.length = nrow(data), dist.model = dist.model, 
#                       var.model = var.model, time.v = time.v, copula = copula)
#   sim <- cgarchsim(
#     fit = fitted$MGARCH, n.sim = 1, n.start = 200, m.sim = 1000,
#     startMethod = c("sample"), presigma = NULL, preresiduals = NULL, prereturns = NULL,
#     preR = NULL, preQ = NULL, preZ = NULL, cluster = NULL, prerealized = NULL
#   )
#   rcov(sim)[, , 1]
# }
cgarch_vcov <- function(data, dist.model = "sstd", var.model = "eGARCH",
                        time.v = FALSE, copula = "mvnorm") {
  data <- as.data.frame(data)
  fitted <- cgarch_auto(data = data, set.length = nrow(data), dist.model = dist.model, 
                        var.model = var.model, time.v = time.v, copula = copula)
  sim <- cgarchsim(
    fit = fitted$MGARCH, n.sim = 1, n.start = 200, m.sim = 1000,
    startMethod = c("sample"), presigma = NULL, preresiduals = NULL, prereturns = NULL,
    preR = NULL, preQ = NULL, preZ = NULL, cluster = NULL, prerealized = NULL
  )
  rcov(sim)[, , 1]
}

cgarch_last <- function(data, dist.model="sstd", var.model="eGARCH",
                        time.v=FALSE, copula="mvt") {
  data <- as.data.frame(data)
  fitted <- cgarch_auto(data = data, set.length = nrow(data), dist.model = dist.model, 
                        var.model = var.model, time.v = time.v, copula = copula)
  rcov(fitted$MGARCH)[, , nrow(data)]
}

# function utilized for diagnostic tests on cgarch
# input ugarchfit object, outputs 'margins', which can be tested if they are ~ U(0,1)
# default option calculates for cdf of ugarchspec; 'norm' calculates for cdf N(0,1)
margins <- function(fitted, dist = NULL) {
  normalized_res <- fitted@fit$residuals / fitted@fit$sigma
  if (is.null(dist)) {
    if (is.na(coef(fitted)["skew"])) {
      pdist(distribution = "std", q = normalized_res,
            shape = coef(fitted)["shape"])
    } else {
      pdist(distribution = "sstd", q = normalized_res,
            shape = coef(fitted)["shape"], skew = coef(fitted)["shape"])
    }
  } else if (dist == "norm") {
    pdist(distribution = dist, q = normalized_res, mu = 0, sigma = 1)
  }
}

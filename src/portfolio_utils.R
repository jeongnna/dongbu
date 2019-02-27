get_weight <-function(x, covmethod = c("sample", "garch"),
                      optim = c("gmv", "tangency"), risk_free = NULL) {
  # setting for solve.QP()
  nc <- ncol(x)
  zeros <- matrix(0, nrow = nc)
  if (optim == "gmv") {
    A <- cbind(matrix(1, nrow = nc), diag(nc))
    b <- c(1, rep(0, nc))
  } else if (optim == "tangency") {
    A <- cbind(matrix(apply(x, 2, mean) - risk_free), diag(rep(1, nc)))
    b <- c(sum(matrix(apply(x, 2, mean) - risk_free)), rep(0, nc))
  } else {
    stop("ERROR: `optim` must be one of ('gmv', 'tangency')")
  }
  # covariance matrix
  if (covmethod == "sample") {
    covmat <- cov(x)
  } else if (covmethod == "garch") {
    covmat <- cgarch_vcov(x)
  } else if (covmethod == "garch_last") {
    covmat <- cgarch_last(x)
  } else if (covmethod == "garch_var") {
    covmat <- var_vcov(x)
  } else if (covmethod == "garch_var_last") {
    covmat <- var_last(x)
  }
  # optimization
  qp <- solve.QP(covmat, zeros, A, b, meq = 1)
  qp$solution
}


get_portfolio_return <- function(data, analysis_period, clustering_period_length,
                                 clustering_control, portfolio_control,
                                 mc_cores = 1) {
  ncmin <- clustering_control$ncmin
  ncmax <- clustering_control$ncmax
  clmethod <- clustering_control$clmethod
  covmethod <- portfolio_control$covmethod
  optim <- portfolio_control$optim
  
  date_set <- unique(data$date)
  
  process <- function(t) {
    y_idx <- which(date_set == t)
    x_idx <- (y_idx - clustering_period_length):(y_idx - 1)
    clustering_period <- date_set[x_idx]
    
    # cluster information table
    cluster_tbl <- 
      data %>%
      filter(date %in% clustering_period) %>%
      get_cluster_tbl(ncmin, ncmax, clmethod)
    # log returns of each cluster
    c(x, y) %<-% get_cluster_return(data, cluster_tbl, clustering_period)
    
    # portforlio
    rf <- NULL
    if (optim == "tangency") {
      rf <- 
        data %>% 
        filter(date %in% clustering_period) %>%
        pull(rf) %>%
        mean()
    }
    weight <- get_weight(x, covmethod, optim, rf)
    average_return(r = y, w = weight)
  }
  
  pf_return <- unlist(pbmclapply(analysis_period, process, mc.cores = mc_cores))
  
  tibble(
    date = analysis_period,
    logret = pf_return
  )
}


average_portfolio <- function(data, analysis_period) {
  daily %>% 
    group_by(code) %>% 
    mutate(size = lag(size, default = first(size))) %>%
    ungroup() %>% 
    group_by(date) %>% 
    summarize(logret = average_return(r = logret, w = size)) %>% 
    filter(date %in% analysis_period)
}


evaluate_portfolio <- function(portfolio_returns) {
  portfolio_returns %>%
    mutate(benchmark = avg) %>%
    gather(key = portfolio, value = logret, -date, -benchmark) %>%
    group_by(portfolio, year(date)) %>%
    mutate(year_info = mean(logret - benchmark) / (sd(logret - benchmark) + 1e-12)) %>%
    ungroup() %>%
    group_by(portfolio) %>%
    mutate(
      avg_logret = mean(logret),
      sd = sd(logret),
      info = mean(logret - benchmark) / (sd(logret - benchmark) + 1e-12),
      avg_info = mean(year_info)
    ) %>%
    select(portfolio, avg_logret, sd, info, avg_info) %>%
    distinct()
}

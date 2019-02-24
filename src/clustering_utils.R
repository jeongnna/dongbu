get_kmeans_tbl_ <- function(data, ncmin, ncmax) {
  ##############################################################################
  # Please DO NOT call this function directly.                                 #
  # Instead call by `get_cluster_tbl` with argument 'method = "factor_kmeans"' #
  ##############################################################################
  data <-
    data %>%
    select(-logret) %>%
    spread_at(key = "date", cols = -(1:2)) %>%
    na.omit()
  x <- data %>% select(-code)
  ncs <- ncmin:ncmax
  # kmeans models for each nc
  models <- lapply(ncs, function(nc) {kmeanspp(x, nc)})
  # dunn index for each nc
  dunns <- sapply(models, function(object) {dunn(Data = x, clusters = object$cluster)})
  # select the best model by dunn index
  best_model <- models[[which.max(dunns)]]
  data %>%
    select(code) %>%
    distinct() %>%
    mutate(cluster = best_model$cluster)
}


get_hclust_tbl_ <- function(data, ncmin, ncmax, cormethod) {
  ##############################################################################
  # Please DO NOT call this function directly.                                 #
  # Instead call by `get_cluster_tbl` with argument 'method = "return_hclust"' #
  ##############################################################################
  corr <- 
    data %>%
    select(code, date, logret) %>%
    spread(key = code, value = logret) %>%
    select(-date) %>%
    cor(method = cormethod)
  dissim <- 0.5 * (1 - corr)
  ncs <- ncmin:ncmax
  # hclust model
  model <- hclust(as.dist(dissim))
  # dunn index for each nc
  dunns <- sapply(ncs, function(nc) {dunn(distance = dissim, clusters = cutree(model, k = nc))})
  best_nc <- ncs[which.max(dunns)]
  data %>%
    select(code) %>%
    distinct() %>%
    mutate(cluster = cutree(model, k = best_nc))
}


get_cluster_tbl <- function(data, ncmin, ncmax, clmethod) {
  if (clmethod == "factor_kmeans") {
    get_kmeans_tbl_(data, ncmin, ncmax)
  } else if (clmethod == "hclust_pearson") {
    get_hclust_tbl_(data, ncmin, ncmax, cormethod = "pearson")
  } else if (clmethod == "hclust_kendall") {
    get_hclust_tbl_(data, ncmin, ncmax, cormethod = "kendall")
  } else {
    stop("ERROR: unexpected `clmethod`")
  }
}


average_return <- function(return, weight, log_scale = TRUE) {
  weight <- weight / sum(weight)
  if (log_scale) {
    log(sum(weight * exp(return)))
  } else {
    sum(weight * return)
  }
}


get_cluster_return <- function(data, cluster_tbl) {
  data %>%
    inner_join(cluster_tbl, by = "code") %>%
    select(code, date, logret, size, cluster) %>%
    group_by(code) %>%
    mutate(size = lag(size, default = first(size))) %>%
    ungroup() %>%
    group_by(cluster, date) %>%
    summarize(logret = average_return(r = logret, w = size)) %>%
    ungroup() %>%
    spread(key = cluster, value = logret, sep = "")
}


clustering_process <- function(data, clustering_period, ncmin, ncmax, clmethod) {
  # cluster information table
  cluster_tbl <- 
    data %>%
    filter(date %in% clustering_period) %>%
    get_cluster_tbl(ncmin, ncmax, clmethod)
  # log returns of each cluster
  cluster_return <- get_cluster_return(data, cluster_tbl)
  # split sample & target period
  x <- cluster_return %>% filter(date %in% clustering_period)
  y <- cluster_return %>% next_day_of(clustering_period)
  list(x = x, y = y)
}

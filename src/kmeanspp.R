kmeanspp <- function(x, k, iter_max = 500, nstart = 20,
                     algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
                                   "MacQueen"), trace = FALSE) {
  n <- nrow(x)
  centers <- integer(k)
  centers[1] <- sample(1:n, 1)
  L2_mat <- as.matrix(dist(x))^2
  
  for (i in 2:k) {
    weight <- apply(as.matrix(L2_mat[, centers]), 1, min)
    centers[i] <- sample(1:n, 1, prob = weight)
  }
  
  kmeans(x, x[centers, ], iter_max, nstart, algorithm, trace)
}

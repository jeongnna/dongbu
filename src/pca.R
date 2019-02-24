pca <- function(data, cols = NULL, threshold = 0.9) {
  if (is.null(cols)) {
    cols <- 1:ncols(data)
  }
  prfit <-
    data %>%
    select(cols) %>%
    na.omit() %>%
    prcomp()
  pve <- prfit$sdev^2 %>% (function(x) {x / sum(x)})
  n_pc <- first(which(cumsum(pve) > threshold))
  x_origin <- as.matrix(data[cols])
  loading <- prfit$rotation[, 1:n_pc]
  x_pc <- x_origin %*% loading
  bind_cols(data[-cols], as_tibble(x_pc))
}

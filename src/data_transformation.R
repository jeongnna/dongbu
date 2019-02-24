date_in <- function(data, period, group = NULL) {
  if (is.null(group)) {
    data %>% filter(date %in% period)
  } else {
    data %>%
      group_by(get(group)) %>%
      filter(date %in% period) %>%
      ungroup()
  }
}


next_day_of <- function(data, period) {
  date_set <- unique(data$date)
  idx <- last(which(date_set %in% period)) + 1
  next_day <- date_set[idx]
  data %>% filter(date == next_day)
}


normalize_at <- function(data, cols = NULL, method = "meansd") {
  if (is.null(cols)) {
    cols <- 1:ncol(data)
  }
  if (method == "meansd") {
    data[cols] <- lapply(data[cols], scale)
  } else if (method == "minmax") {
    data[cols] <- lapply(data[cols], function(x) {(x - min(x)) / (max(x) - min(x))})
  }
  data
}


spread_at <- function(data, key, cols) {
  data %>% 
    gather(column, value, cols) %>% 
    unite(tmp, column, key) %>% 
    spread(tmp, value)
}


expand_grid <- function(...) {
  grd <- expand.grid(...) %>%
    as_tibble()
  nc <- ncol(grd)
  text <- 
    str_c(
      "grd %>% arrange(",
      str_c(str_c(".[[", 1:nc, "]]"), collapse = ", "),
      ")"
    )
  expr <- parse(text = text)
  eval(expr) %>% mutate_if(is.factor, as.character)
}


inner_join_all <- function(..., datalist = NULL, by = NULL) {
  datalist <- append(list(...), datalist)
  data <- datalist[[1]]
  for (i in 2:length(datalist)) {
    data <- inner_join(data, datalist[[i]], by = by)
  }
  data
}

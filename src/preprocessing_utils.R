nan2na <- function(x) {
  x[is.nan(x)] <- NA
  x
}


as_quarter <- function(x) {
  # This function converts month to quarter.
  #
  # Args:
  #   x: integer vector in range [1, 12]
  #
  # Returns:
  #   an integer vector
  
  x <- as.integer(x)
  if (!all(x %in% 1:12)) {
    stop("ERROR: `x` must be in range [1, 12].")
  }
  (x - 1) %/% 3 + 1
}


as_month <- function(x) {
  # This function converts quarter to month.
  #
  # Args:
  #   x: integer vector in range [1, 4]
  
  x <- as.integer(x)
  if (length(x) != 3) {
    stop("ERROR: length of `x` must be 3.")
  }
  if (!all(x %in% 1:4)) {
    stop("ERROR: `x` must be in range [1, 4].")
  }
  3 * (x - 1) + 1:3
}


month_pad0 <- function(m) {
  mask <- m < 10
  m[mask] <- str_c(0, m[mask])
  m
}


interpolate_quarter_to_month <- function(data) {
  data %>% 
    select(code, name, year, quarter, value) %>% 
    group_by(code) %>% 
    mutate(
      prev_value = lag(value, default = first(value)),
      inc = (value - prev_value) / 3
    ) %>% 
    group_by(code, name, year, quarter) %>%
    sample_n(3, replace = TRUE) %>% 
    mutate(
      month = as_month(quarter),
      cuminc = cumsum(inc),
      value = prev_value + cuminc
    ) %>%
    ungroup() %>% 
    select(code, name, year, month, value)
}


is_date_format <- function(x) {
  # This function examines strings are in format "yyyymmdd"
  # where 'y', 'm' and 'd' are all digits.
  #
  # Args:
  #   x: character vector
  #
  # Returns:
  #   a logical value
  
  if (!is.character(x)) {
    stop("ERROR: `x` must be a character vector")
  }
  all(str_detect(x, "^[:digit:]{8}$"))
}


kisvalue_data_to_longform <- function(data) {
  # Args:
  #   data: data frame (short form data from kisvalue)
  #
  # Returns:
  #   a tibble (long form)
  
  names(data)[1:2] <- c("code", "name")
  data <- data %>% gather(key = "date", value = "value", -(1:2))
  data$value <- as.numeric(data$value)
  if (is_date_format(data$date)) {
    data <-
      data %>%
      mutate(date = as.Date(date, format = "%Y%m%d"))
      # mutate(
      #   year = year(date),
      #   month = month(date)
      # )
  } else {
    data <-
      data %>%
      separate(col = date, into = c("year", "quarter")) %>%
      mutate(
        quarter = as.integer(plyr::revalue(quarter, c("Semi" = 2, "Annual" = 4)))
      ) %>%
      interpolate_quarter_to_month() %>%
      mutate(date = str_c(year, month, 1, sep = "-")) %>%
      mutate(date = as.Date(date, format = "%Y-%m-%d"))
  }
  data %>% 
    select(code, date, value) %>%
    arrange(code, date)
}


load_pp_features <- function(dir) {
  if (!str_detect(dir, "/$")) {
    dir <- str_c(dir, "/")
  }
  files <- c(
    asset = "KOSPI200_assets.xls",
    equity = "KOSPI200_equity.xls",
    market_cap = "KOSPI200_market_cap.xls",
    net_profit = "KOSPI200_net_profit.xls",
    stock_num = "KOSPI200_stock_number.xls",
    price = "KOSPI200_adjusted_prices.xls",
    trade_amount = "KOSPI200_trade_amount.xls",
    volatility = "KOSPI200_volatility.xls"
  )
  features <- NULL
  for(f in files) {
    cat("loading ", f, "...\n", sep = "")
    in_path <- str_c(dir, f)
    data <- kisvalue_data_to_longform(read_excel(in_path, skip = 5)[-1, -1])
    if (is.null(features)) {
      features <- data
    } else {
      features <- features %>% inner_join(data, by = c("code", "date"))
    }
  }
  features %>%
    setNames(c("code", "date", names(files))) %>%
    mutate_all(nan2na) %>%
    transmute(
      code = code,
      date = date,
      asset = asset,
      equity = equity,
      leverage = (asset - equity) / equity,
      size = market_cap,
      roa = net_profit / asset,
      roe = net_profit / equity,
      sharesturnover = trade_amount / stock_num,
      volatility = volatility,
      logret = c(NA, diff(log(price)))
    )
}


load_pp_kospi <- function(dir) {
  if (!str_detect(dir, "/$")) {
    dir <- str_c(dir, "/")
  }
  in_path <- str_c(dir, "KOSPI200_index.xls")
  data <- read_excel(in_path, skip = 4)[-1, ]
  names(data)[1:2] <- c("code", "name")
  data %>%
    filter(code == "KP0010") %>%
    gather(key = date, value = price, -(1:2)) %>%
    mutate(
      date = as.Date(date, format = "%Y%m%d"),
      year = year(date),
      month = month(date),
      price = as.numeric(price)
    ) %>%
    arrange(year, month) %>%
    transmute(
      date = str_c(year, month_pad0(month), sep = "-"),
      logret = c(NA, diff(log(price)))
    )
}


load_daily_price <- function(dir, year) {
  if (!str_detect(dir, "/$")) {
    dir <- str_c(dir, "/")
  }
  in_path <- str_c(dir, "Daily", year, ".xls")
  data <- read_excel(in_path, skip = 5)[-1, -1]
  names(data)[1:2] <- c("code", "name")
  data %>%
    gather(key = "date", value = "price", -(1:2)) %>%
    arrange(code, date) %>%
    mutate(
      date = as.Date(date, format = "%Y%m%d"),
      price = as.numeric(price)
    ) %>%
    select(code, date, price)
}

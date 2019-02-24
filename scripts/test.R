library(tidyverse)
library(lubridate)
library(clValid)
library(quadprog)
library(parallel)
source("src/data_transformation.R")
source("src/clustering_utils.R")
source("src/portfolio_utils.R")
source("src/cgarch_roll.R")
first <- dplyr::first
last <- dplyr::last


# Arguments ---------------------------------------------------------------

for (arg in commandArgs(TRUE)) {
  eval(parse(text=arg))
}

# case grid
grd <- expand_grid(
  period_length = c(100),
  clmethod = c("hclust_pearson", "hclust_kendall"),
  covmethod = c("sample"),
  optim = c("gmv")
)

# number of clusters
ncmin <- 4
ncmax <- 6


# Prepare data ------------------------------------------------------------

data_dir <- "data/processed/"
daily <- read_csv(str_c(data_dir, "daily.csv"))
market_cap <- read_csv(str_c(data_dir, "market_cap.csv"))

daily <- 
  left_join(
    daily %>% 
      mutate(year = year(date), month = month(date)),
    market_cap %>% 
      transmute(code, year = year(date), month = month(date), size),
    by = c("code", "year", "month")
  ) %>% 
  select(code, date, logret, size)


# Get all portfolio returns -----------------------------------------------

date_set <- unique(daily$date)  # length: 3704
start <- first(which(date_set >= str_c(analysis_year, "-01-01")))
end <- start + 19
analysis_period <- date_set[start:end]
risk_free <- NULL

process <- function(len, clmethod, covmethod, optim, mc_cores) {
  c_ctrl <- list(
    ncmin = 4,
    ncmax = 6,
    clmethod = clmethod
  )
  p_ctrl <- list(
    optim = optim,
    covmethod = covmethod,
    risk_free = risk_free
  )
  get_portfolio_return(daily, analysis_period, len, c_ctrl, p_ctrl, mc_cores)
}

process_time <- system.time({
  pf_return_list <- mapply(
    FUN = process,
    len = grd$period_length,
    clmethod = grd$clmethod,
    covmethod = grd$covmethod,
    optim = grd$optim,
    MoreArgs = list(mc_cores = mc_cores),
    SIMPLIFY = FALSE
  )
})

# size-weighted-average portfolio (benchmark)
avg_portfolio <- average_portfolio(daily, analysis_period)

# combine all portfolio returns
case_names <- str_c("case-", unite(grd, col = case, everything(), sep = "-")$case)
allpofol_returns <-
  inner_join_all(avg_portfolio, datalist = pf_return_list, by = "date") %>% 
  setNames(c("date", "avg", case_names))

# test
load("outputs/testing.RData")
cat("\n")
cat(identical(allpofol_returns, correct_allpofol_returns))
cat("\n\n")

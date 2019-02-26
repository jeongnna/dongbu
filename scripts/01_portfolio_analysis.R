library(tidyverse)
library(lubridate)
library(clValid)
library(quadprog)
library(pbmcapply)
source("src/data_transformation.R")
source("src/clustering_utils.R")
source("src/portfolio_utils.R")
source("src/cgarch_roll.R")
first <- dplyr::first
last <- dplyr::last


# Arguments ---------------------------------------------------------------

# for (arg in commandArgs(TRUE)) {
#   eval(parse(text=arg))
# }

mc_cores <- 6

# case grid
grd <- expand_grid(
  period_length = c(1600),
  clmethod = c("hclust_pearson"),
  covmethod = c("garch"),
  optim = c("gmv")
)

# number of clusters
ncmin <- 4
ncmax <- 6


# Prepare data ------------------------------------------------------------

data_dir <- "data/processed/"
daily <- read_csv(str_c(data_dir, "daily.csv"))
market_cap <- read_csv(str_c(data_dir, "market_cap.csv"))
risk_free <- read_csv(str_c(data_dir, "cd_rate.csv"))

daily <- 
  daily %>% 
  left_join(risk_free, by = "date") %>% 
  mutate(year = year(date), month = month(date)) %>% 
  left_join(
    market_cap %>% transmute(code, year = year(date), month = month(date), size),
    by = c("code", "year", "month")
  ) %>% 
  select(code, date, logret, rf, size)


###########################################################################
###########################################################################
for (analysis_year in 2011:2018) {
  cat(
    "\n",
    "analysis year: ", analysis_year,
    "\n",
    sep = ""
  )

# Get all portfolio returns -----------------------------------------------

date_set <- unique(daily$date)  # length: 3704
start <- first(which(date_set >= str_c(analysis_year, "-01-01")))
end <- last(which(date_set <= str_c(analysis_year, "-12-31")))
analysis_period <- date_set[start:end]

process <- function(len, clmethod, covmethod, optim, mc_cores) {
  cat(
    "len: ", len,
    ", clmethod: ", clmethod,
    ", covmethod: ", covmethod,
    ", optim: ", optim,
    "\n", sep = ""
  )
  c_ctrl <- list(
    ncmin = 4,
    ncmax = 6,
    clmethod = clmethod
  )
  p_ctrl <- list(
    optim = optim,
    covmethod = covmethod
  )
  get_portfolio_return(daily, analysis_period, len, c_ctrl, p_ctrl, mc_cores)
}

system.time({
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

# save result
out_path <- str_c("outputs/garch-gmv-mvnorm-timev-", analysis_year, ".RData")
save(allpofol_returns, file = out_path)

}
########################################################################### 
###########################################################################

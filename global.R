library(Rcpp)
library(tidyverse)
library(tidyquant)
library(TTR)

sourceCpp("bond.cpp")

# PRICE BOND FUNCTION
bond_cpp_call <- function(ytm = 0.05, C = 0.05, T2M = 1, m = 2, face = 100) {
  result <- bond_cpp(ytm, C, T2M, m, face)
  return(result)
}

#bond_cpp_call(ytm = 0.06, C = 0.05, T2M = 10, m = 2, face = 100)

# DURATION FUNCTION
duration_cpp_call <- function(ytm = 0.05, C = 0.05, T2M = 1, m = 2, face = 100) {
  result_d <- PVBdur(ytm, C, T2M, m, face)
  return(result_d)
}

# PVBdur(ytm = 0.06, C = 0.05, T2M = 10, m = 2, face = 100)
# duration_cpp_call(ytm = 0.06, C = 0.05, T2M = 10, m = 2, face = 100)

# CONVEXITY FUNCTION
convexity_cpp_call <- function(ytm = 0.05, C = 0.05, T2M = 1, m = 2, face = 100) {
  result <- calculateConvexityR(ytm, C, T2M, m, face)
  return(result)
}

bond_df_org <- data.frame(data.frame(
  C = numeric(),
  YTM = numeric(),
  Par = numeric(),
  TTM = numeric(),
  "Bonds held" = numeric(),
  Price = numeric(0),
  "Total Value" = numeric(),
  Duration = numeric(),
  Delta = numeric(0),
  Gamma = numeric(0)
))

# LOAD HISTORICAL DATA
strd <- "1992-01-01"
codes <- c("1MO", "3MO", "6MO", "1", "2", "5", "7", "10", "20", "30") 

tickers <- c()
for (i in 1:length(codes)) {
  # DGS is Market Yield on US Treasury Securities with those specific time frames
  tickers <- c(tickers, paste0("DGS", codes[i]))
}

# FRED DATA PULL
FRED <- tidyquant::tq_get(c(tickers), get = "economic.data", from = strd)

FRED_data <- FRED %>% 
  dplyr::group_by(symbol) %>% 
  dplyr::mutate(symbol = dplyr::case_when(symbol == 'DGS1MO' ~ 1/12,
                                          symbol == 'DGS3MO' ~ 3 / 12,
                                          symbol == 'DGS6MO' ~ 6 / 12,
                                          symbol == 'DGS1' ~ 1,
                                          symbol == 'DGS2' ~ 2,
                                          symbol == 'DGS5' ~ 5,
                                          symbol == 'DGS7' ~ 7,
                                          symbol == 'DGS10' ~ 10,
                                          symbol == 'DGS20' ~ 20,
                                          symbol == 'DGS30' ~ 30), 
                rate = price / 100,
                cf = 100,
                basischange = (rate - dplyr::lag(rate)) * 10000,
                Bond_price = 100 - price,
                Frac = stringr::str_sub(symbol, start = 4),
                num = as.numeric(stringr::str_extract(symbol, "[0-9]+")),
                test = dplyr::if_else(grepl("MO", symbol), num/12, num),
                Adj_price = dplyr::if_else(test < 1, Bond_price, (100/((1+(price/100))^test))),
                Yield = price) %>% 
  tidyr::drop_na() %>% 
  dplyr::mutate(sd = round(runSD(basischange, n = 5, sample = F), 2))

# TAYLOR SERIES
taylorseries <- function(basis_shock = 1, ytm = 0.05) {
  taylor <- FRED_data %>% 
    dplyr::mutate(ytm = 0.05,
                  fv = 100,
                  coupon = 0,
                  m = 2,
                  price = bond_cpp_call(ytm = ytm, c = 0, t2m = symbol, m = m, face = fv),
                  duration = durtion_cpp_call(ytm = ytm, c = 0, t2m = symbol, m = m, face = fv),
                  convexity = convexity_cpp_call(ytm = ytm, c = 0, t2m = symbol, m = m, face = fv),
                  shock = 1,
                  term1 = durationshock_upprice,
                  term2 = 0.5*convexityshock^2*price,
                  risk = term1 + term2)
}

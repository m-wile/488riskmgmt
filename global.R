library(Rcpp)
library(tidyverse)
library(tidyquant)
library(TTR)

Rcpp::sourceCpp("bond.cpp")

bond_cpp_call2 <- function(ytm = 0.05, C = 0.05, T2M = 1, m = 2, face = 100) {
  result <- bond_cpp2(ytm, C, T2M, m, face)
  return(result)
}

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

#FRED DATA PULL
StepSize <- 0.01/100
FRED <- tidyquant::tq_get(c(tickers), get = "economic.data", from = strd) %>% 
  na.omit()

FRED_data <- FRED %>%
  dplyr::group_by(symbol) %>%
  dplyr::mutate(symbol = dplyr::case_when(symbol == 'DGS1MO' ~ 1/12,
                                          symbol == 'DGS3MO' ~ 3/12,
                                          symbol == 'DGS6MO' ~ 6/12,
                                          symbol == 'DGS1' ~ 1,
                                          symbol == 'DGS2' ~ 2,
                                          symbol == 'DGS5' ~ 5,
                                          symbol == 'DGS7' ~ 7,
                                          symbol == 'DGS10' ~ 10,
                                          symbol == 'DGS20' ~ 20,
                                          symbol == 'DGS30' ~ 30, 
                                          TRUE ~ 0), 
                rate = price/100) %>%
  ungroup() %>%
  dplyr::mutate(Price = bond_cpp_call2(ytm = rate, C = 0, T2M = symbol, m = 2, face = 100),
                Price_minus = bond_cpp_call2(ytm = rate - StepSize, C = 0, T2M = symbol, m = 2, face = 100),
                Price_plus = bond_cpp_call2(ytm = rate + StepSize, C = 0, T2M = symbol, m = 2, face = 100),
                Delta = as.numeric((Price_plus - Price_minus) / (2 * StepSize) / 10000), 
                Gamma = as.numeric(0.5 * ((Price_plus - 2 * Price + Price_minus) / StepSize^2) / 10000^2)) %>% 
  tidyr::drop_na() %>%
  # remove weirdly high values
  dplyr::filter(Delta < 2, Gamma < 2, Delta > -2, Gamma > -2) %>% 
  
  # replace inf
  dplyr::mutate(Delta = ifelse(Delta == Inf, NA, Delta),
                Gamma = ifelse(Gamma == Inf, NA, Gamma)) 

# TAYLOR SERIES
taylorseries <- function(basis_shock = 1, ytm = 0.05) {
  taylor <- FRED_data %>% 
    dplyr::mutate(ytm = 0.05,
                  fv = 100,
                  coupon = 0,
                  m = 2,
                  # Change YTM intake to vector - will be taking the portfolio 
                  price = bond_cpp_call(ytm = ytm, C = 0, T2M = symbol, m = m, face = fv),
                  duration = duration_cpp_call(ytm = ytm, C = 0, T2M = symbol, m = m, face = fv),
                  convexity = convexity_cpp_call(ytm = ytm, C = 0, T2M = symbol, m = m, face = fv),
                  shock = 1,
                  term1 = duration*shock*price,
                  term2 = 0.5*convexity*shock^2*price,
                  risk = term1 + term2)
}

library(Rcpp)
sourceCpp("bond.cpp")
#######################################################
# PRICE BOND FUNCTION
#######################################################
bond_cpp_call <- function(ytm = 0.05, C = 0.05, T2M = 1, m = 2, face = 100) {
  result <- bond_cpp(ytm, C, T2M, m, face)
  return(result)
}


#######################################################
# Duration FUNCTION
#######################################################

duration_cpp_call <- function(ytm = 0.05, C = 0.05, T2M = 1, m = 2, face = 100) {
  result <- calculateMacaulayDurationR(ytm, C, T2M, m, face)
  return(result)
}


#######################################################
# Convexity FUNCTION
#######################################################
convexity_cpp_call <- function(ytm = 0.05, C = 0.05, T2M = 1, m = 2, face = 100) {
  result <- calculateConvexityR(ytm, C, T2M, m, face)
  return(result)
}

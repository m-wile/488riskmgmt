library(Rcpp)
sourceCpp("bond.cpp")
#######################################################
# PRICE BOND FUNCTION
#######################################################
bond_cpp_call <- function(ytm = 0.05, C = 0.05, T2M = 1, m = 2, face = 100) {
  result <- bond_cpp(ytm, C, T2M, m, face)
  return(result)
}


# Time the execution of the loop
# cpp_time <- system.time(
#   for (i in 1:1000) {
#     bond_cpp_call(ytm = 0.03, C = 0.05, T2M = 1, m = 2, output = "price")
#   }
# )
# r_time <- system.time(
#   for (i in 1:1000) {
#     RTL::bond(ytm = 0.03, C = 0.05, T2M = 1, m = 2, output = "df")
#   }
# )
# cpp_time
# r_time


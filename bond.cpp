#include <Rcpp.h>
#include <cmath>  // Include the cmath header for pow function

// [[Rcpp::export]]
float bond_cpp(double ytm = 0.05, double C = 0.05, double T2M = 1, int m = 2, float face = 100) {
  
  // function to calculate the price of a coupon bond
  float price = 0;
  
  // discounting all the coupons
  for (int i = 1; i <= (T2M * m); ++i) {
    price += (C * face / m) / std::pow((1 + ytm / m), i);
  }
  
  // discounting the face value
  price += face / std::pow(1 + ytm / m, T2M * m);
  
  return price;
}

#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// Function to calculate Macaulay duration
double calculateMacaulayDuration(double ytm, double C, double T2M, int m, float face) {
  double duration = 0.0;
  double discountFactor;
  
  for (int i = 1; i <= m * T2M; ++i) {
    discountFactor = 1 / pow(1 + ytm / m, i);
    duration += i * C / m * discountFactor;}
  
  duration += m * T2M * face / pow(1 + ytm / m, m * T2M);
  return duration / (1 + ytm / m);}

// Function to calculate convexity
double calculateConvexity(double ytm, double C, double T2M, int m, float face) {
  double convexity = 0.0;
  double discountFactor;
  for (int i = 1; i <= m * T2M; ++i) {
    discountFactor = 1 / pow(1 + ytm / m, i);
    convexity += pow(i / m, 2) * C / m * discountFactor;
  }
  convexity += pow(m * T2M, 2) * face / pow(1 + ytm / m, m * T2M);
  return convexity / pow(1 + ytm / m, 2);}

// Expose the functions to R using Rcpp
// [[Rcpp::export]]
double calculateMacaulayDurationR(double ytm, double C, double T2M, int m, float face) {
  return calculateMacaulayDuration(ytm, C, T2M, m, face);
}

// [[Rcpp::export]]
double calculateConvexityR(double ytm, double C, double T2M, int m, float face) {
  return calculateConvexity(ytm, C, T2M, m, face);
}

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

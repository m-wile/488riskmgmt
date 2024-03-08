#include <Rcpp.h>
#include <cmath>
#include <iostream>

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

// Expose the functions to ytm using Rcpp
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

// [[Rcpp::export]]
NumericVector bond_cpp2(NumericVector ytm, NumericVector C, NumericVector T2M, int m = 2, float face = 100) {
  
  // calculate the number of elements in the vectors
  int n = ytm.size();
  
  // create a vector to store the results
  NumericVector price(n);
  
  // loop over the vectors and calculate the price for each set of parameters
  for (int k = 0; k < n; ++k) {
    // discounting all the coupons
    for (int i = 1; i <= (T2M[k] * m); ++i) {
      price[k] += (C[k] * face / m) / std::pow((1 + ytm[k] / m), i);
    }
    
    // discounting the face value
    price[k] += face / std::pow(1 + ytm[k] / m, T2M[k] * m);
  }
  
  return price;
}

using namespace std;
// [[Rcpp::export]]
double PVBdur(float face,double C,double ytm, int m, double T2M)
  
{
  
  // store the value of the bond
  
  double BV1=0.;
  
  double BV2=0.;
  
  double Dur = 0.;
  
  double rr = ytm + 0.000001;
  
  // add in coupons
  
  int TNC=T2M*m;
  
  double cpn = (C/m)*face;
  
  for(int i=1;i<=TNC; i++)
    
  {
    
    BV1 = BV1 + cpn*pow((1+ytm/m),-i);
    
    BV2 = BV2 + cpn*pow((1+rr/m),-i);
    
  }
  
  // finally add principle
  
  BV1 = BV1 + face*pow((1+ytm/m),-T2M*m);
  
  BV2 = BV2 + face*pow((1+rr/m),-T2M*m);
  
  Dur = (((BV2 - BV1)/BV1)*(1+ytm/m)/(ytm-rr))/(1+ytm/m);
  
  return Dur;
  
}

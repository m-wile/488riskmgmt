#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::List bond_cpp(double ytm = 0.05, double C = 0.05, double T2M = 1, int m = 2, std::string output = "price") {
  int numPeriods = static_cast<int>(T2M * m);
  
  Rcpp::NumericVector t_years(numPeriods);
  Rcpp::NumericVector cf(numPeriods);
  Rcpp::NumericVector t_periods(numPeriods);
  Rcpp::NumericVector disc_factor(numPeriods);
  Rcpp::NumericVector pv(numPeriods);
  Rcpp::NumericVector duration(numPeriods);
  
  // Create tibble
  for (int i = 0; i < numPeriods; ++i) {
    t_years[i] = (i + 1.0) / m;
    cf[i] = C * 100 / m;
  }
  
  // Mutate
  for (int i = 0; i < numPeriods; ++i) {
    t_periods[i] = m * t_years[i];
    if (t_years[i] == T2M) {
      cf[i] += 100;
    }
    disc_factor[i] = 1 / pow(1 + ytm / m, t_periods[i]);
    pv[i] = cf[i] * disc_factor[i];
  }
  
  // Calculate price
  double price = std::accumulate(pv.begin(), pv.end(), 0.0);
  
  // Mutate for duration
  for (int i = 0; i < numPeriods; ++i) {
    duration[i] = (pv[i] * t_years[i]) / price;
  }
  
  // Output handling
  if (output == "price") {
    return Rcpp::wrap(price);
  }
  if (output == "df") {
    return Rcpp::List::create(
      Rcpp::Named("t_years") = t_years,
      Rcpp::Named("cf") = cf,
      Rcpp::Named("t_periods") = t_periods,
      Rcpp::Named("disc_factor") = disc_factor,
      Rcpp::Named("pv") = pv,
      Rcpp::Named("duration") = duration
    );
  }
  if (output == "duration") {
    return Rcpp::wrap(std::accumulate(duration.begin(), duration.end(), 0.0));
  }
  
  Rcpp::stop("error in output variable definition");
  return Rcpp::List();  // To avoid compiler warning
}

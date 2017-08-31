#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


//' testing function
//' @export
//' @param x vector
//[[Rcpp::export]]
NumericVector timesThree(NumericVector x) {
  return x * 3;
}

//' testing function
//' @export
//' @param x vector
//[[Rcpp::export]]
double getMean(NumericVector x) {
  double res = 0.0;
  for(int i = 0; i < x.size(); i++) {
    res += x[i];
  }
  return res/x.size();
}

//' testing function
//' @export
//' @param x vector
//[[Rcpp::export]]
double getSharpe(NumericVector x) {
  double res = 0.0;
  double m = 0.0;
  for(int i = 0; i < x.size(); i++) {
    res +=x[i];
  }
  m = res/x.size();

  double res2 = 0.0;
  for(int i = 0; i < x.size(); i++) {
    res2+= pow((x[i]-m),2);
  }
  return sqrt(res2/(x.size()-1));
}

//' testing function
//' @export
//' @param x vector
//[[Rcpp::export]]
double getSD(NumericVector x) {
  return sqrt((sum(pow(x,2))/x.size()- pow(mean(x),2))*x.size()/(x.size()-1));
}




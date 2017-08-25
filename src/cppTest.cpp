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


//' @export
//[[Rcpp::export]]
NumericVector timesThree(NumericVector x) {
  return x * 3;
}

//' @export
//[[Rcpp::export]]
double getMean(NumericVector x) {
  double res = 0.0;
  for(int i = 0; i < x.size(); i++) {
    res += x[i];
  }
  return res/x.size();
}

//' @export
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

//' @export
//[[Rcpp::export]]
double getSD(NumericVector x) {
  int len = x.size();
  return sqrt((sum(pow(x,2))/x.size()- pow(mean(x),2))*x.size()/(x.size()-1));

}




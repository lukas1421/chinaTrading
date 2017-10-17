#include <Rcpp.h>
using namespace Rcpp;
#include <algorithm>
#include <string>

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
  for(unsigned int i = 0; i < x.size(); i++) {
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
  for(unsigned int i = 0; i < x.size(); i++) {
    res +=x[i];
  }
  m = res/x.size();

  double res2 = 0.0;
  for(unsigned int i = 0; i < x.size(); i++) {
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

//' get ticker function
//' @export
//' @param s vector
//[[Rcpp::export]]
std::vector< std::string > getTicker(std::vector<std::string> s) {
  std::vector<std::string> res;
  for(unsigned int i = 0; i < s.size(); i++) {
    std::transform(s[i].begin(), s[i].end(),s[i].begin(),::toupper);
    res.push_back(s[i].substr(0,2)+"#"+s[i].substr(2,6));
    //(0,2) + "#" + s(i).substr(2, 6);
  }
  return res;
}


//' get one ticker(not vectorized)
//' @export
//' @param s one string
//[[Rcpp::export]]
std::string getOneTicker(std::string s) {
  std::transform(s.begin(),s.end(),s.begin(),::toupper);
  return s.substr(0,2)+"#"+s.substr(2,6);
}



//' identity function
//' @export
//' @param x vector
//[[Rcpp::export]]
double getSum(NumericVector x) {
  return sum(x);
}

//' get sumChg, sumChgSq, n, sr of a change series in list
//' @export
//' @param x numvec
//[[Rcpp::export]]
List getSumChgC(NumericVector x) {
  double sumChg = sum(x);
  double sumChgSq = sum(pow(x,2));
  int n = x.size();

  if(sumChgSq != 0.0) {
    double sr = sumChg/n/sqrt((sumChgSq/n-pow(sumChg/n,2))*n/(n-1))*sqrt(240);

    return List::create(Named("sumRet")=sumChg,
                      Named("sumRetSq")=sumChgSq,
                      Named("N")=n,
                      Named("sr")=sr);
  } else {
    return List::create(Named("sumRet")=sumChg,Named("sumRetSq")=sumChgSq,
                        Named("N")=n,Named("sr")=0.0);
  }
}

//' get cumu sharpe
//' @export
//' @param x numvec
//[[Rcpp::export]]
DataFrame getDayCumSharpeCpp(NumericVector x) {
  NumericVector m;
  NumericVector sd;
  NumericVector sr;
  NumericVector chgSq  = pow(x,2);

  double runningSum = 0.0;
  double runningSumSq = 0.0;
  int runningCount = 0;

  for(unsigned int i=0; i<x.size(); i++) {
    runningSum += x[i];
    runningSumSq += chgSq[i];
    runningCount ++;

    double meanSoFar = runningSum/runningCount;
    double meanSumSq = runningSumSq/runningCount;
    double sdSoFar = sqrt((meanSumSq-pow(meanSoFar,2))*runningCount/(runningCount-1));
    m.push_back(meanSoFar);
    sd.push_back(sdSoFar);
    sr.push_back(meanSoFar/sdSoFar*sqrt(240));
  }
  return DataFrame::create(Named("chg")=x, Named("mean") = m, Named("sd")=sd, Named("sr")=sr);
}

//' get cumu ytd sharpe
//' @export
//' @param x numvec
//[[Rcpp::export]]
DataFrame getYtdCumSharpeCpp(NumericVector x) {
  NumericVector m;
  NumericVector sd;
  NumericVector sr;
  NumericVector chgSq  = pow(x,2);

  double runningSum = 0.0;
  double runningSumSq = 0.0;
  int runningCount = 0;

  for(unsigned int i=0; i<x.size(); i++) {
    runningSum += x[i];
    runningSumSq += chgSq[i];
    runningCount ++;

    double meanSoFar = runningSum/runningCount;
    double meanSumSq = runningSumSq/runningCount;
    double sdSoFar = sqrt((meanSumSq-pow(meanSoFar,2))*runningCount/(runningCount-1));
    m.push_back(meanSoFar);
    sd.push_back(sdSoFar);
    sr.push_back(meanSoFar/sdSoFar*sqrt(252));
  }
  return DataFrame::create(Named("chg")=x, Named("mean") = m, Named("sd")=sd, Named("sr")=sr);
}

//' take a data frame and get percentile
//' @export
//' @param x dataframe
//[[Rcpp::export]]
double getPercentileCpp(DataFrame x) {
  NumericVector c = x["C"];
  NumericVector h = x["H"];
  NumericVector l = x["L"];

  double ma = max(c);
  double mi = min(l);
  double last = c[c.length()-1];
  return (last-mi)/(ma-mi);
}

//' calc sharpe
//' @export
//' @param x retList
//[[Rcpp::export]]
List calcSharpeCpp(NumericVector x) {
  double m = mean(x);
  NumericVector cumpro = cumprod((x+1));
  double cumpro1 = cumpro[cumpro.length()-1];
  double ma = max(x);
  double mi = min(x);
  double s = sd(x);
  NumericVector upVec = x[x>m];
  double ud = sd(upVec);
  NumericVector downVec = x[x<m];
  double dd = sd(downVec);


  return List::create(Named("m")=m,
                      Named("cum")=cumpro1,
                      Named("max")=ma,
                      Named("min") = mi,
                      Named("sd")= s,
                      Named("ud") = ud,
                      Named("dd")= dd);
}


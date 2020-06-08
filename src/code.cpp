#define ARMA_DONT_PRINT_ERRORS
#include <RcppArmadillo.h>
#include <cmath>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;


// Helper Functions
double coeffDet(const arma::colvec& y, const arma::colvec& yFit,  bool useIntercept);
double adjR(double Rsq, double k, double n, bool useIntercept);
double sigmaCalc(const arma::mat& x, const arma::colvec& res, const arma::mat& w);


// [[Rcpp::export]]
List fastLM(const arma::mat& x, const arma::colvec& y, const arma::mat& w, bool useIntercept) {

  // Compute coefficients and sigma quickly
  // Given predictor(s), response, weights, and a bool for if an intercept is present


  // (1) Regression Coefficients
  // Formula for regression coefficients:
  // B-hat-WLS = (x_t * w * x)^-1 * x_t * w * y


  arma::colvec coefs = arma::solve(x.t() * w * x, x.t() * w * y);


  // (2) Sigma


  // First, compute the residuals
  arma::colvec res = y - x * coefs;

  // Rcout the residuals and compare to y - coefs.t() * x


  // Sigma is equal to: sqrt[sum( w * (e^2) ) / ( sum(w) - rank(x) )]
  // Rcout << arma::accu(w) << '\n' << "\n" << arma::rank(x);


  // Use a function
  double sigma = sigmaCalc(x, res, w);


  // (3) R^2 and Adjusted R^2 (unweighted)


  // Use functions
  double Rsq = coeffDet(y, x * coefs, useIntercept);
  double adjRsq = adjR(Rsq, (double)x.n_cols, (double)x.n_rows, useIntercept);


  // Return the matrix of coefficients as a vector
  Rcpp::NumericVector coefVec = Rcpp::wrap(coefs);
  coefVec.attr("dim") = R_NilValue;


  return List::create(Named("coef") = coefVec, Named("sigma") = sigma,
                      Named("Rsq") = Rsq, Named("Adj.Rsq") = adjRsq);

}


double coeffDet(const arma::colvec& y, const arma::colvec& yFit, bool useIntercept) {

  // Goal: Compute R^2


  // Compute R^2 differently depending on if there's an intercept
  // Use 0 in place of mean(y) if there's no intercept


  double yAvg = 0;

  if (useIntercept == true) {
    yAvg = (1.0 / (double)y.n_rows) * (double)arma::accu(y);
  }


  // SSE = sum(y - y-hat)^2
  // SSTO = sum(y - y-bar)^2
  // R^2 = 1 - SSE / SSTO


  return 1.0 - (double)arma::accu(arma::pow(y - yFit, 2.0)) / (double)arma::accu(arma::pow(y - yAvg, 2.0));

}


double adjR(double Rsq, double k, double n, bool useIntercept) {

  // Goal: Get the Adjusted R^2


  // Formula:
  // 1.0 - (1.0 - Rsq) * (x.n_rows - useIntercept) / (x.n_rows - x.n_cols)
  // k is x.n_cols and n is x.n_rows


  // There's a dependency on the presence of an intercept
  double interceptContribution = 0.0;

  if (useIntercept == true) {
    interceptContribution = 1.0;
  }


  // Compute the result and return it
  return 1.0 - ((1.0 - Rsq) * (n - interceptContribution) / (n - k));

}


double sigmaCalc(const arma::mat& x, const arma::colvec& res, const arma::mat& w) {

  // Goal: Get sigma


  // Formula: sqrt[sum( w * (e^2) ) / ( sum(w) - rank(x) )]


  // Compute the numerator and denominator separately
  // and return the ratio
  double bot = (double)arma::accu(w) - (double)arma::rank(x);
  double top = (double)arma::accu(w * arma::pow(res, 2.0));


  return sqrt(top / bot);

}


// References used
// https://github.com/RcppCore/RcppArmadillo/blob/master/src/fastLm.cpp
// https://cran.r-project.org/web/packages/RcppArmadillo/RcppArmadillo.pdf
// https://cran.r-project.org/web/packages/RcppArmadillo/vignettes/RcppArmadillo-intro.pdf
// https://gseacademic.harvard.edu/~willetjo/pdf%20files/Willett_Singer_AS88.pdf
// http://arma.sourceforge.net/docs.html
// https://ncss-wpengine.netdna-ssl.com/wp-content/themes/ncss/pdf/Procedures/NCSS/Linear_Regression_and_Correlation.pdf
// https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/lm.R
// https://people.richland.edu/james/ictcm/2004/multiple.html#:~:text=That%20is%2C%20the%20df(Regression,%3D%20n%20-%20k%20-%201.&text=Even%20the%20hypothesis%20test%20here%20is%20an%20extension%20of%20simple%20linear%20regression
// https://pubs.usgs.gov/tm/tm4a8/pdf/TM4-A8.pdf
// https://www.stat.purdue.edu/~bacraig/notes525/topic15.pdf




// Code for the weighted R^2 was written based on lm()'s version
// But their usage of the weights is different, so this function needs modification
// double weightedRsq(const arma::colvec& y, const arma::colvec& yFit, bool useIntercept, const arma::mat& w);
// double weightedRsq(const arma::colvec& y, const arma::colvec& yFit, bool useIntercept, const arma::mat& w) {
//
//   // Goal: Get the weighted R^2
//
//
//   // Code adapted from the summary() function for lm
//   // in the stats package
//
//
//   // MSS is affected by the presence of an intercept
//   double mss;
//
//   if (useIntercept == true) {
//
//     double m = (double)arma::accu(w * yFit / (double)arma::accu(w));
//     mss = (double)arma::accu(w * arma::pow(yFit - m, 2.0));
//
//   } else {
//
//     mss = (double)arma::accu(w * arma::pow(yFit, 2.0));
//
//   }
//
//
//   // Get RSS too
//   double rss = (double)arma::accu(w * arma::pow(y - yFit, 2.0));
//
//
//   // Return the result
//   return mss / (mss + rss);
//
// }
// //
// // (4) Weighted R^2
// // (Only compute a weighted R^2 if weights are used)
// double weightRsq = -5.0;
//
//
// // Note: approx_equal() checks if each value in the matrix
// // is equal to the corresponding value in the other matrix
// // (With a tolerance for differences of +/- 10e-6)
//
//
// if (!approx_equal(w, arma::eye(w.n_rows, w.n_cols), "absdiff", pow(10.0, -6.0))) {
//   weightRsq = weightedRsq(y, x * coefs, useIntercept, w);
// }
//
//
// // Return all computed values
// if (weightRsq == -5.0) {
//
// } else {
//   return List::create(Named("coef") = coefVec, Named("sigma") = sigma,
//                       Named("Rsq") = Rsq, Named("Adj.Rsq") = adjRsq,
//                       Named("Weighted.Rsq") = weightRsq);
// }
#include <Rcpp.h>
#include <math.h>
#include <algorithm>
#include <iostream>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix innerLoop(int n, int NumberOfIterations, double Beta, double Gamma, double Del, double Alpha, double K, NumericVector x,
               NumericVector vw, NumericVector y){
  int m = NumberOfIterations * 5 * n;
  NumericVector xnew (m);
  NumericVector vwnew (m);
  NumericVector ynew (m);
  for(int i=0; i < 100; i ++){
    xnew[i] = x[1];
    vwnew[i] = vw[1];
    ynew[i] = y[1];
    
  }
  for(int i=100; i < m; i ++){
    float max_val = std::max(0.0, 1 - xnew[i-1] / K );
    float power_y = std::pow(ynew[i-1],Alpha);
    xnew[i] = xnew[i-1] + Beta * power_y * max_val * Del;
    vwnew[i]= vwnew[i-1] + Gamma* ynew[i-1] * Del;
    ynew[i] = std::max(0.0,xnew[i]-vwnew[i]);
  }
  NumericMatrix results(3,m);
  results.row(0) = xnew;
  results.row(1) = vwnew;
  results.row(2) = ynew;
  return results;
}
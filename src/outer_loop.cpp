#include <Rcpp.h>
#include <math.h>
#include <algorithm>
#include <iostream>
using namespace Rcpp;

// [[Rcpp::export]]

NumericMatrix innerLoop(int n, 
                        int NumberOfIterations, 
                        double Beta, double Gamma, double Del, double Alpha, double K, double x,
                        double vw, double y){
  int m = NumberOfIterations * 5 * n;
  NumericVector xnew (m);
  NumericVector vwnew (m);
  NumericVector ynew (m);
  //Initilization
  for(int i=0; i < NumberOfIterations; i ++){
    xnew[i] = x;
    vwnew[i] = vw;
    ynew[i] = y;
    
  }
  for(int i=NumberOfIterations; i < m; i ++){
    float max_val = std::max(0.0, 1 - xnew[i-1] / K);
    float power_y = std::pow(ynew[i-1],Alpha);
    xnew[i] = xnew[i-1] + Beta * power_y * max_val * Del;
    vwnew[i]= vwnew[i-1] + Gamma * ynew[i-1] * Del;
    ynew[i] = std::max(0.0,xnew[i]-vwnew[i]);
  }
  NumericMatrix results(3,m);
  results.row(0) = xnew;
  results.row(1) = vwnew;
  results.row(2) = ynew;
  return results;
}

NumericVector timesTwo(int CalibrationLoops,
                       int n, 
                       int PartitionParameter,
                       int NumberOfIterations, double Beta, double Gamma, double Del, double Alpha, double K, NumericVector X,
                       NumericVector VW, NumericVector Y) {
  NumericVector beta_hat (CalibrationLoops);
  NumericVector gamma_hat (CalibrationLoops);
  for(int j=0; j < CalibrationLoops; j ++){
  {
    NumericMatrix res = innerLoop(n, PartitionParameter, Beta ,Gamma,
                     Del,Alpha,K = K, X[1], VW[1], Y[1]);
    NumericVector x = res.row(0);
    NumericVector vw = res.row(1);
    NumericVector y = res.row(2);
//random time transformation
//The events where the pde solution equals to the emphirical values
    NumericMatrix T1(n,2);
    for(int i=0; i < n; i ++){
      T1(i,1) = sum(x<=X[i]);
      T1(i,2) = sum(vw<=VW[i]);
    }
    T1=T1*Del;
    Beta = Beta * T1(n,1)/n ;
    Gamma = Gamma * T1(n,2)/n;
    beta_hat[j] = Beta;
    gamma_hat[j] = Gamma;
    }
  }
  NumericVector results (2);
  results[1] = beta_hat[CalibrationLoops - 1];
  results[2] = gamma_hat[CalibrationLoops - 1];
  return results;
}

  
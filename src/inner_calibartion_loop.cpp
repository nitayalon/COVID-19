#include <Rcpp.h>
#include <math.h>
#include <algorithm>
#include <iostream>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix innerLoop(int n, 
                        int NumberOfIterations, double Beta, double Gamma, double Del, double Alpha, double K, double x,
                        double vw, double y){
  int m = NumberOfIterations * 5 * n;
  NumericVector xnew(m);
  NumericVector vwnew(m);
  NumericVector ynew(m);
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

// [[Rcpp::export]]
List InnerCalibrationLoopRcpp(
              double Beta,
              double Gamma,
              double K, 
              int n,
              int NumberOfIterations, 
               NumericVector X, 
               NumericVector VW, 
               NumericVector Y, 
               double Alpha,
               double Del,
               int calibration_loops){
  List results(5);
  NumericVector beta_hat(calibration_loops);
  NumericVector gamma_hat(calibration_loops);
  NumericMatrix T1 (n,2);
  
  for(int j = 0; j < calibration_loops; j++){
    NumericMatrix inner_loop_results = innerLoop(n, NumberOfIterations, Beta, Gamma, Del, Alpha, K, X(0), VW(0),Y(0));
    NumericVector x = inner_loop_results(0 , _);
    NumericVector vw = inner_loop_results(1 , _);
    NumericVector y = inner_loop_results(2 , _);
    // random time transformation
    // The events where the pde solution equals to the emphirical values
    for(int i=0; i < n; i++)
    {
      T1(i,0) = sum(x<=X(i));
      T1(i,1) = sum(vw<=VW(i));
    }
    for(int i=0; i < n; i++)
    {
      T1(i,0) = T1(i,0) + (X[i]-x[T1(i,0)])/(x[T1(i,0)+1]-x[T1(i,0)]);
      T1(i,1) = T1(i,1) + (VW[i]-vw[T1(i,1)])/(vw[T1(i,1)+1]-vw[T1(i,1)]);
    }
    NumericVector blah = (X-x[T1(_,0)])/(x[T1(_,0)+1]-x[T1(_,0)]);
    NumericVector x_rtt_correction = pmax(blah, 0);
    NumericVector vw_rtt_correction = pmax((VW-vw[T1(_,1)])/(vw[T1(_,1)+1]-vw[T1(_,1)]), 0);
    T1(_,0) = T1(_,0) + x_rtt_correction;
    T1(_,1) = T1(_,1) + vw_rtt_correction; 
    T1=T1 * Del;
    Beta = Beta * T1((n-1), 0) / n;
    Gamma = Gamma * T1((n-1), 1) / n;
    beta_hat[j] = Beta;
    gamma_hat[j] = Gamma;
  }
  NumericMatrix inner_loop_results = innerLoop(n, NumberOfIterations, Beta, Gamma, Del, Alpha, K, X(0), VW(0),Y(0));
  results[0] = Beta;
  results[1] = Gamma;
  results[2] = beta_hat;
  results[3] = gamma_hat;
  results[4] = T1;

  return results;
}
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

List InnerCalibrationLoop(double K, int n,int NumberOfIterations, 
               NumericVector X, 
               NumericVector VW, 
               NumericVector Y, 
               double Alpha,
               NumericVector X_Middle, 
               NumericVector Y_Middle,
               int PartitonParameter,
               double Del,
               int calibration_loops = 15){
  List results(5);
  NumericVector power_y (Y_Middle.size());
  for(int i = 0 ; i < Y_Middle.size() ; i ++){
    power_y(i) = std::pow(Y_Middle(i), Alpha);
  }
  double beta = (X(n)-X(1)) / sum(pmax(1,power_y  - power_y(0)) * (1 - (X_Middle-X_Middle(0)) / K));
  double gamma = (VW(n)-VW(1)) / sum(pmax(1,Y_Middle-Y_Middle(0)));
  NumericMatrix beta_hat(n,1);
  NumericMatrix gamma_hat(n,1);
  NumericMatrix T_final (n,2);
  NumericMatrix T1 (n,2);
  
  for(int j = 0; j < calibration_loops; j++){
    NumericMatrix inner_loop_results = innerLoop(n, NumberOfIterations, beta, gamma, Del, Alpha, K, X(0), VW(0),Y(0));
    NumericVector x = round(inner_loop_results(_ , 0),7);
    NumericVector vw = round(inner_loop_results(_ , 1),7);
    NumericVector y = round(inner_loop_results(_ , 2),7);
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
    T1=T1 * Del;
    beta = beta * T1(n-1,0)/n;
    gamma = gamma * T1(n-1,1)/n;
    beta_hat[j] = beta;
    gamma_hat[j] = gamma;
  }
  results[0] = beta;
  results[1] = gamma;
  results[2] = beta_hat;
  results[3] = gamma_hat;
  
  return results;
}
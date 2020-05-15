predictCovidTrajectory <- function(number_of_prediction_days,){
  
  res <- innerLoop(n, partition_parameter,Beta = beta,Gamma = gamma,
                   Del = del,Alpha = alpha,
                   K = K,x = X[1], vw = VW[1], y = Y[1])  
}
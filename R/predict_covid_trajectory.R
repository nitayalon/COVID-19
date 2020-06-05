predictCovidTrajectory <- function(environment_data,K, alpha = NULL){
  partition_parameter = 100
  del = 1/partition_parameter 
  X = environment_data$X
  VW = environment_data$VW
  Y = environment_data$Y
  X_middle = environment_data$X_middle
  Y_middle = environment_data$Y_middle
  n = environment_data$n
  if(is.na(alpha)){
    alpha_grid = seq(0.05,0.6,length.out = 40)
    grid_search_results <- gridSearchMethodHelper(alpha_grid,K,X,VW,Y, del,Y_middle,X_middle,partition_parameter)
    alpha <- alpha_grid[which.max(grid_search_results$alpha_profile)]
  }
  
  inner_calibration_loop <- InnerCalibrationLoop(K, n, X,VW,Y,
                                                 alpha,Y_middle,X_middle,partition_parameter,del)
  beta <- inner_calibration_loop$beta
  gamma <- inner_calibration_loop$gamma
  beta_hat <- inner_calibration_loop$beta_hat
  gamma_hat <- inner_calibration_loop$gamma_hat
  
  res <- innerLoop(n, partition_parameter,Beta = beta,Gamma = gamma,
                   Del = del,Alpha = alpha,
                   K = K,x = X[1], vw = VW[1], y = Y[1])
  
  x = round(res[1,],7)
  vw = round(res[2,],7)
  y = round(res[3,],7)
  return(list(alpha = alpha,
              beta = beta,
              gamma = gamma,
              x = x,
              vw = vw,
              y = y))
}
RunFullCycle <- function(national_data, starting_day, environment_parameter_list = NULL){
  partition_parameter = 100
  del = 1/partition_parameter # dt
  asympt = c()
  LOGL = VAR1 = OBJ = OBJB = c()
  calibration_loops = 15
  hhh_upper_limit = 400
  gamma_hat = beta_hat = matrix(nrow = calibration_loops, ncol = hhh_upper_limit)
  alpha = 0.5
  environment_data <- DataLoader(national_data,starting_day)
  X = environment_data$X
  VW = environment_data$VW
  Y = environment_data$Y
  X_middle = environment_data$X_middle
  Y_middle = environment_data$Y_middle
  n = environment_data$n
  NNN = floor(max(X) / 100) #What is the grid of K
  K_grid_results <- lapply(1:hhh_upper_limit, function(i){
    GridSearchForK(i,X,VW,Y,alpha,Y_middle,X_middle, NNN)
  })
  COV <- lapply(K_grid_results, function(x){x$COV})
  OBJ <- sapply(K_grid_results, function(x){x$OBJ})
  OBJB <- sapply(K_grid_results, function(x){x$OBJB})
  optimal_ks <- computeOptimalK(OBJB, OBJ, NNN, X)
  if(is.null(optimal_ks$single_dim_CI) || is.null(optimal_ks$two_dim_CI))
  {
    K_max_list <- sapply(K_grid_results, function(x){x$K})
    K_max <- K_max_list[length(K_max_list)]
    one_dim_params <- InnerCalibrationLoop(K_max,n, X,VW,Y,alpha,Y_middle,X_middle)
    cov_matrix = COV[[400]]
    std = mean(diag(COV[[400]]))
    beta = one_dim_params$beta
    gamma = one_dim_params$gamma * 100
    
  }
  else{
    one_dim_params <- lapply(optimal_ks$single_dim_CI, function(k){
      InnerCalibrationLoop(k,n, X,VW,Y,alpha,Y_middle,X_middle)})
    cov_matrix = COV[[400]]
    std = mean(diag(COV[[400]]))
    beta = one_dim_params[[2]]$beta
    gamma = one_dim_params[[2]]$gamma * 100
  }
  return(list(cov_matrix = cov_matrix,
              std = std,
              beta = beta,
              gamma = gamma))
}
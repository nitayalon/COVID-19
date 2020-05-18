gridSearchMainFunction <- function(national_data, starting_day, cut_off_day = NULL, prediction_period = NULL,
                         environment_parameter_list = NULL){
  partition_parameter = 100
  del = 1/partition_parameter # dt
  hhh_upper_limit = 400
  environment_data <- DataLoader(national_data,starting_day, cut_off_day)
  X = environment_data$X
  VW = environment_data$VW
  Y = environment_data$Y
  X_middle = environment_data$X_middle
  Y_middle = environment_data$Y_middle
  n = environment_data$n
  NNN = floor(max(X) / 100) #What is the grid of K
  hhh <- 1:hhh_upper_limit
  alpha_grid = seq(0.4,0.6,length.out = 100)
  k_grid <- max(X) + hhh * NNN
  grid_parameters <- expand.grid(alpha_grid,k_grid)
  print("Starting grid search")
  grid_search_results <- list()
  for(i in 1:nrow(grid_parameters)){
    grid_search_results[[i]] <- GridSearchInnerLoop(X,VW,Y,grid_parameters$Var1[i],
                                                    grid_parameters$Var2[i],del,Y_middle,X_middle,partition_parameter)
    if(i %% 500 == 0){print(sprintf('Iteration number %s',i))}
  }
  COV <- lapply(grid_search_results, function(x){x$COV})
  OBJ <- sapply(grid_search_results, function(x){x$OBJ})
  OBJB <- sapply(grid_search_results, function(x){x$OBJB})
  # optimal_ks <- computeOptimalK(OBJB, OBJ, NNN, X)
  # final_results <- computeMLEforK(optimal_ks$single_dim_CI, optimal_ks$two_dim_CI,
  #                                 grid_search_results, n, X, VW, Y, alpha, Y_middle, X_middle,hhh_upper_limit)
  return(list(grid_parameters = grid_parameters,
              environment_data = environment_data,
              COV_matrix = COV,
              OBJ = OBJ,         
              OBJB = OBJB,         
              X = X,
              VW = VW,
              Y = Y,
              X_middle = X_middle,
              Y_middle = Y_middle,
              final_results = grid_search_results))
}
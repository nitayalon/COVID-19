computeMLEforK <- function(single_dim_CI, two_dim_CI,
                           K_grid_results, n, X,VW,Y,alpha,Y_middle,X_middle,
                           hhh_upper_limit){
  if(is.null(single_dim_CI) || is.null(two_dim_CI))
  {
    K_max_list <- sapply(K_grid_results, function(x){x$K})
    K_max <- K_max_list[length(K_max_list)]
    one_dim_params <- InnerCalibrationLoop(K_max,n, X,VW,Y,alpha,Y_middle,X_middle)
  }
  else{
    one_dim_params <- lapply(single_dim_CI, function(k){
      InnerCalibrationLoop(k,n, X,VW,Y,alpha,Y_middle,X_middle)})
    K_max <- single_dim_CI[2]
  }
  return(list(single_dim_CI = single_dim_CI,
              one_dim_params = one_dim_params))
}
InnerCalibrationLoop <- function(K,n,X,VW,Y,
                                 alpha,Y_middle,X_middle, partition_parameter, del, calibration_loops = 15)
{
  beta <- (X[n] - X[1]) / sum(pmax(1,Y_middle ^ alpha)*(1 - (X_middle) / K))
  gamma <- (VW[n] - VW[1]) / sum(pmax(1,Y_middle))
  beta_hat = gamma_hat = c()
  T1 = T_final = matrix(0, nrow = n, ncol = 2)
  # inner_calibration_loop <- InnerCalibrationLoopRcpp(beta, gamma, K, n, partition_parameter, X, VW ,Y, alpha,
  #                                                    del,calibration_loops)
  for(j in 1:calibration_loops)
  {
    res <- innerLoop(n, partition_parameter, Beta = beta,Gamma = gamma,
                     Del = del,Alpha = alpha,
                     K = K,x = X[1], vw = VW[1], y = Y[1])

    x = round(res[1,],7)
    vw = round(res[2,],7)
    y = round(res[3,],7)
    # random time transformation
    # The events where the pde solution equals to the emphirical values
    
    for(i in 1:n)
    {
      # T1[i,1] = sum(round(x,5)<=round(X[i],5))
      # T1[i,2] = sum(round(vw,5)<=round(VW[i],5))
      T1[i,1] = sum(x<=X[i])
      T1[i,2] = sum(vw<=VW[i])
    }
    x_rtt_correction <- pmax((X-x[T1[,1]])/(x[T1[,1]+1]-x[T1[,1]]), 0, na.rm = T)
    vw_rtt_correction <- pmax((VW-vw[T1[,2]])/(vw[T1[,2]+1]-vw[T1[,2]]), 0, na.rm = T)
    T1[,1] = T1[,1] + c(x_rtt_correction[1:(n-1)],0) 
    T1[,2] = T1[,2] + c(vw_rtt_correction[1:(n-1)],0) 
    T1=T1 * del
    beta = beta * T1[n,1]/n
    gamma = gamma * T1[n,2]/n
    beta_hat[j] = beta
    gamma_hat[j] = gamma
  }
  inner_calibration_loop = list(beta = beta,
                                gamma = gamma,
                                beta_hat = beta_hat,
                                gamma_hat = gamma_hat,
                                T1 = T1)
  T_final=inner_calibration_loop[[5]]
  TT=diff(T_final)-1
  COV=t(TT)%*%TT/(n-1)
  AD = inner_calibration_loop[[1]] * Y_middle ^ alpha * pmax(0, 1 - X_middle / K)
  BD = inner_calibration_loop[[2]] * Y_middle
  OBJ1=sum(log(AD)+log(BD)) #single figure - denom
  OBJ=OBJ1+(n/2)*log(det(COV)) # likelihood when we use two BM's
  OBJB=sum(log(AD))+(n/2)*log(COV[1,1]) # BM for single component for X only
  
  return(list(
              TT = TT,
              COV = COV,
              OBJ = OBJ,
              OBJB = OBJB,
              beta = inner_calibration_loop[[1]],
              gamma = inner_calibration_loop[[2]],
              beta_hat = inner_calibration_loop[[3]],
              gamma_hat = inner_calibration_loop[[4]]))
}
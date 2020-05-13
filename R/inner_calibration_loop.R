InnerCalibrationLoop <- function(K,n,X,VW,Y,
                                 alpha,Y_middle,X_middle)
{
  beta <- (X[n]-X[1]) / sum(pmax(1,Y_middle ^ alpha - Y_middle[1] ^ alpha)*(1 - (X_middle-X_middle[1]) / K))
  gamma <- (VW[n]-VW[1]) / sum(pmax(1,Y_middle-Y_middle[1]))
  beta_hat <- gamma_hat <- c()
  T_final = T1 = matrix(0, ncol = 2, nrow = n)
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
      T1[i,1] = sum(x<=X[i])
      T1[i,2] = sum(vw<=VW[i])
    }
    T1=T1*del
    beta = beta * T1[n,1]/n
    gamma = gamma * T1[n,2]/n
    beta_hat[j] = beta
    gamma_hat[j] = gamma
  }
  return(list(beta = beta,
              gamma = gamma,
              beta_hat = beta_hat,
              gamma_hat = gamma_hat))
}
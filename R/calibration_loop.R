CalibrationLoop <- function(X,VW,Y,alpha,Y_M,X_M, grid, internatl_iteration) {
  K = max(X) + hhh * NNN
  beta <- (X[n]-X[1]) / sum(pmax(1,Y_middle ^ alpha - Y_middle[1] ^ alpha)*(1 - (X_middle-X_middle[1]) / K))
  gamma <- (VW[n]-VW[1]) / sum(pmax(1,Y_middle-Y_middle[1]))
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
    beta_hat[j,hhh] = beta
    gamma_hat[j,hhh] = gamma
  }
  res <- innerLoop(n, partition_parameter,Beta = beta,Gamma = gamma,
                   Del = del,Alpha = alpha,
                   K = K,x = X[1], vw = VW[1], y = Y[1])
  
  x = round(res[1,],7)
  vw = round(res[2,],7)
  y = round(res[3,],7)
  for(i in 1:n)
  {
    T1[i,1] = sum(x<=X[i])
    T1[i,2] = sum(vw<=VW[i])
  }
  T_final=T1*del
  TT=diff(T_final)-1
  print(colMeans(TT))
  COV=t(TT)%*%TT/(n-1)
  # The solution deviratives - Jacobi matrix
  AD = beta * Y_middle ^ alpha * pmax(0, 1 - X_middle / K)
  BD = gamma * Y_middle
  
  OBJ1=sum(log(AD)+log(BD)) #single figure - denom
  OBJ[hhh]=OBJ1+(n/2)*log(det(COV)) # likelihood when we use to BM's
  LOGL[hhh]=log(det(COV))
  VAR1[hhh]=mean(diag(COV))
  OBJB[hhh]=sum(log(AD))+(n/2)*log(COV[1,1]) # BM for single component for X only
  return(list(K = K,
              ))
}
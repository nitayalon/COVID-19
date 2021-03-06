GridSearchInnerLoop <- function(X,VW,Y,
                           alpha,K,del,
                           Y_middle,X_middle,partition_parameter){
  n = length(X)
  T_final = T1 = matrix(0, ncol = 2, nrow = n)
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
  T1[1,1] = sum(round(x,5)<=round(X[1],5))
  T1[1,2] = sum(round(vw,5)<=round(VW[1],5))
  for(i in 2:n)
  {
    T1[i,1] = sum(x<=X[i])
    T1[i,2] = sum(vw<=VW[i])
  }
  x_rtt_correction <- pmax((X-x[T1[,1]])/(x[T1[,1]+1]-x[T1[,1]]), 0, na.rm = T)
  vw_rtt_correction <- pmax((VW-vw[T1[,2]])/(vw[T1[,2]+1]-vw[T1[,2]]), 0, na.rm = T)
  T1[,1] = T1[,1] + c(x_rtt_correction[1:(n-1)],0)
  T1[,2] = T1[,2] + c(vw_rtt_correction[1:(n-1)],0) 
  T_final=T1*del
  TT=diff(T_final)-1
  COV=t(TT)%*%TT/(n-1)
  # The solution deviratives - Jacobi matrix
  AD = beta * Y_middle ^ alpha * pmax(0, 1 - X_middle / K)
  BD = gamma * Y_middle
  
  OBJ1=sum(log(AD)+log(BD)) #single figure - denom
  OBJ=OBJ1+(n/2)*log(det(COV)) # likelihood when we use two BM's
  if(is.na(OBJ1) || is.na(OBJ)){browser()}
  LOGL=log(det(COV))
  VAR1=mean(diag(COV))
  OBJB=sum(log(AD))+(n/2)*log(COV[1,1]) # BM for single component for X only
  return(list(TT = TT,
              T_final = T_final,
              K = K,
              beta = beta,
              gamma = gamma,
              beta_hat = beta_hat,
              gamma_hat = gamma_hat,
              COV = COV,
              OBJ = OBJ,
              LOGL = LOGL,
              VAR1 = VAR1,
              OBJB = OBJB))
}
innerLoopR <- function(n, 
                      NumberOfIterations,  Beta,  Gamma,  Del,  Alpha,  K,  x,
                        vw,  y){
  m = NumberOfIterations * 5 * n
  ynew = vwnew = xnew = c()
  xnew[1:NumberOfIterations] = x
  vwnew[1:NumberOfIterations] = vw
  ynew[1:NumberOfIterations] = y
  for(i in (NumberOfIterations+1):m)
    {
    max_val = max(0.0, 1 - xnew[i-1] / K)
    power_y = ynew[i-1] ^ Alpha
    xnew[i] = xnew[i-1] + Beta * power_y * max_val * Del
    vwnew[i]= vwnew[i-1] + Gamma * ynew[i-1] * Del
    ynew[i] = max(0.0,xnew[i]-vwnew[i])
  }
  results = matrix(nrow = 3,ncol = m)
  results[1,] = xnew
  results[2,] = vwnew
  results[3,] = ynew
  return(results)
}
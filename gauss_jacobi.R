gauss_jacobi = function(A, b, n, x0, e){
  alfas = numeric()
  sum = 0
  for(i in 1:n){ # verificando se a matriz é diagonal dominante
    for(j in 1:n){
      if(i != j)
        sum = sum + abs(A[i, j])
    }
    alfas[i] = sum / abs(A[i, i])
    if(max(alfas) < 1)
      cat("👍\n")
  }
  
  x1 = numeric() # próximo x
  
  while(TRUE){
    for(i in 1:n){
      sum = 0
      for(j in 1:n){
        if(i != j)
          sum = sum + (A[i, j]*x0[j])
      }
      x1[i] = (b[i] - sum) / A[i, i]
    }
  
    # cat(x1, '\n') # exibindo x_i a cada iteração
    
    d = numeric()
    for(i in 1:n)
      d[i] = abs(x1[i] - x0[i])
    if(max(d) < e)
      break
    
    x0 = x1
  }
  return(x1)
}

if(FALSE){
  n = 3
  A = matrix(0, nrow=3, ncol=3)
  A[1,] = c(10, 2,  1)
  A[2,] = c(1,  5,  1)
  A[3,] = c(2,  3, 10)
  b = c(7, -8, 6)
  x0 = c(0.7, -1.6, 0.6)
  e = 0.01

  gauss_jacobi(A, b, n, x0, e)
}

if(TRUE){
  n = 4
  A = matrix(nrow=n, ncol=n)
  A[1,] = c(5, -2, 0, -1)
  A[2,] = c(1,  8, 2,  0)
  A[3,] = c(0,  3, 7, -2)
  A[4,] = c(0,  0, 3,  7)
  b = c(2, -11, -27, 29)
  x0 = c(1, 1, 1, 1)
  e = 0.001
  
  gauss_jacobi(A, b, n, x0, e)
}
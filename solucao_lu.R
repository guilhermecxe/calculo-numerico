solucao_lu = function(A, b, n){
  LUb = fatoracao_lu(A, b, n)
  L = LUb[[1]]
  U = LUb[[2]]
  b = LUb[[3]]
  
  x = sistema_triangular_superior(U, b, n)
  
  return (x)
}

if(FALSE){
  n = 3
  A = matrix(0, nrow=n, ncol=n)
  A[1,] = c(3, 2,  4)
  A[2,] = c(1, 1,  2)
  A[3,] = c(4, 3, -2)
  b = c(1, 2, 3)
  solucao_lu(A, b, n)
}

if(FALSE){
  n = 3
  A = matrix(0, nrow=n, ncol=n)
  A[1,] = c(3, -2,  1)
  A[2,] = c(1,  2, -1)
  A[3,] = c(2,  1,  3)
  b = c(9, -5, 6)
  solucao_lu(A, b, n) # 1, -2, 2
}

if(TRUE){
  n = 5
  A = matrix(0, nrow=n, ncol=n)
  A[1,] = c(2, -3,  4,  5, -6)
  A[2,] = c(1,  2, -1,  3,  4)
  A[3,] = c(3, -4,  1,  2, -1)
  A[4,] = c(5,  2, -2,  4,  3)
  A[5,] = c(-2, 3,  2, -1,  5)
  b = c(-21, 29, -2, 30, 24)
  solucao_lu(A, b, n) # 1, 1, 0, 2, 5
}
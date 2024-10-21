fatoracao_lu = function(A, b, n){
  Ms = list() # lista das matrizes M
  m = numeric()
  
  for(j in 1:(n-1)){
    M = diag(n) # matriz identidade de tamanho n
    for(i in (j+1):n){
      m = A[i, j] / A[j, j]
      M[i, j] = -m
    }
    Ms[[length(Ms)+1]] = M # adicionando M à lista de matrizes
    A = M %*% A
    b = M %*% b
  }
  
  L = solve(Ms[[1]]) # solve calcula a inversa
  for (i in 2:length(Ms))
    L = L %*% solve(Ms[[i]])
  
  U = A

  #print(L)
  #print(U)
  #print(L %*% U) # se deu certo, este resultado é igual a primeira A
  return (list(L, U, b))
}

if(FALSE){
  n = 3
  A = matrix(0, nrow=n, ncol=n)
  A[1,] = c(3, 2,  4)
  A[2,] = c(1, 1,  2)
  A[3,] = c(4, 3, -2)
  b = c(1, 2, 3)
  LUb = fatoracao_lu(A, b, n)
  L = LUb[[1]]
  U = LUb[[2]]
  b = LUb[[3]]
  print(L)
  print(U)
  print(b)
}

if(TRUE){
  n = 5
  A = matrix(0, nrow=n, ncol=n)
  A[1,] = c(2, -3,  4,  5, -6)
  A[2,] = c(1,  2, -1,  3,  4)
  A[3,] = c(3, -4,  1,  2, -1)
  A[4,] = c(5,  2, -2,  4,  3)
  A[5,] = c(-2, 3,  2, -1,  5)
  b = c(0, 0, 0, 0, 0) # estou dando um b qualquer
  LUb = fatoracao_lu(A, b, n)
  L = LUb[[1]]
  U = LUb[[2]]
  print(L)
  print(U)
}









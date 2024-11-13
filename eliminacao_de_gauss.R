# Este método consiste em transformar a matriz dos coeficientes
# do sistema de equações em uma matriz triangular superior
# e resolver este pelo método específico do sistema triangular
# superior

eliminacao_de_gauss = function(a, b, n){
  m = numeric()
  for(j in 1:(n-1)){
    for(i in (j+1):n){
      m[i] = a[i, j] / a[j, j]
      a[i,] = a[i,] - (m[i] * a[j,])
      b[i] = b[i] - (m[i]*b[j])
    }
    cat("\n\nA[", j, "]:\n")
    print(a)
    cat("b[", j, "]:", b)
  }
  return(sistema_triangular_superior(a, b, n))
}

if(FALSE){
  n = 3
  a = matrix(c(3, 1, 4, 2, 1, 3, 4, 2, -2), nrow = 3, ncol = 3)
  b = c(1, 2, 3)
  
  print(eliminacao_de_gauss(a, b, n)) # -3 5 0
}

if(TRUE){
  # Questão 1 da Prova 2 antiga
  n = 5
  A = matrix(0, nrow=n, ncol=n)
  A[1,] = c( 2, -3,  4,  5, -6)
  A[2,] = c( 1,  2, -1,  3,  4)
  A[3,] = c( 3, -4,  1,  2, -1)
  A[4,] = c( 5,  2, -2,  4,  3)
  A[5,] = c(-2,  3,  2, -1,  5)
  b = c(-21, 29, -2, 30, 24)
  cat('\014')
  x = eliminacao_de_gauss(A, b, n)
  cat("\n", x)
}

if(TRUE){
  # Questão 1 da Prova 2 antiga
  n = 5
  A = matrix(0, nrow=n, ncol=n)
  A[1,] = c( 2, -3,  4,  5, -6)
  A[2,] = c( 1,  2, -1,  3,  4)
  A[3,] = c( 3, -4,  1,  2, -1)
  A[4,] = c( 5,  2, -2,  4,  3)
  A[5,] = c(-2,  3,  2, -1,  5)
  b = c(-21, 29, -2, 30, 24)
  cat('\014')
  x = eliminacao_de_gauss(A, b, n)
  cat("\n", x)
}
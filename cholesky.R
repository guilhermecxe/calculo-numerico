cholesky_g = function(A, b, n){
  G = matrix(0, nrow=3, ncol=3)

  G[1, 1] = A[1, 1]^(1/2)

  for(i in 2:n){
    G[i, 1] = A[i, 1] / G[1, 1]
  }

  for(k in 2:(n-1)){
    sum = 0
    for(j in 1:(k-1))
      sum = sum + G[k, j]^2
  
    r = A[k, k] - sum
    G[k, k] = r^(1/2)
  
    for(i in (k+1):n){
      sum = 0
      for(j in 1:(k-1))
        sum = sum + G[i, j] * G[k, j]
      G[i, k] = (A[i, k] - sum)/G[k, k]
    }
  }
  sum = 0
  for(j in 1:(n-1))
    sum = sum + G[n, j]^2

  r = A[n, n] - sum
  G[n, n] = r^(1/2)
  
  return(G)
}

n = 3

A = matrix(0, nrow=3, ncol=3)
A[1,] = c(1, 2,  2)
A[2,] = c(2, 5,  1)
A[3,] = c(2, 1, 14)

b = c(-3, -1, -23)

G = cholesky_g(A, b, n)

G_t = t(G)

G %*% G_t # == A

y = sistema_triangular_inferior(G, b, n)

x = sistema_triangular_superior(G_t, y, n) ## Resposta


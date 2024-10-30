sistema_triangular_inferior = function(a, b, n){
  x = numeric()
  x[1] = b[1] / a[1, 1]
  for(k in 2:(n)){
    S = 0
    for(j in 1:(k-1))
      S = S + a[k, j] * x[j]
    x[k] = (b[k] - S)/a[k, k]
  }
  return(x)
}

n = 3

A = matrix(0, nrow=3, ncol=3)
A[1,] = c(2,  0, 0)
A[2,] = c(3,  1, 0)
A[3,] = c(5,  2, 1)

b = c(2, 4, 6)

sistema_triangular_inferior(A, b, n) # 1 1 -1


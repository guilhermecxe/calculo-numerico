n = 3
a = matrix(c(3, 1, 4, 2, 1, 3, 4, 2, -2), nrow = 3, ncol = 3)
b = c(1, 2, 3)
m = numeric()

eliminacao_de_gauss = function(a, b, n){
  for(j in 1:(n-1)){
    for(i in (j+1):n){
      m[i] = a[i, j] / a[j, j]
      a[i,] = a[i,] - (m[i] * a[j,])
      b[i] = b[i] - (m[i]*b[j])
      print(a)
    }
  }
  return(sistema_triangular_superior(a, b, n))
}

print(eliminacao_de_gauss(a, b, n)) # -3 5 0


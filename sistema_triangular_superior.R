

sistema_triangular_superior = function(a, b, n){
  x = numeric()
  x[n] = b[n] / a[n, n]
  for(k in (n-1):1){
    S = 0
    for(j in (k+1):n)
      S = S + a[k, j] * x[j]
    x[k] = (b[k] - S)/a[k, k]
  }
  return(x)
}

if(FALSE){
  n = 3
  b = c(5, 9, 10)
  a = matrix(0, nrow = n, ncol = n)
  a[3, 3] = 5
  a[2, 2] = -1
  a[2, 3] = 4
  a[1, 1] = 2
  a[1, 2] = 1
  a[1, 3] = 2
  
  sistema_triangular_superior(a, b, n) # 1 -1 2
  
}
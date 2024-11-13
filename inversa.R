eliminacao_de_gauss_inversa <- function(a, n) {
  I = diag(n)
  
  # eliminação de gauss normal
  for (j in 1:(n - 1)) {
    for (i in (j + 1):n) {
      m = a[i, j] / a[j, j]
      a[i,] = a[i,] - m * a[j,]
      I[i,] = I[i,] - m * I[j,]
    }
  }
  
  #cat("\nA:\n")
  #print(a)
  #cat("\nIn:\n")
  #print(I)

  for (j in n:1) {
    I[j,] = I[j,] / a[j, j]
    a[j,] = a[j,] / a[j, j] # deixando a diagonal j igual a 1
    
    #cat(j)
    #cat("\nA:\n")
    #print(a)
    #cat("\nIn:\n")
    #print(I)
    
    for (i in (j-1):0) { # zerando a parte triangular superior
      #if (i < 1) break
      m = a[i, j]
      a[i,] = a[i,] - m * a[j,]
      I[i,] = I[i,] - m * I[j,]
    }
    
    #cat(j)
    #cat("\nA:\n")
    #print(a)
    #cat("\nIn:\n")
    #print(I)
  }
  
  return(I)
}

if(TRUE){
  n = 2
  A = matrix(nrow=n, ncol=n)
  A[1,] = c(2, 1)
  A[2,] = c(1, 3)
  cat('\014')
  Ain = eliminacao_de_gauss_inversa(A, n)
  print(Ain)
}

if(FALSE){
  n = 4
  A = matrix(nrow=n, ncol=n)
  A[1,] = c(   1, 0, 0, 0)
  A[2,] = c(-0.5, 1, 0, 0)
  A[3,] = c(  -1, 0, 1, 0)
  A[4,] = c(-0.5, 0, 0, 1)
  cat('\014')
  Ain = eliminacao_de_gauss_inversa(A, n)
  print(Ain)
}

# Questão 03

## a) ------------------------------
n = 4
A = matrix(nrow=n, ncol=n)
A[1,] = c( 1,  1,  0,  3)
A[2,] = c( 2,  1, -1,  1)
A[3,] = c( 3, -1, -1,  2)
A[4,] = c(-1,  2,  3, -1)
b = c(0, 0, 0, 0) # placeholder

LUb = fatoracao_lu(A, b, n)
L = LUb[[1]]
U = LUb[[2]]
cat("3. a)")
cat("L:", L)
cat("U:", U)
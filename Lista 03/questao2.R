# Questão 02

## a) ------------------------------
n = 3
A = matrix(nrow=n, ncol=n)
A[1,] = c(4, -1, 1)
A[2,] = c(2,  5, 2)
A[3,] = c(1,  2, 4)
b = c(8, 3, 11)

x = eliminacao_de_gauss(A, b, n)
cat("1. a)", x)

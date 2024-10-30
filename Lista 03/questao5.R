# Lista 3

## Questão 5

### Letra a

A = matrix(0, nrow=3, ncol=3)
A[1,] = c(2,  5, 2)
A[2,] = c(4, -1, 1)
A[3,] = c(1,  2, 4)

b = c(3, 8, 11)

# Trocando linhas 1 e 2
P1 = matrix(0, nrow=3, ncol=3)
P1[1,] = c(0, 1, 0)
P1[2,] = c(1, 0, 0)
P1[3,] = c(0, 0, 1)
A0 = P1 %*% A

# Criando matriz AUX para manter os valores fantasma
AUX = A0
#AUX = diag(3)

# Matriz M0 dos coeficientes
M0 = diag(3)
M0[2, 1] = - (A0[2, 1] / A0[1, 1])
M0[3, 1] = - (A0[3, 1] / A0[1, 1])

# ---

# Aplicando os coeficientes a A e a AUX
A1 = M0 %*% A0
AUX[2, 1] = (A0[2, 1] / A0[1, 1])
AUX[3, 1] = (A0[3, 1] / A0[1, 1])

# Não trocando linhas
#P2 = matrix(0, nrow=3, ncol=3)
#P2[1,] = c(1, 0, 0)
#P2[2,] = c(0, 0, 1)
#P2[3,] = c(0, 1, 0)
#A1 = P2 %*% A1
#AUX = P2 %*% AUX

# Parece que temos que trocar das matrizes M que ficam para trás também
#aux = M0[2, 1]
#M0[2, 1] = M0[3, 1]
#M0[3, 1] = aux

# Matriz M1 dos coeficientes
M1 = diag(3)
M1[3, 2] = - (A1[3, 2] / A1[2, 2])

# Aplicando os coeficientes a A e a AUX
A2 = M1 %*% A1
AUX[3, 2] =  (A1[3, 2] / A1[2, 2])

# Criando L
L = diag(3)
L[2, 1] = AUX[2, 1]
L[3, 1] = AUX[3, 1]
L[3, 2] = AUX[3, 2]

# Criando U
U = diag(3)
U[1, 1] = A2[1, 1]
U[1, 2] = A2[1, 2]
U[1, 3] = A2[1, 3]
U[2, 2] = A2[2, 2]
U[2, 3] = A2[2, 3]
U[3, 3] = A2[3, 3]

# Criando P, que resume as trocas de linhas
P = P1

# Aplicando as trocas de linhas a b
Pb = P %*% b

# L^(-1) = M1 * M0
M1 %*% M0
solve(L)

# Calculando y
y = solve(L) %*% P %*% b

sistema_triangular_superior(U, y, 3)


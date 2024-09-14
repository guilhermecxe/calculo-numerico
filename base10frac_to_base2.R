# Decimal fracionário para binário fracionário

N <- 0.1
a <- N
bits_precision <- 5
binary <- numeric()
i <- 1

while(a != 0 & i <= bits_precision){
  binary[i] <- (a * 2) %/% 1
  frac <- (a * 2) %% 1
  a <- frac
  i <- i + 1
}

cat('\014')
cat(N, "em binário é 0.", binary)

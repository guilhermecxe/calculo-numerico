# Decimal fracionário para binário fracionário

c10frac_to_2frac = function(n){
  a <- n
  bits_precision <- 15
  binary <- numeric()
  i <- 1
  
  while(a != 0 & i <= bits_precision){
    binary[i] <- (a * 2) %/% 1
    frac <- (a * 2) %% 1
    a <- frac
    i <- i + 1
    cat('binário:', binary[i-1], '\n')
    cat('fracionário:', frac, '\n')
  }
  
  #cat('\014')
  #cat(N, "em binário é 0.", binary)
  return (binary)
}
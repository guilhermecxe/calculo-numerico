# Binário fracionário para decimal fracionário

binary <- c(0, 1, 0, 1, 1) # apenas parte fracionária
decimal <- 0

for(i in 1:length(binary)){
  decimal <- decimal + binary[i]*2^(-i)
}

cat('\014')
cat("0,", binary, "em decimal é", decimal)
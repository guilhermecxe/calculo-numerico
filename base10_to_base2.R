n <- 78
b <- numeric()
i <- 1

# calculando os bits
while (n != 0) {
    b[i] <- n %% 2
    n <- n %/% 2
    i <- i + 1
}

# invertendo os bits
size <- length(b)
half_size <- size / 2
aux <- numeric()
for (i in 0:half_size){
    cat(i)
    aux <- b[i+1]
    b[i+1] <- b[size-i]
    b[size-i] <- aux
}

cat("78 in binary is", b)

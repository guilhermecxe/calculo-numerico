binary_vector_to_decimal <- function(binary_vector){
  decimal <- 0
  position <- length(binary_vector)-1
  
  for(bit in binary_vector){
    decimal <- decimal + (bit * 2^position)
    position <- position - 1
  }
  return(decimal)
}

binary_string_to_decimal <- function(binary_string){
  decimal <- 0
  position <- nchar(binary_string)-1
  chars_vector = strsplit(binary_string, "")[[1]]
  
  for(bit in chars_vector){
    decimal <- decimal + (as.integer(bit) * 2^position)
    position <- position - 1
  }
  return(decimal)
}

binary_vector_to_decimal_method_2 <- function(binary_vector){
  last_value = binary_vector[1]
  
  for(bit in binary_vector[-1]){
    new_value = bit + 2*last_value
    last_value = new_value
  }
  return(new_value)
}

binary_vector <- c(1, 0, 1, 0, 1, 1, 1, 0, 1)
binary_string <- "101011101"

cat('\014')
decimal_from_vector = binary_vector_to_decimal(binary_vector)
decimal_from_string = binary_string_to_decimal(binary_string)
decimal_from_vector_m2 = binary_vector_to_decimal_method_2(binary_vector)


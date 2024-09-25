f = function(x){
  #return (x^2 + x - 6)
  return (10*exp((-10)*x) - 30)
}

secante = function(f, x0, x1, prec){
  # Método da Secante aplicado o critério de parada
  # onde o valor de f(x) encontrado é menor que
  # epsilon
  while(abs(f(x1)) > prec){
    next_x = (x0*f(x1) - x1*f(x0)) / (f(x1) - f(x0))
    x0 = x1
    x1 = next_x
  }
  return(x1)
}

secante2 = function(f, x0, x1, prec){
  # Método da Secante aplicado o critério de parada
  # onde o espaço entre o x atual e o x anterior
  # é menor que epsilon
  while(abs(x1 - x0) > prec){
    next_x = (x0*f(x1) - x1*f(x0)) / (f(x1) - f(x0))
    cat("x =", next_x, " f(x) =", f(next_x), "\n")
    x0 = x1
    x1 = next_x
  }
  return(x1)
}
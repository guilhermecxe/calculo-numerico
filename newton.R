f = function(x){
  #return (x^2 + x - 6)
  #return (x^4 - (5/4)*x^3 - (5/8)*x^2 + (5/4)*x - 3/8)
  #return (5*(x^3) - 8*x - 0.5)
  return (10*exp((-10)*x) - 30)
}

f_ = function(x){
  # Derivada de f
  #return (2*x + 1)
  #return (4*(x^3) - (15/4)*(x^2) - (5/4)*x + (5/4))
  #return (15*(x^2) - 8)
  return (-100*exp((-10*x)))
}

newton1 = function(f, f_, x, prec){
  # Método de Newton aplicado o critério de parada
  # onde o espaço entre o x atual e o x anterior
  # é menor que epsilon
  previous_x = numeric()
  while(TRUE){
    previous_x = x
    x = x - (f(x)/f_(x))
    if(abs(previous_x - x) < prec)
      break
  }
  return (x)
}

newton2 = function(f, f_, x, prec){
  # Método de Newton aplicado o critério de parada
  # onde o valor de f(x) encontrado é menor que
  # epsilon
  previous_x = numeric()
  while(abs(f(x)) > prec){
    previous_x = x
    x = x - (f(x)/f_(x))
    cat("x =", x, " f(x) =", f(x), "\n")
  }
  return (x)
}

f = function(x){
  return (x^2 + x - 6)
}

f_ = function(x){
  return (2*x + 1)
}

g = function(x){
  return(4*cos(x) - exp(1)^x)
}

g_ = function(x){
  return (-4*sin(x) - exp(1)^x)
}

newton1 = function(f, f_, x, prec){
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
  previous_x = numeric()
  while(abs(f(x)) > prec){
    previous_x = x
    x = x - (f(x)/f_(x))
  }
  return (x)
}
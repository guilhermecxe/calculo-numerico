
f = function(x){
  # Função de exemplo
  # return (x*log10(x) - 1)
  # return (5*(x^3) - 8*x - 0.5)
  # return (x^4 - (5/4)*x^3 - (5/8)*x^2 + (5/4)*x - 3/8)
  return (-(x^4) - 6*(x^3) + 10*(x^2) - 6*x + 9)
}

iteractions = function(a, b, erro){
  # Retorna a quantidade de iterações necessárias no
  # método da bissecção até que o intervalo [ax, bx]
  # seja menor que o erro.
  return (ceiling((log(b - a) - log(erro)) / log(2)))
}

bisseccao = function(f, a, b, erro){
  # Aplica o método da bissecção para retornar a raíz
  # da função f com precisão na grandeza do erro.
  # Condições do while: ((b-a) > erro) ou (abs(f(half)) > erro)
  half = a + ((b-a) / 2)
  while(abs(f(half)) > erro){
    half = a + ((b-a) / 2)
    if((f(a)*f(half)) < 0)
      b = half
    else if(f(half) == 0)
      break
    else
      a = half
  }
  return (half)
}

display_sign_rule = function(f, a, b, by){
  # exibe o sinal de f(x) tal que x vai de "a" até "b"
  # de "by" em "by"
  for(i in seq(from=a, to=b, by=by))
    cat("f(", i, "): ", f(i)/abs(f(i)), "\n", sep="")
}

# display_sign_rule(f, -5, 5, 0.1)
# its = iteractions(2, 3, 10^-6)
# root = bisseccao(f, -8, -6, 10^-2)


expr <- expression(sin(x) + cos(x))
deriv_expr <- D(expr, "x")
print(deriv_expr)

expr <- expression(x^2 + 3*x + 5)
deriv_expr <- D(expr, "x")
print(deriv_expr)
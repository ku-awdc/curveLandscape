
epanechnikov <- \(x) 3/sqrt(5) * (1 - (x**2)/5)

curve(epanechnikov(x), from = -sqrt(5), to = sqrt(5))
# curve(epanechnikov(x -2), from = -sqrt(5) - 2, to = sqrt(5) + 2)
epanechnikov <- \(x,c) 3/sqrt(5) * (1 - (x**2)/c)


epanechnikov <- \(x, c) 3/(4*c) * (1-x**2 / c**2)


curve(epanechnikov(x, 10), from = -20, to = 20)
abline(h=0)
integrate(
  \(x) epanechnikov(x, 10), lower = -10, upper = 10
)


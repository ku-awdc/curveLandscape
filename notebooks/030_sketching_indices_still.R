
tri_num <- function(n) n*(n+1) / 2

k <- 12

ii <- (sqrt(8*k + 1) - 1) %/% 2
ii

jj <- k - ii * (ii + 1) / 2
jj

c(i = ii, j = jj)


k - tri_num(ii)

#' The task is to take a linear, column-wise index `k`
#' and turn it into `(i,j)` for a strictly lower-triangular matrix.
#'
#' [`dist`] function doesn't contain the diagonal..
#' regardless as to what you set the call to and its `diag`-parameter.
#'
#'

get_ij <- function(n) {
  k <- seq_len(n * (n - 1) / 2) - 1

  # APPROACH:
  # SOURCE: https://stackoverflow.com/a/27088560/63696
  i <- n - 2 - floor(sqrt(-8 * k + 4 * n * (n - 1) - 7) / 2.0 - 0.5)
  # j <- k + i + 1 - n * (n - 1) / 2 + (n - i) * ((n - i) - 1) / 2
  j <- k + i * (i + 3) / 2 - n * i + 1

  cbind(i = i, j = j)
  
  # APPROACH: doesn't work
  # kk <- n * (n - 1) / 2 - k
  # p <- floor(
  #   (sqrt(1 + 8 * kk) - 1) / 2
  # )
  # cbind(
  #   i = k - n * (n - 1) / 2 + p * (p + 1) / 2,
  #   j = n - 1 - p
  # )
}

get_ij(1)
get_ij(2)
get_ij(3)
get_ij(4)
get_ij(5)

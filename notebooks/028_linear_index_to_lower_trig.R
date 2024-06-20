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


# Lower and upper triangle in one go

lower_upper_no_diag <- function(n) {
  k <- seq_len(n * (n - 1) / 2) - 1

  # (i,j) for upper
  i <- (sqrt(1 + 8 * k) - 1) %/% 2
  j <- k - i * (i + 1) / 2 + 1

  # (r,c) for lower triangle
  r <- (sqrt(1 + 8 * k) + 1) %/% 2
  c <- k - r * (r - 1) / 2

  cbind(i = i, j = j, r = r, c = c)
}

lower_upper_no_diag(1)
lower_upper_no_diag(2)
lower_upper_no_diag(3)
lower_upper_no_diag(4)
lower_upper_no_diag(5)

# Matrix::tril(k = 0L)


triangle_numbers <- \(n) n * (n + 1) / 2

triangle_numbers(0:10)

#' Returns the indices of lower-triangle with diagonal elements
get_k <- function(n, k) {
  ri <- floor((sqrt(1 + 8 * k) - 1) / 2)
  ci <- k - (ri * (ri + 1) / 2)
  cbind(
    ri = ri,
    ci = ci
  )
}

# DOESN'T WORK
# get_rc <- function(n, i, j) {
#   # triangle_numbers(n-1) - triangle_numbers(n-i-1) + j
#   triangle_numbers(n - 1) - triangle_numbers(n - j - 1) + i - j - 1
# }

mXY <- pracma::meshgrid(seq_len(5)-1, seq_len(5)-1)
mID <- mXY$X # initialise
dimnames(mID) <- dim(mID) |>
  lapply(\(x) seq_len(x) - 1)
mID[] <- str_c(seq_len(prod(dim(mID))), ":", "(", as.numeric(t(t(mXY$Y))), ",", as.numeric(t(t(mXY$X))), ")")
mID

get_k(5, seq_len(triangle_numbers(5 - 1)) - 1)

#' Returns the indices of lower-triangle with diagonal elements
get_k_no_diag <- function(n, k) {
  ri <- floor((sqrt(1 + 8 * k) + 1) / 2)
  ci <- k - (ri * (ri + 1) / 2)
  cbind(
    ri = ri,
    ci = ci
  )
}

get_k(5, seq_len(triangle_numbers(5 - 1)) - 1)

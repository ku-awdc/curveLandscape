n_dim <- 4
stopifnot(n_dim > 0)
triangle_numbers <- 1:n_dim
triangle_numbers <- triangle_numbers * (triangle_numbers + 1) / 2
triangle_numbers
triangle_numbers[4]


tibble(
  k = seq_len((n_dim * (n_dim + 1)) %/% 2)
) |> mutate(
  kk = n_dim*(n_dim+1)/2 - k,
  p = (sqrt(8 * kk + 1) - 1) %/% 2,
  # pp = (sqrt(8 * k + 1) - 1) / 2,
  # p = floor(pp),
  i = NA_integer_,
  j = n_dim - p
)

matrix(
  NA,
  nrow = n_dim, ncol = n_dim, 
  byrow = TRUE
)

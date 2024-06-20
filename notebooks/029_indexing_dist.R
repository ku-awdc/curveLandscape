
devtools::load_all()


library(sfdep)

# data(package = "sfdep")
guerry <- sfdep::guerry
sdistguerry <- guerry |> st_distance()

distguerry <- dist(guerry |> st_centroid() |> st_coordinates())

distguerry |> unclass() |> length()

get_total_number_of_elements(85)

# get_row_col(
#   0,
#   n = 85
# )
# get_row_col(
#   1,
#   n = 85
# )
# get_row_col(
#   4,
#   n = 85
# )

# for (k in 0:get_total_number_of_elements(85)
# ) {
#   print(paste0("k = ", k))
#   print(
#     get_row_col(
#       k, n = 85
#     )
#   )
# }


# n <- 5
# mXY <- pracma::meshgrid(seq_len(n)-1, seq_len(n)-1)
# mID <- mXY$X # initialise
# dimnames(mID) <- dim(mID) |>
#   lapply(\(x) seq_len(x) - 1)
# mID[] <- str_c(seq_len(prod(dim(mID))), ":", "(", as.numeric(t(t(mXY$Y))), ",", as.numeric(t(t(mXY$X))), ")")
# mID
# Matrix::tril(mXY$X, k = -1, sparse=TRUE)
# Matrix::band()

get_row_col(0, n=3)
get_row_col(1, n=3)
get_row_col(2, n=3)
get_row_col(4, n=3)

get_row_col(10, n=3)


hand <- function(k,n) {
  # jj = 2 * n - 1 + sqrt((2*n - 1)**2 - 8*k)
  # jj = floor(jj / 2)
  # jj <- (n-1) + sqrt((n-1)**2 - 2 * k) 
  ii <- sqrt(8*k + 1) - 1
  ii <- floor(ii / 2)
  jj <- k - ii * (ii + 1) / 2
  c(ii,jj)
}
hand(0, 1) # expect nothing
hand(1, 1) # NAN 
hand(2, 1) # NAN 
hand(3, 1) # NAN 

hand(0, n = 2) # expect 1 0 
hand(1, n = 2)
hand(2, n = 2) # NAN

hand(0, n = 3)
hand(1, n = 3)
hand(2, n = 3)
# hand(3, n = 3)
# hand(4, n = 3) # NAN

hand(0, n = 4)
hand(1, n = 4)
hand(2, n = 4)
hand(3, n = 4)
hand(4, n = 4)
hand(5, n = 4)
# hand(6, n = 4)
# hand(7, n = 4) # NAN

# # diagonal included
# index_to_i_j_colwise_diag <- function(k, n) {
#   kp <- n * (n + 1) / 2 - k
#   p  <- floor((sqrt(1 + 8 * kp) - 1) / 2)
#   i  <- n - (kp - p * (p + 1) / 2)
#   j  <- n - p
#   c(i, j)
# }

# # diagonal excluded
# #' SOURCE: <https://atrebas.github.io/post/2021-01-17-index_to_lower_triangular_subscripts/>
# index_to_i_j_colwise_nodiag <- function(k, n) {
#   kp <- n * (n - 1) / 2 - k
#   p  <- floor((sqrt(1 + 8 * kp) - 1) / 2)
#   i  <- n - (kp - p * (p + 1) / 2)
#   j  <- n - 1 - p
#   c(i, j)
# }
# index_to_i_j_colwise_nodiag(0, n=1) # expect: nothing
# # index_to_i_j_colwise_nodiag(1, n=1) # NaN
# index_to_i_j_colwise_nodiag(0, n=2) # expect: 1 0
# index_to_i_j_colwise_nodiag(1, n=2) # expect nothing
# # index_to_i_j_colwise_nodiag(2, n=2) # NaN
# index_to_i_j_colwise_nodiag(0, n=3) # expect: 1 0 
# index_to_i_j_colwise_nodiag(1, n=3) # expect: 2 0
# index_to_i_j_colwise_nodiag(2, n=3) # expect: 2 1 
# index_to_i_j_colwise_nodiag(3, n=3) # expect nothing
# # index_to_i_j_colwise_nodiag(4, n=3) # NaN



# index_to_i_j_colwise_diag <- function(k, n) {
#   kp <- n * (n + 1) / 2 - k
#   p  <- floor((sqrt(1 + 8 * kp) - 1) / 2)
#   i  <- n - (kp - p * (p + 1) / 2)
#   j  <- n - p
#   c(i, j)
# }

# index_to_i_j_colwise_diag(0, n=1) # expect: nothing
# # index_to_i_j_colwise_nodiag(1, n=1) # NaN
# index_to_i_j_colwise_diag(0, n=2) # expect: 1 0
# index_to_i_j_colwise_diag(1, n=2) # expect nothing
# # index_to_i_j_colwise_diag(2, n=2) # NaN
# index_to_i_j_colwise_diag(0, n=3) # expect: 1 0 
# index_to_i_j_colwise_diag(1, n=3) # expect: 2 0
# index_to_i_j_colwise_diag(2, n=3) # expect: 2 1 
# index_to_i_j_colwise_diag(3, n=3) # expect nothing
# index_to_i_j_colwise_diag(4, n=3) # NaN
# index_to_i_j_colwise_diag(5, n=3) # NaN
# index_to_i_j_colwise_diag(6, n=3) # NaN
# # index_to_i_j_colwise_diag(7, n=3) # NaN

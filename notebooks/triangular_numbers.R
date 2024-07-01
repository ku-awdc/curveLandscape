# Triangular matrix indices
index4 <- function(k, n) {
  # because we want to start with index 0 and algorithm starts with index 1
  k = k+1

  # replace n by n-1 in index3 and i by i + 1
  kk = n*(n-1)/2 - k
  p = floor((sqrt(1+ 8 * kk) - 1) / 2)

  i = k - (n - 1) * (n - 2) / 2 + p * (p + 1) / 2 + 1
  j = n - 1 - p

  # because we want to start with index 0 and algorithm starts with index 1
  return(cbind(i = i-1, j = j-1))
}

index4_clean <- function(k, n) {
  # because we want to start with index 0 and algorithm starts with index 1
  k = k+1

  # replace n by n-1 in index3 and i by i + 1
  kk = n*(n-1)/2 - k
  p = floor((sqrt(1+ 8 * kk) - 1) / 2)

  # because we want to start with index 0 and algorithm starts with index 1
  # we need to subtract 1 from i and from j
  i = k - (n - 1) * (n - 2) / 2 + p * (p + 1) / 2
  j = n - 2 - p

  return(cbind(i = i, j = j))
}

index4(0, 4)
index4(1, 4)
index4(2, 4)
index4(3, 4)
index4(4, 4)
index4(5, 4)
# index4(6, 4)
index4_clean(0, 4)
index4_clean(1, 4)
index4_clean(2, 4)
index4_clean(3, 4)
index4_clean(4, 4)
index4_clean(5, 4)

index3 <- function(k, n) {
  kk = n*(n+1)/2 - k
  p = floor((sqrt(1 + 8 * kk) - 1) / 2)

  i = k - n * (n - 1) / 2 + p * (p + 1) / 2
  j = n - p

  return(cbind(i = i, j = j))
}

# index3(0, 4)
index3(1, 4)
index3(2, 4)
index3(3, 4)
index3(4, 4)
index3(5, 4)
index3(6, 4)
index3(7, 4)
index3(8, 4)
index3(9, 4)
index3(10, 4)

index_to_i_j_colwise_nodiag <- function(k, n) {
  kp <- n * (n - 1) / 2 - k
  p  <- floor((sqrt(1 + 8 * kp) - 1) / 2)
  i  <- n - (kp - p * (p + 1) / 2)
  j  <- n - 1 - p
  c(i, j)
}
index_to_i_j_colwise_nodiag(1, 4)
index_to_i_j_colwise_nodiag(2, 4)
index_to_i_j_colwise_nodiag(3, 4)
index_to_i_j_colwise_nodiag(4, 4)
index_to_i_j_colwise_nodiag(5, 4)
index_to_i_j_colwise_nodiag(6, 4)



devtools::load_all()


library(sfdep)

# data(package = "sfdep")
guerry <- sfdep::guerry
sdistguerry <- guerry |> st_distance()

distguerry <- dist(guerry |> st_centroid() |> st_coordinates())

distguerry |> unclass() |> length()

get_total_number_of_elements(85)

get_row_col(
  0,
  n = 85
)
get_row_col(
  1,
  n = 85
)
get_row_col(
  4,
  n = 85
)

for (k in 0:get_total_number_of_elements(85)
) {
  print(paste0("k = ", k))
  print(
    get_row_col(
      k, n = 85
    )
  )
}


n <- 5
mXY <- pracma::meshgrid(seq_len(n)-1, seq_len(n)-1)
mID <- mXY$X # initialise
dimnames(mID) <- dim(mID) |>
  lapply(\(x) seq_len(x) - 1)
mID[] <- str_c(seq_len(prod(dim(mID))), ":", "(", as.numeric(t(t(mXY$Y))), ",", as.numeric(t(t(mXY$X))), ")")
mID
Matrix::tril(mXY$X, k = -1, sparse=TRUE)
Matrix::band()
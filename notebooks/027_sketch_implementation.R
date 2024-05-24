devtools::load_all()

total_n <- 10
n0 <- 5
t_max <- 25

sim_bdm(
  n0 = rep.int(n0, times = total_n) |> 
    as.integer(),
  rep.int(4., times = total_n),
  rep.int(1., times = total_n),
  t_max = 25
)


# scenario 1
n <- c(50, 0, 0, 0)
cc <- c(10, 20, 5, 15)

Matrix::Matrix(
  n, nrow = 2, ncol = 2, byrow = FALSE
)
Matrix::Matrix(
  cc, nrow = 2, ncol = 2, byrow = FALSE
)


sim_bdm(
  n0 = rep.int(n0, times = total_n) |> 
    as.integer(),
  rep.int(4., times = total_n),
  rep.int(1., times = total_n),
  t_max = 25
)
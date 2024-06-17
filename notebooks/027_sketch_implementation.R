devtools::load_all()

library(magrittr)

total_n <- 10
n0 <- 5
t_max <- 25

sim_bdm_result <- sim_bdm(
  n0 = as.integer(n0),
  birth_baseline = 4.,
  death_baseline = 1.,
  carrying_capacity = 7L,
  t_max = t_max
)

sim_bdm_result |> 
  as_tibble() |>
  ggplot() +
  aes(time, state) + 
  geom_step(show.legend = FALSE) +
  labs(
    y = "number of boars",
  ) +
  theme_bw()

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
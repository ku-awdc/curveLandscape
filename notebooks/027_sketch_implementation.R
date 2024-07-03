devtools::load_all()

library(magrittr)

total_n <- 10
n0 <- 5
t_max <- 25
cc <- 7

sim_bdm_result <- sim_bdm(
  n0 = as.integer(n0),
  birth_baseline = 4.,
  death_baseline = 1.,
  carrying_capacity = cc,
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
  geom_hline(
    aes(yintercept = cc, color = "cc"),
    linetype = "dashed"
  ) +
  labs(color = NULL) +
  theme_bw() +
  theme(legend.position = "bottom") +
  NULL

# migration only simulation

n0 <- c(10L, 2L) * 10L
migration_baseline <- c(0.1, 0.00001)
carrying_capacity <- c(9L, 1L) * 10L
k_dij <- c(1)
t_max <- 1000000.

devtools::load_all()
sim_migration_only(
  n0, migration_baseline, carrying_capacity, k_dij, t_max
) -> sim_mig_only

sim_mig_only |> 
  as_tibble() -> sim_mig_only;
sim_mig_only |> 
  pivot_wider(names_from = id_state, values_from = state) |> 
  mutate(total = `0` + `1`) |> 
  print(n=Inf)

sim_mig_only |> 
  ggplot() +
  aes(time) +
  geom_step(aes(y=state, color = id_state)) +
  geom_hline(aes(yintercept = carrying_capacity),
data = \(x) mutate(x, carrying_capacity = carrying_capacity[id_state + 1])) +
  scale_color_steps() +
  theme_bw(base_size = 15) +
  theme(legend.position = "bottom") +
  NULL

# # scenario 1
# n <- c(50, 0, 0, 0)
# cc <- c(10, 20, 5, 15)

# Matrix::Matrix(
#   n,
#   nrow = 2, ncol = 2, byrow = FALSE
# )
# Matrix::Matrix(
#   cc,
#   nrow = 2, ncol = 2, byrow = FALSE
# )


# sim_bdm(
#   n0 = rep.int(n0, times = total_n) |>
#     as.integer(),
#   rep.int(4., times = total_n),
#   rep.int(1., times = total_n),
#   t_max = 25
# )

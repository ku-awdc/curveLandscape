
create_ode_migration_only(
  initial_population = c(2, 0),
  times = seq.default(0, 100, length.out = 1000),
  birth_rate = NA,
  death_rate = NA,
  carrying_capacity <- c(2, 3),
  migration_baseline = c(0.1, 0.1),
  kernel_distance_flat = c(1)
) -> ode_mig_output

create_ode_migration_only(
  initial_population = c(5, 0),
  times = seq.default(0, 150, length.out = 1000),
  birth_rate = NA,
  death_rate = NA,
  carrying_capacity <- c(2, 3),
  migration_baseline = c(0.1, 0.1),
  kernel_distance_flat = c(1)
) -> ode_mig_output


create_ode_migration_only(
  initial_population = c(5, 5),
  times = seq.default(0, 150, length.out = 1000),
  birth_rate = NA,
  death_rate = NA,
  carrying_capacity <- c(2, 3),
  migration_baseline = c(0.1, 0.1),
  kernel_distance_flat = c(1)
) -> ode_mig_output

#' What if carrying capacity was lower than the current number of animals

create_ode_migration_only(
  initial_population = c(5, 5),
  times = seq.default(0, 150, length.out = 1000),
  birth_rate = NA,
  death_rate = NA,
  carrying_capacity <- c(7, 9),
  migration_baseline = c(0.1, 0.1),
  kernel_distance_flat = c(1)
) -> ode_mig_output


create_ode_migration_only(
  initial_population = c(5, 15),
  times = seq.default(0, 400, length.out = 1000),
  birth_rate = NA,
  death_rate = NA,
  carrying_capacity <- c(3, 9),
  migration_baseline = c(0.1 / 2.9, 0.1),
  kernel_distance_flat = c(1)
) -> ode_mig_output


{
ode_mig_output %>%
  as_tibble() %>%
  pivot_longer(-time) %>%
  mutate(carrying_capacity = carrying_capacity[as.integer(name)]) %>%
  identity() %>%
  # filter(time < 2) %>% 
  {
    ggplot(.) +
      aes(time) +
      geom_step(aes(y = value, color = name)) +
      geom_hline(
        aes(yintercept = carrying_capacity, color = name),
        linewidth = 1.1,
        linetype = "dashed"
      ) +
      theme_bw() +
      theme(legend.position = "bottom") +
      NULL
  }
}

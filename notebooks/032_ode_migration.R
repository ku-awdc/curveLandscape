#' When developing this, in a terminal run `just watch`.
devtools::load_all()
library(deSolve)

#'
#'
create_ode_model <- function(
    initial_population, times,
    birth_rate = NULL,
    death_rate = NULL,
    carrying_capacity,
    migration_baseline,
    kernel_distance_flat) {
  # TODO: construct these

  #
  # - [x] update_migration_only(population, migration_baseline, carrying_capacity,
  #   k_dij)
  # - [ ] update_birth_death_and_migration(
  #   population, birth_rate, death_Rate, migration_baseline, carrying_capacity,
  #   k_dij
  # )
  # - ?



  ode(
    y = initial_population,
    times = times,
    parms = list(
      # birth_rate = birth_rate, death_rate = death_rate,
      carrying_capacity = carrying_capacity,
      migration_baseline = migration_baseline,
      k_dij = kernel_distance_flat
    ),
    func = function(t, y, parms, ...) {
      with(parms, {
        update_migration_only(
          population_total = y, migration_baseline, carrying_capacity,
          k_dij
        )

        list(
          c(dN = y)
        )
      })
    }
  )
}
#'
#'
devtools::load_all()
create_ode_model(
  initial_population = c(0, 50),
  times = seq.default(0, 300, length.out = 1000),
  birth_rate = NA,
  death_rate = NA,
  # carrying capacity contains differences in cell areas
  # and availability of resources
  carrying_capacity <- c(20,  20),
  # use migration-baseline for duration of movement season
  # i.e. difference between male vs. female dispersal
  # migration_baseline = c(1, 1) / (6 * 7) * 10,
  migration_baseline = c(1, 1) * 1e7,
  kernel_distance_flat = c(1, 1)
) -> ode_mig_output

ode_mig_output %>%
  as_tibble() %>%
  pivot_longer(-time) %>%
  mutate(carrying_capacity = carrying_capacity[as.integer(name)]) %>%
  identity() %>%
  {
    ggplot(.) +
      aes(time) +
      geom_step(aes(y = value, color = name)) +
      geom_hline(
        aes(yintercept = carrying_capacity, color = name),
        linewidth = 0.9,
        linetype = "dashed"
      ) +
      theme_bw() +
      theme(legend.position = "bottom") +
      NULL
  }

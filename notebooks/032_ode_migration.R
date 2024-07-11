#' When developing this, in a terminal run `just watch`.
devtools::load_all()
library(deSolve)

#'
#' @description
#' Returns a fit of an ODE model with density-dependent migration.
#' 
#' dot(N)_i = sum_(j!=i) m_(ij) N_j - sum_(j!=i) m_(ji) N_i
#' 
#' where m_(ij) = m_0 • k_dij_(ij) / (CC_i) 
#' 
#' @param kernel_distance_flat Numeric vector, one dimensional, having the same format as [`stat::dist`].
#' We assume the distance metric is symmetric.
#' 
create_ode_migration_only <- function(
    initial_population, times,
    birth_rate = NULL,
    death_rate = NULL,
    carrying_capacity,
    migration_baseline,
    kernel_distance_flat) {
  
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
create_ode_migration_only(
  initial_population = c(100, 2),
  times = seq.default(0, 500, length.out = 1000),
  birth_rate = NA,
  death_rate = NA,
  # carrying capacity contains differences in cell areas
  # and availability of resources
  carrying_capacity <- c(20, 20),
  # use migration-baseline for duration of movement season
  # i.e. difference between male vs. female dispersal
  # migration_baseline = c(1, 1) / (6 * 7) * 10,
  migration_baseline = c(1, 1) / 10,
  kernel_distance_flat = c(1)
) -> ode_mig_output

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
        linewidth = 0.9,
        linetype = "dashed"
      ) +
      theme_bw() +
      theme(legend.position = "bottom") +
      NULL
  }


#' Birth + Death + Migration
#'
#'
#' #' When developing this, in a terminal run `just watch`.
devtools::load_all()
library(deSolve)

#'
#' 

#' @description
#' Returns a fit of an ODE model with density-dependent birth, death and migration.
#' 
#' dot(N)_i = (birth-death) • N_i (1 - N_i / CC_i) + sum_(j!=i) m_(ij) N_j - sum_(j!=i) m_(ji) N_i
#' 
#' where m_(ij) = m_0 • k_dij_(ij) / (CC_i)
#' 
#' @param kernel_distance_flat Numeric vector, one dimensional, having the same format as [`stat::dist`].
#' We assume the distance metric is symmetric.
#' 
create_ode_model_bdm <- function(
    initial_population, times,
    birth_rate = NULL,
    death_rate = NULL,
    carrying_capacity,
    migration_baseline,
    kernel_distance_flat) {
  ode(
    y = initial_population,
    times = times,
    parms = list(
      birth_rate = birth_rate,
      death_rate = death_rate,
      carrying_capacity = carrying_capacity,
      migration_baseline = migration_baseline,
      k_dij = kernel_distance_flat
    ),
    func = function(t, y, parms, ...) {
      with(parms, {
        update_birth_death_and_migration(
          population_total = y,
          birth = birth_rate,
          death = death_rate,
          migration_baseline, carrying_capacity,
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
create_ode_model_bdm(
  initial_population = c(1, 5),
  times = seq.default(0, 10 * 365, length.out = 1000),
  birth_rate = 4 / 365,
  death_rate = 1 / 365,
  # carrying capacity contains differences in cell areas
  # and availability of resources
  carrying_capacity <- c(20, 20),
  # use migration-baseline for duration of movement season
  # i.e. difference between male vs. female dispersal
  # migration_baseline = c(1, 1) / (6 * 7) * 10,
  # migration_baseline = c(1, 1) / (6*7),
  migration_baseline = c(0, 0),
  kernel_distance_flat = c(1)
) -> ode_bdm_output

ode_bdm_output %>%
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

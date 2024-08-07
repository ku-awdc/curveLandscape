#'
#'
#' @param migration_baseline Double, normalised internally by (n-1)
ode_source_only <- function(growth_rate, carrying_capacity, n0, migration_baseline, delta_t, t_max = 25) {
  n_len <- length(n0)
  migration_baseline <- if (n_len == 1) {
    0
  } else {
    migration_baseline / (n_len - 1)
  }
  # ensure that even if n = 1, that it is called N1.
  N0 <- if (n_len == 1) {
    c(N1 = n0)
  } else {
    c(N = n0)
  }

  deSolve::ode(
    y = N0, times = seq.default(0, t_max, by = delta_t),
    parms = list(r = growth_rate, cc = carrying_capacity, m0 = migration_baseline),
    func = function(times, y, parameters) {
      with(parameters, {
        dN <- r * y * (1 - y / cc)

        # APPROACH: WEDGE
        mj <- (
          m0 * pmax(y - (cc - 1), 0)
        ) / cc
        dN <- dN + (sum(mj * y) - mj * y) - mj * y * (n_len - 1)

        # APPROACH: SMOOTH OR LogSumExp
        # mj <- (m0 * log1p(exp(y - cc))) / (log(2) * cc)
        # dN <- dN + (sum(mj * y) - mj * y) - mj * y * (n_len - 1)

        list(dN, dM = (sum(mj * y) - mj * y) - mj * y * (n_len - 1))
      })
    }
  ) %>%
    unclass() %>%
    as_tibble() %>%
    pivot_longer(
      matches("(N|d\\w+)"),
      names_pattern = "(\\w+)\\.?N?(\\d+)",
      names_to = c("metric", "id_patch"),
    ) %>%
    pivot_wider(names_from = "metric", values_from = "value") %>%
    identity()
}


#' let us do with two patches...

ode_source_only(
  # n0 = as.integer(c(10, 0)),
  # birth_baseline = c(4, 4),
  # death_baseline = c(1, 1),
  # carrying_capacity = as.integer(c(2, 8)),
  # migration_baseline = 1/(8/12),
  # t_max = 5
  growth_rate = 4 - 1,
  carrying_capacity = c(2, 8),
  n0 = c(10, 0),
  migration_baseline = 1 / (8 / 12),
  delta_t = 1 / (12 * 4),
  t_max = 5
) -> deter_output


deter_output %>%
  identity() %>%
  {
    ggplot(.) +
      aes(time, N, group = id_patch) +
      geom_step(
        # alpha = 0.6,
        aes(color = factor(id_patch))
      ) +
      labs(color = NULL) +
      # scale_colour_manual(
      #   values = c(
      #     "ODE" = "black",
      #     "Stochastic" = "lightblue"
      #   )
      # ) +
      # expand_limits(y = c(0, 2, 8)) +
      theme_bw(base_size = 15) +
      theme(legend.position = "bottom") +
      NULL
  }

#' Let us test one patch...
#'
ode_source_only(
  growth_rate = 4 - 1,
  carrying_capacity = 4,
  # n0 = 0.1,
  n0 = 1,
  migration_baseline = 1 / (8 / 12),
  delta_t = 1 / (12 * 4),
  t_max = 5
) -> deter_output
# deter_output %>% print(n=Inf)

deter_output %>%
  as_tibble() %>%
  identity() %>%
  {
    ggplot(.) +
      aes(time, N, group = id_patch) +
      geom_step(
        # alpha = 0.6,
        aes(color = factor(id_patch))
      ) +
      labs(color = NULL) +
      # scale_colour_manual(
      #   values = c(
      #     "ODE" = "black",
      #     "Stochastic" = "lightblue"
      #   )
      # ) +
      expand_limits(y = 0) +
      theme_bw(base_size = 15) +
      theme(legend.position = "bottom") +
      NULL
  }

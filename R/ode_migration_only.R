#' Birth, death, and migration ODE model.
#' 
#' This migration rate is just density-dependent. It makes no distinction on the state of the source nor the destination.
#' 
#'
#'
#' @param migration_baseline Double, normalised internally by (n-1)
#' @inheritParams deSolve::ode
ode_migration_only <- function(growth_rate, carrying_capacity, n0, migration_offset = 0, migration_intercept = 0, migration_baseline, delta_t, t_max = 25, ...) {
  n_len <- length(n0)
  migration_baseline <- if (n_len == 1) {
    0
  } else {
    migration_baseline / (n_len - 1)
  }
  migration_intercept <- if (n_len == 1) {
    0
  } else {
    migration_intercept / (n_len - 1)
  }
  # ensure that even if n = 1, that it is called N1.
  N0 <- if (n_len == 1) {
    c(N1 = n0)
  } else {
    c(N = n0)
  }

  deSolve::ode(
    y = N0,
    times = seq.default(from = 0, to = t_max, by = delta_t),
    parms = list(r = growth_rate, 
      cc = carrying_capacity, 
      migration_offset = migration_offset,
      migration_baseline = migration_baseline, 
      migration_intercept = migration_intercept),
    func = function(times, y, parameters) {
      with(parameters, {
        dN <- r * y * (1 - y / cc)

        dN <- dN + sum((migration_baseline * y) / cc) - ((migration_baseline * n_len) * y) / cc
        # APPROACH: WEDGE
        # cc_modified <- cc - migration_offset 
        # mj <- (
        #   migration_baseline * pmax(y - (cc_modified - 1), 0)
        # ) / cc_modified
        # mj <- mj + migration_intercept
        # # all m_i terms without the current i'th term would be all m_j terms.
        # dN <- dN + sum(mj * y) - mj * y * n_len

        list(dN)
      })
    },
    ...
  ) %>%
    unclass() %>%
    as_tibble() %>%
    pivot_longer(
      starts_with("N"),
      names_pattern = "N(\\d+)",
      names_to = "id_patch",
      values_to = "N"
    ) %>%
    identity()
}

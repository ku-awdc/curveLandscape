#' Birth, death, and migration ODE model.
#'
#' Wedge and smooth refers to the presence of a non-differentiability in the state-dependent migration rate.
#'
#' The migration mechanism is only aware of the conditions in _source_. The
#' destination patch is evenly distributed amongst all present patches.
#'
#'
#' @param migration_baseline Double, normalised internally by (n-1)
#' @inheritParams deSolve::ode
ode_source_only_wedge <- function(growth_rate, carrying_capacity, n0, migration_offset = 0, migration_intercept = 0, migration_baseline, delta_t, t_max = 25, ...) {
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

        # APPROACH: WEDGE
        cc_modified <- cc - migration_offset 
        mj <- (
          migration_baseline * pmax(y - (cc_modified - 1), 0)
        ) / cc_modified
        mj <- mj + migration_intercept
        dN <- dN + sum(mj * y) - mj * y * n_len

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

#' @inherit ode_source_only_wedge description
ode_source_only_smooth <- function(growth_rate, carrying_capacity, n0, migration_offset = 0, migration_intercept = 0, migration_baseline, delta_t, t_max = 25) {
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
    y = N0, times = seq.default(0, t_max, by = delta_t),
    parms = list(r = growth_rate, cc = carrying_capacity, 
      migration_intercept = migration_intercept,
      m0 = migration_baseline,
      migration_offset = migration_offset
      ),
    func = function(times, y, parameters) {
      with(parameters, {
        dN <- r * y * (1 - y / cc)

        # APPROACH: SMOOTH OR LogSumExp
        cc_modified <- cc - migration_offset 
        mj <- (m0 * log1p(exp(y - cc_modified))) / (log1p(1) * cc_modified)
        mj <- mj + migration_intercept
        dN <- dN + sum(mj * y) - mj * y * n_len

        list(dN)
      })
    }
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

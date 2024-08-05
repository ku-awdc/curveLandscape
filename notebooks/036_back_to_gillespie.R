devtools::load_all()

ode_dd_bd_only <- function(growth_rate, carrying_capacity, n0, delta_t, t_max = 25) {
  deSolve::ode(
    y = c(N = n0), times = seq.default(0, t_max, by = delta_t),
    parms = list(r = growth_rate, cc = carrying_capacity),
    func = function(times, y, parameters) {
      with(parameters, {
        dN <- r * y * (1 - y / cc)
        list(dN)
      })
    }
  ) %>%
    unclass() %>%
    as_tibble()
}

ode_dd_bd_only(
  growth_rate = 4 - 1, carrying_capacity = 5, n0 = c(0.1), delta_t = 0.01, t_max = 5
) -> ode_output

ode_output %>%
  glimpse()

ode_output %>%
  identity() %>%
  {
    ggplot(.) +
      aes(time, N) +
      geom_line() +
      coord_fixed() +
      # lims(y = c(NA, 20)) +
      labs(color = NULL) +
      theme_bw(base_size = 15) +
      theme(legend.position = "bottom") +
      NULL
  }

fs::dir_create("figures")
ggsave(
  filename = "figures/036_ode_birth_death_only.svg",
  device = svglite::svglite
  # scaling = 3,
  # scale = 3,
  # width = 2*4.27,
  # height = 2.5
)

sim_bd_only(
  n0 = c(1L),
  growth_baseline = 4 - 1,
  carrying_capacity = 5L,
  t_max = 5,
  # reps = 1L
)


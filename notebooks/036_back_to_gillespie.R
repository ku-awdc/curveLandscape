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
  growth_rate = 4 - 1, carrying_capacity = 5, n0 = c(3), delta_t = 0.01, t_max = 5
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

devtools::load_all()
sim_bd_only(
  n0 = c(3L),
  birth_baseline = 4,
  death_baseline = 1,
  carrying_capacity = 5L,
  t_max = 5
) %>%
  as_tibble() %>%
  rename(N = state) %>%
  tail()


# sim_bd_only(
#   n0 = c(3L),
#   birth_baseline = 4,
#   death_baseline = 1,
#   carrying_capacity = 5L,
#   t_max = 5
# ) %>%
#   as_tibble()



stoch_output %>%
  identity() %>%
  {
    ggplot(.) +
      aes(time, N) +
      geom_step() +
      labs(color = NULL) +
      theme_bw(base_size = 15) +
      theme(legend.position = "bottom") +
      NULL
  }


devtools::load_all()
map(1:100, \(rep) {
  sim_output <- sim_bd_only(
    n0 = c(1L),
    birth_baseline = 4,
    death_baseline = 1,
    carrying_capacity = 5L,
    t_max = 5
  )
  sim_output$repetition <- rep
  sim_output %>%
    as_tibble() %>%
    rename(N = state)
}) %>%
  bind_rows() ->
stoch_output
stoch_output %>%
  identity() %>%
  {
    ggplot(.) +
      aes(time, N) +
      geom_step(
        # alpha = 0.6,
        aes(color = "Stochastic")
      ) +
      geom_line(
        # data = ode_output,
        data = ode_dd_bd_only(
          growth_rate = 4 - 1, carrying_capacity = 5, n0 = c(1), delta_t = 0.01, t_max = 5
        ),
        linewidth = .9,
        linetype = "dashed",
        aes(time, N, color = "ODE")
      ) +
      labs(color = NULL) +
      scale_colour_manual(
        values = c(
          "ODE" = "black",
          "Stochastic" = "lightblue"
        )
      ) +
      theme_bw(base_size = 15) +
      theme(legend.position = "bottom") +
      NULL
  }

stoch_output %>%
  identity() %>%
  {
    ggplot(.) +
      aes(time, N) +
      geom_step(
        alpha = 0.1
      ) +
      geom_smooth(
        se = FALSE,
        orientation = "x",
        formula = y ~ s(x, bs = "cs")
      ) +
      geom_line(
        # data = ode_output,
        data = ode_dd_bd_only(
          growth_rate = 4 - 1, carrying_capacity = 5, n0 = c(3), delta_t = 0.01, t_max = 5
        ),
        linewidth = .9,
        linetype = "dashed",
        aes(time, N, color = "ODE")
      ) +
      labs(color = NULL) +
      theme_bw(base_size = 15) +
      theme(legend.position = "bottom") +
      NULL
  }

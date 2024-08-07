devtools::load_all()

sim_bdm(
  n0 = as.integer(c(10, 0)),
  birth_baseline = c(4, 4),
  death_baseline = c(1, 1),
  carrying_capacity = as.integer(c(2, 8)),
  migration_baseline = 1/(8/12),
  t_max = 5
) -> output

# output
output %>%
  as_tibble() %>%
  rename(N = state) %>% 
  print(n=Inf)

output %>%
  as_tibble() %>%
  rename(N = state) %>%
  identity() %>%
  {
    ggplot(.) +
      aes(time, N, group = id_state) +
      geom_step(
        # alpha = 0.6,
        aes(color = factor(id_state))
      ) +
      labs(color = NULL) +
      # scale_colour_manual(
      #   values = c(
      #     "ODE" = "black",
      #     "Stochastic" = "lightblue"
      #   )
      # ) +
      expand_limits(y=0) +
      theme_bw(base_size = 15) +
      theme(legend.position = "bottom") +
      NULL
  }


# stoch_output %>%
#   identity() %>%
#   {
#     ggplot(.) +
#       aes(time, N) +
#       geom_step(
#         # alpha = 0.6,
#         aes(color = "Stochastic")
#       ) +
#       geom_line(
#         # data = ode_output,
#         data = ode_dd_bd_only(
#           growth_rate = 4 - 1, carrying_capacity = 5, n0 = c(1), delta_t = 0.01, t_max = 5
#         ),
#         linewidth = .9,
#         linetype = "dashed",
#         aes(time, N, color = "ODE")
#       ) +
#       labs(color = NULL) +
#       scale_colour_manual(
#         values = c(
#           "ODE" = "black",
#           "Stochastic" = "lightblue"
#         )
#       ) +
#       theme_bw(base_size = 15) +
#       theme(legend.position = "bottom") +
#       NULL
#   }

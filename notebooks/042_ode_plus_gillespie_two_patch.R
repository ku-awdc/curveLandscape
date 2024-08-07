devtools::load_all()

#'
#'
#' @inheritParams sim_bdm
#'
ssa_source_only_wedge <- function(
    n0,
    birth_baseline,
    death_baseline,
    carrying_capacity,
    migration_baseline,
    t_max) {
  sim_bdm(
    n0 = n0,
    birth_baseline = birth_baseline,
    death_baseline = death_baseline,
    carrying_capacity = carrying_capacity,
    migration_baseline = migration_baseline,
    t_max = t_max
  ) -> sim_output

  sim_output %>%
    as_tibble() %>%
    rename(N = count) %>%
    mutate(id_patch = id_patch + 1)
}

ssa_source_only_wedge(
  n0 = as.integer(c(10, 0)),
  birth_baseline = c(4, 4),
  death_baseline = c(1, 1),
  carrying_capacity = as.integer(c(2, 8)),
  migration_baseline = 1 / (8 / 12),
  t_max = 5
) -> ssa_output

ssa_output

#' let us do with two patches...

ode_source_only_wedge(
  # ode_source_only_smooth(
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

deter_output

deter_output %>%
  identity() %>%
  {
    ggplot(.) +
      aes(time, N, group = id_patch) +
      geom_line(
        # alpha = 0.6,
        aes(color = factor(id_patch)),
        show.legend = FALSE,
        linetype = "dashed"
      ) +
      
      geom_step(
        alpha  = 0.5,
        aes(
          group = str_c(id_patch, repetition),
          color = factor(id_patch)
        ),
        data = ssa_output
      ) +
      labs(
        color = NULL,
        y = "Patch-level count",
        x = "time [years]"
      ) +
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

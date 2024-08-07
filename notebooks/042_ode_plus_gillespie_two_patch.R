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
    t_max,
    reps = 100) {
  map(
    seq_len(reps), \(rep) {
      sim_bdm(
        n0 = n0,
        birth_baseline = birth_baseline,
        death_baseline = death_baseline,
        carrying_capacity = carrying_capacity,
        migration_baseline = migration_baseline,
        t_max = t_max
      ) %>%
        as_tibble() %>%
        mutate(repetition = rep)
    }
  ) %>%
    bind_rows() %>%
    rename(N = count) %>%
    mutate(id_patch = id_patch + 1)
}

ssa_source_only_wedge(
  n0 = as.integer(c(10, 0)),
  birth_baseline = c(4, 4),
  death_baseline = c(1, 1),
  carrying_capacity <- as.integer(c(2, 8)),
  migration_baseline = 1 / (8 / 12),
  t_max = 5,
  reps = 100
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

#' Add carrying capacity information for plotting...
deter_output <- deter_output %>%
  mutate(carrying_capacity = carrying_capacity[as.integer(id_patch)])

deter_output %>%
  identity() %>%
  {
    ggplot(.) +
      aes(time, N, group = id_patch) +
      geom_line(
        # alpha = 0.5,
        aes(color = factor(id_patch)),
        show.legend = FALSE,
        linetype = "dashed"
      ) +
      geom_line(
        alpha = 0.1,
        aes(
          group = str_c(id_patch, repetition),
          color = factor(id_patch)
        ),
        data = ssa_output,
        show.legend = FALSE
      ) +
      # stat_smooth(
      #   data = ssa_output,
      #   aes(
      #     color = factor(id_patch)
      #   ),
      #   linewidth = .8,
      #   se = FALSE
      # ) +
      labs(
        color = NULL,
        y = "Patch-level count",
        x = "time [years]"
      ) +
      geom_hline(
        aes(
          yintercept = carrying_capacity,
          # color = id_patch
        ),
        color = "forestgreen",
        linetype = "dotdash",
        linewidth = 0.3
      ) +

      # scale_colour_manual(
      #   values = c(
      #     "ODE" = "black",
      #     "Stochastic" = "lightblue"
      #   )
      # ) +
      # expand_limits(y = c(0, 2, 8)) +
      theme_bw(base_size = 15) +
      # ggpubr::theme_transparent() +
      theme(legend.position = "bottom") +
      NULL
  }



#' Filter the data as to only be `t_max = 1`

deter_output %>%
  filter(time <= 1) %>%
  identity() %>%
  {
    ggplot(.) +
      aes(time, N, group = id_patch) +
      geom_line(
        # alpha = 0.5,
        aes(color = factor(id_patch)),
        show.legend = FALSE,
        linetype = "dashed"
      ) +
      geom_line(
        alpha = 0.1,
        aes(
          group = str_c(id_patch, repetition),
          color = factor(id_patch)
        ),
        data = ssa_output %>%
          filter(time <= 1),
        show.legend = FALSE
      ) +
      labs(
        color = NULL,
        y = "Patch-level count",
        x = "time [years]"
      ) +
      geom_hline(
        aes(
          yintercept = carrying_capacity,
          # color = id_patch
        ),
        color = "forestgreen",
        linetype = "dotdash",
        linewidth = 0.3
      ) +
      expand_limits(y = c(0, 2, 8)) +
      theme_bw(base_size = 15) +
      theme(legend.position = "bottom") +
      NULL
  }

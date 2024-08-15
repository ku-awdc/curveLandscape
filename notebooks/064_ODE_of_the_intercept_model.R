devtools::load_all()

migration_intercept <- 0.5

carrying_capacity <- c(2, 8)
ode_source_only_smooth(
  growth_rate = 4 - 1,
  carrying_capacity = carrying_capacity,
  n0 = c(10, 0),
  migration_baseline = 1 / (8 / 12),
  migration_intercept = migration_intercept,
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
        # linetype = "dashed"
      ) +
      # geom_line(
      #   data = ssa_output,
      #   alpha = 0.1,
      #   aes(
      #     group = str_c(id_patch, repetition),
      #     color = factor(id_patch)
      #   ),
      #   show.legend = FALSE
      # ) +
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
        # linewidth = 0.3
      ) +

      # scale_colour_manual(
      #   values = c(
      #     "ODE" = "black",
      #     "Stochastic" = "lightblue"
      #   )
      # ) +
      # expand_limits(y = c(0, 2, 8)) +
      ylim(c(NA, 10)) +
      theme_bw(base_size = 15) +
      # ggpubr::theme_transparent() +
      theme(legend.position = "bottom") +
      NULL
  }


# fs::dir_create("figures")
# ggsave(
#   filename = "figures/042_ssa_ode_birth_death_migration_aio_ycutoff_10.svg",
#   device = svglite::svglite,
#   # scaling = 3,
#   scale = 1.9
#   # width = 2*4.27,
#   # height = 2.5
# )

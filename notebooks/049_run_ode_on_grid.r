devtools::load_all()

all_landscapes

naive_grid <- all_landscapes$grid[[5]]

ode_source_only_wedge(
  # n0 = ceiling(naive_grid$cc) %>% as.integer(),
  n0 = pmin(naive_grid$Capacity, 1),
  # birth_baseline = rep.int(4., nrow(naive_grid)),
  # death_baseline = rep.int(1., nrow(naive_grid)),
  growth_rate = 4 - 1,
  carrying_capacity = naive_grid$Capacity,
  migration_baseline = 1 / (8 / 12),
  delta_t = 1/52,
  t_max = 5,
  # reps = 100
) -> ode_output

ode_output %>%
  glimpse() %>% 
  identity() %>% {
    ggplot(.) +
      aes(time, N, group = id_patch) +
      geom_step(aes(alpha = N), show.legend = FALSE) +
      ggpubr::theme_pubclean(15) +
      # theme_light(15) +
      NULL
  }

  ode_output %>%
    glimpse() %>% 
    filter(time<=2) %>% 
    identity() %>% {
      ggplot(.) +
        aes(time, N, group = id_patch) +
        geom_step(aes(alpha = N), show.legend = FALSE) +
        labs(x = "time [years]") +
        ggpubr::theme_pubclean(15) +
        # theme_light(15) +
        NULL
    }

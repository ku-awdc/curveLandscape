devtools::load_all()

all_landscapes

naive_grid <- all_landscapes$grid[[5]]

all_landscapes_trans <- transpose(all_landscapes)


pdf("figures/049_run_ode_on_all_landscapes.pdf")

for (id_grid in seq_along(all_landscapes_trans)) {
  with(all_landscapes_trans[[id_grid]], {
    ode_source_only_wedge(
      # n0 = ceiling(grid$cc) %>% as.integer(),
      n0 = pmin(grid$Capacity, 1),
      # birth_baseline = rep.int(4., nrow(grid)),
      # death_baseline = rep.int(1., nrow(grid)),
      growth_rate = 4 - 1,
      carrying_capacity = grid$Capacity,
      migration_baseline = 1 / (8 / 12),
      delta_t = 1 / 52,
      t_max = 5,
      # reps = 100
    ) -> ode_output

    ode_output %>%
      glimpse() %>%
      identity() %>%
      {
        ggplot(.) +
          aes(time, N, group = id_patch) +
          geom_step(aes(alpha = N), show.legend = FALSE) +
          labs(x = "time [years]") +
          labs(caption = glue("Landscape type {landscape_type}, and total patches {n_len}")) +
          ggpubr::theme_pubclean(15) +
          # theme_light(15) +
          NULL
      } -> p_ode

    print(p_ode)

    ode_output %>%
      glimpse() %>%
      filter(time <= 2) %>%
      identity() %>%
      {
        ggplot(.) +
          aes(time, N, group = id_patch) +
          geom_step(aes(alpha = N), show.legend = FALSE) +
          labs(x = "time [years]") +
          labs(caption = glue("Landscape type {landscape_type}, and total patches {n_len}")) +
          ggpubr::theme_pubclean(15) +
          # theme_light(15) +
          NULL
      } -> p_ode_zoom

    print(p_ode_zoom)
  })
}

dev.off()

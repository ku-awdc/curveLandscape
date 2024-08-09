devtools::load_all()
library(conflicted)

# all_landscapes

# DEBUG
# grid <- all_landscapes$grid[[5]]
# landscape_type <- all_landscapes$landscape_type[[5]]
# n_len <- all_landscapes$n_len[[5]]

TimeMax <- 50
DeltaT <- 1/12

stopifnot(DeltaT <= TimeMax)
all_landscapes_trans <- purrr::transpose(all_landscapes)

pdf("figures/050_run_ssa_on_all_landscapes_minimum_n0.pdf")

# pdf("figures/050_run_ssa_on_all_landscapes_eq_cc.pdf")
# pdf("figures/050_run_ssa_on_all_landscapes_gt_cc.pdf")
# pdf("figures/050_run_ssa_on_all_landscapes_lt_cc.pdf")

for (id_grid in seq_along(all_landscapes_trans)) {
  with(all_landscapes_trans[[id_grid]], {
    
    ssa_source_only_wedge(
      n0 = pmin(grid$Capacity, 1) %>% as.integer(), # minimum

      # n0 = ceiling(grid$Capacity) %>% as.integer(), # eq_cc
      # n0 = pmax(0, ceiling(grid$Capacity - 1)) %>% as.integer(), # lt_cc
      # n0 = pmax(0, ceiling(grid$Capacity + 1)) %>% as.integer(), # gt_cc

      # n0 = pmin(grid$Capacity, 1) %>% as.integer(),
      # n0 = grid$Capacity,
      birth_baseline = rep.int(4., nrow(grid)),
      death_baseline = rep.int(1., nrow(grid)),
      # growth_rate = 4 - 1,
      carrying_capacity = grid$Capacity,
      migration_baseline = 1 / (8 / 12),
      # delta_t = 1 / 52,
      t_max = TimeMax,
      reps = 100
    ) -> ssa_output

    ssa_output

    # ssa_output %>%
    #   glimpse() %>%
    #   identity() %>%
    #   {
    #     ggplot(.) +
    #       aes(time, N, group = str_c(repetition, ":", id_patch)) +
    #       geom_step(aes(alpha = N, color = id_patch), show.legend = FALSE) +
    #       labs(x = "time [years]") +
    #       labs(caption = glue("Landscape type {landscape_type}, and total patches {n_len}")) +
    #       ggpubr::theme_pubclean(15) +
    #       # theme_light(15) +
    #       NULL
    #   }


    ssa_output <- ssa_output %>%
      rename(
        Replicate = repetition, Patch = id_patch, N = N,
        Time = time
      )
    ## Assume we know the data at time=0 (I guess this is in the model output?)
    ssa_output |>
      group_by(Replicate, Patch) |>
      arrange(Time) |>
      slice(1L) |>
      ungroup() |>
      mutate(Time = 0) |>
      bind_rows(
        ssa_output
      ) ->
    ssa_output

    ## Converting model output to something plottable:
    ssa_output |>
      distinct(Replicate, Patch) |>
      expand_grid(
        Time = seq(0, TimeMax, by = DeltaT)
      ) |>
      ## Remove time=0, as that is already in the continuous data:
      filter(Time > 0) |>
      mutate(Using = TRUE) |>
      bind_rows(
        ssa_output |> mutate(Using = FALSE)
      ) |>
      group_by(Replicate, Patch) |>
      arrange(Time) |>
      fill(N, .direction = "down") |>
      ungroup() |>
      filter(Using) |>
      select(-Using) ->
    discrete_data

    discrete_data |>
      group_by(Replicate, Time) |>
      summarise(MeanN = mean(N), .groups = "drop") |>
      ggplot(aes(x = Time, y = MeanN, group = factor(Replicate))) +
      geom_step(alpha = 0.1, show.legend = FALSE) +
      # geom_point(show.legend = FALSE) +
      ggpubr::theme_pubclean(15) +
      labs(x = "time [years]") +
      labs(caption = glue("Landscape type {landscape_type}, and total patches {n_len}")) +
      NULL ->
      # facet_wrap(~Replicate) %>%
    p_ssa_replicates

    print(p_ssa_replicates)



    # print(p_ode)

    # ode_output %>%
    #   glimpse() %>%
    #   filter(time <= 2) %>%
    #   identity() %>%
    #   {
    #     ggplot(.) +
    #       aes(time, N, group = id_patch) +
    #       geom_step(aes(alpha = N), show.legend = FALSE) +
    #       labs(x = "time [years]") +
    #       labs(caption = glue("Landscape type {landscape_type}, and total patches {n_len}")) +
    #       ggpubr::theme_pubclean(15) +
    #       # theme_light(15) +
    #       NULL
    #   } -> p_ode_zoom

    # print(p_ode_zoom)
  })
}

dev.off()

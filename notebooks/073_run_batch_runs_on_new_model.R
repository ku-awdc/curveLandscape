devtools::load_all()


all_landscapes

#' Pre-allocate slots for the binned_output, the stat_output, and ignore error for now...
all_landscapes$binned_output <- vector("list", length = nrow(all_landscapes))
all_landscapes$stat_binned_output <- vector("list", length = nrow(all_landscapes))
# all_landscapes$ode_output <- vector("list", length = nrow(all_landscapes))

# all_landscapes_trans <- purrr::transpose(all_landscapes)


# all_landscapes_trans

#' @param binned_ssa_output This is the output of `Fixed` recorders.
#'
summarised_binned_reps <- function(binned_ssa_output, binned_time, repetitions) {
  mean_binned <- rowMeans(binned_ssa_output)
  # Assumes things are symmetric....
  # sd_binned <- apply(binned_ssa_output, 1, sd)
  # se_binned <- sd_binned / sqrt(repetitions)
  # ci_lower <- mean_binned - qt(0.975, df = repetitions - 1) * se_binned
  # ci_upper <- mean_binned + qt(0.975, df = repetitions - 1) * se_binned

  ci_lower <- apply(binned_ssa_output, 1, quantile, probs = 0.025)
  ci_upper <- apply(binned_ssa_output, 1, quantile, probs = 1 - 0.025)

  tibble(
    time = binned_time,
    mean = mean_binned,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
}
#'
# migration_baseline <- 1 / (8 / 12)
# repetitions <- 250
# repetitions <- 50
repetitions <- 50
t_max <- 25
delta_t <- 1 / 12
# all_migration_baseline <- 1 / (8/12)
all_migration_baseline <- 0
binned_time <- seq.default(from = 0, to = t_max, by = delta_t)

fs::dir_create("figures")
pdf("figures/073_plot_m0_zero_new_model.pdf")
# DEBUG
# all_migration_baseline <- all_migration_baseline[[5]]
for (id_m0 in seq_along(all_migration_baseline)) {
  # migration_baseline <- 1 / (8 / 12)
  migration_baseline <- all_migration_baseline[id_m0]


  # Amend simulation details to the object
  all_landscapes <- structure(all_landscapes,
    simulation_configuration = list(
      migration_baseline = migration_baseline,
      migration_intercept = 0,
      repetitions = repetitions,
      t_max = t_max,
      delta_t = delta_t
    )
  )

  all_landscapes_trans <- purrr::transpose(all_landscapes)
  # binned_time <- seq.default(from = 0, to = t_max, by = delta_t)
  for (id_landscape in seq_along(all_landscapes_trans)) {
    landscape <- all_landscapes_trans[[id_landscape]]
    print("processing")
    print(landscape$set_patch_size)

    # print(all_landscapes, width = Inf)
    # browser()
    n0 <- landscape$grid$Capacity %>%
      round() %>%
      as.integer()

    p_landscape_caption <-
      labs(caption = glue("Landscape type {landscape$landscape_type}, with patch-size {landscape$set_patch_size} km^2, and total patches {landscape$n_len}."))

    # print(grid$Capacity)
    wild_ssa_model <- WildSSA$new(
      n0 = n0,
      birth_baseline = rep.int(4, times = landscape$n_len),
      death_baseline = rep.int(1, times = landscape$n_len),
      migration_intercept = 0,
      carrying_capacity = landscape$grid$Capacity,
      migration_baseline = migration_baseline
    )

    wild_ssa_model$run_and_record_fixed_time_population_par(
      t_max = t_max,
      fixed_time_points = binned_time,
      repetitions = repetitions,
      20002224
    ) -> wild_ssa_output

    df_binned_ssa_output <- wild_ssa_output %>%
      bind_rows() %>%
      rename(population_count = count) %>%
      select(-current_time, -current_count, -current_time_index) %>%
      mutate(id_time = seq_len(n()), .by = repetition)


    all_landscapes$binned_output[[id_landscape]] <- df_binned_ssa_output
    #' ## Binned pr repetition population count plot..

    df_binned_ssa_output %>%
      glimpse() %>%
      {
        ggplot(.) +
          aes(time, population_count, group = repetition) +
          geom_step(aes(alpha = population_count, color = repetition), show.legend = FALSE) +
          labs(x = "time [years]") +
          geom_hline(
            aes(yintercept = landscape$total_cc),
            linetype = "dotted",
            linewidth = 1.1
          ) +
          labs(y = "Population count by replicate") +
          p_landscape_caption +
          ggpubr::theme_pubclean(15) +
          NULL
      } -> p_binned_population_count
    print(p_binned_population_count)

    #'
    #' # Binned and statistical bandwidth of each signal..
    #'
    # stat_wild_ssa_output <- summarised_binned_reps(binned_ssa_output, binned_time, repetitions)

    # df_binned_ssa_output %>%
    #   count(id_time)

    stat_wild_ssa_output <-
      df_binned_ssa_output %>%
      group_by(id_time) %>%
      reframe(
        time = time[1],
        mean = mean(population_count),
        ci_lower = quantile(population_count, probs = 0.025),
        ci_upper = quantile(population_count, probs = 1 - 0.025)
      ) %>%
      select(-id_time)
    stat_wild_ssa_output
    all_landscapes$stat_binned_output[[id_landscape]] <- stat_wild_ssa_output

    stat_wild_ssa_output %>%
      glimpse() %>%
      {
        ggplot(.) +
          geom_line(aes(time, mean)) +
          geom_line(linetype = "dotted", aes(time, ci_lower)) +
          geom_line(linetype = "dotdash", aes(time, ci_upper)) +
          # geom_step(aes(alpha = count, color = repetition), show.legend = FALSE) +
          labs(x = "time [years]") +
          labs(y = "Mean population count") +
          geom_hline(
            aes(yintercept = landscape$total_cc),
            linetype = "dotted",
            linewidth = 1.1
          ) +
          p_landscape_caption +
          ggpubr::theme_pubclean(15) +
          # theme_light(15) +
          NULL
      } -> p_binned_stat_ssa_output
    # print(p_binned_stat_ssa_output)

    #' ## Full resolution
    #'
    #'
    if (FALSE) {
      total_cc

      wild_ssa_output <- wild_ssa_output %>%
        bind_rows()
      wild_ssa_output %>%
        glimpse() %>%
        identity() %>%
        {
          ggplot(.) +
            aes(time, count, group = repetition) +
            geom_step(aes(alpha = count, color = repetition), show.legend = FALSE) +
            labs(x = "time [years]") +
            geom_hline(
              aes(yintercept = total_cc),
              linetype = "dotted",
              linewidth = 1.1
            ) +
            p_landscape_caption +
            ggpubr::theme_pubclean(15) +
            # theme_light(15) +
            NULL
        }
    }
  }

  # write_rds(all_landscapes, ".cache/056_all_landscapes.rds")
  # all_landscapes <- read_rds(".cache/056_all_landscapes.rds")


  #' These landscapes were simulated with:
  attributes(all_landscapes)$simulation_configuration

  all_landscapes %>%
    mutate(
      landscape_type = fct_inorder(landscape_type),
      set_patch_size_label =  fct(set_patch_size %>% str_c(" km²"))
    ) %>%
    select(landscape_type:n_len, stat_binned_output, set_patch_size_label, -grid) %>%
    unnest(stat_binned_output) %>%
    identity() %>%
    glimpse() %>% 
    {
      ggplot(.) +
        aes(time, mean, group = str_c(landscape_type, set_patch_size_label)) +
        geom_line(
          aes(color = set_patch_size_label),
        ) +
        geom_hline(
          aes(yintercept = all_landscapes$total_cc[[1]]),
          linetype = "dotted",
          linewidth = 1.1
        ) +
        labs(x = "time [years]") +
        labs(y = "Mean Total Population over replicates") +
        labs(caption = glue_data(
          attributes(all_landscapes)$simulation_configuration,
          "Migration baseline {migration_baseline}; Replicates: {repetitions}"
        )) +
        scale_color_viridis_d(direction = 1) +
        # confidence interval... but they are very low..
        geom_line(linetype = "dotdash", aes(time, ci_lower, color = set_patch_size_label)) +
        geom_line(linetype = "dotdash", aes(time, ci_upper, color = set_patch_size_label)) +
        labs(color = NULL) +
        guides(color = guide_legend(override.aes = list(linewidth = 1.2))) +
        facet_grid(
          rows = vars(landscape_type),
          labeller = labeller(landscape_type = c(naive = "grid-based", habic = "habitat-based"))
        ) +
        ggpubr::theme_pubclean(15) +
        NULL
    } -> p_plot_all_landscapes_together

  print(p_plot_all_landscapes_together)
  write_rds(all_landscapes, glue(
    ".cache/073_m0_is_zero_new_model.rds"
  ))
  # fs::dir_create("figures")
  ggsave(
    p_plot_all_landscapes_together,
    filename = "figures/073_m0_is_0_all_landscapes.svg",
    device = svglite::svglite,
    # scaling = 3,
    scale = 1.9
    # width = 2*4.27,
    # height = 2.5
  )
}

beepr::beep("ding")
dev.off()

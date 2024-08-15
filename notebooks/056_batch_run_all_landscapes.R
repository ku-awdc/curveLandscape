devtools::load_all()


all_landscapes

#' Pre-allocate slots for the binned_output, the stat_output, and ignore error for now...
all_landscapes$binned_output <- vector("list", length = nrow(all_landscapes))
all_landscapes$stat_binned_output <- vector("list", length = nrow(all_landscapes))
all_landscapes$ode_output <- vector("list", length = nrow(all_landscapes))

# test
# all_landscapes$ode_output[[2]] <- as_tibble(iris)

all_landscapes_trans <- purrr::transpose(all_landscapes)
# all_landscapes_trans <- purrr::transpose(all_landscapes[-c(1, 6),])


all_landscapes_trans

# TODO: needs.. fixing...

# seed_point <- all_landscapes$grid[[2]][1283, ] %>% st_centroid()
# seed_point

# seed_indices <- all_landscapes$grid %>%
#   map_int(\(grid) {
#     st_filter(grid %>% rowid_to_column(), seed_point)$rowid
#     # browser()
#     # grid[st_within(grid, seed_point),]
#   })
# seed_indices
# map2(
#   all_landscapes$grid, seed_indices, \(x, y) {
#     x[y, ]
#   }
# ) %>%
#   bind_rows()



# naive_landscape <- all_landscapes_trans[[5]]
# naive_seed_point <- seed_indices[5]
# naive_landscape$grid[naive_seed_point, ]

# n0 <- integer(naive_landscape$n_len)
# n0[naive_seed_point] <- 1L
# n0

#' Issue: Seeding in one location is incurring extinction. We probably need to seed in a few more locations.
#' Teleportation makes the starting point not an issue. So go with that.
#'
#'
#'

summarised_binned_reps <- function(binned_ssa_output, binned_time, repetitions) {
  # binned_ssa_output <- cbind(
  #   # binned_time = binned_time,
  #   wild_ssa_output %>%
  #     lapply(\(output) {
  #       approx(output$time, output$count, binned_time)$y
  #     }) %>%
  #     {
  #       do.call(cbind, .)
  #     }
  # )
  mean_binned <- rowMeans(binned_ssa_output)
  sd_binned <- apply(binned_ssa_output, 1, sd)
  se_binned <- sd_binned / sqrt(repetitions)
  ci_lower <- mean_binned - qt(0.975, df = repetitions - 1) * se_binned
  ci_upper <- mean_binned + qt(0.975, df = repetitions - 1) * se_binned

  tibble(
    time = binned_time,
    mean = mean_binned,
    sd = sd_binned,
    se = se_binned,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
}
#'
migration_baseline <- 1 / (8 / 12)
repetitions <- 250
t_max <- 25
delta_t <- 1 / 12

# Amend simulation details to the object
all_landscapes <- structure(all_landscapes, 
  simulation_configuration = list(
    migration_baseline = migration_baseline,
    repetitions = repetitions,
    t_max = t_max,
    delta_t = delta_t
  )  
)

binned_time <- seq.default(from = 0, to = t_max, by = delta_t)
pdf("figures/056_batch_run_everything_wedge.pdf")
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
    carrying_capacity = landscape$grid$Capacity,
    migration_baseline = migration_baseline
  )

  wild_ssa_model$run_and_record_population(
    t_max = 25,
    repetitions = repetitions,
    20002224
  ) -> wild_ssa_output

  # Unused anywhere...
  # binned_outputs <- matrix(NA_integer_, nrow = length(binned_time), ncol = repetitions)
  binned_ssa_output <- cbind(
    # binned_time = binned_time,
    wild_ssa_output %>%
      lapply(\(output) {
        approx(output$time, output$count, binned_time)$y
      }) %>%
      {
        do.call(cbind, .)
      }
  )

  df_binned_ssa_output <- binned_ssa_output %>%
    set_colnames(seq_len(ncol(.))) %>%
    as_tibble() %>%
    mutate(
      time = binned_time,
    )
  all_landscapes$binned_output[[id_landscape]] <- df_binned_ssa_output
  #' ## Binned pr repetition population count plot..

  df_binned_ssa_output %>%
    pivot_longer(-time, names_to = "repetition", values_to = "population_count") %>%
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
  stat_wild_ssa_output <- summarised_binned_reps(binned_ssa_output, binned_time, repetitions)
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
  print(p_binned_stat_ssa_output)

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



  #' Compare with ODE and plot the resulting error curve.
  #'
  ode_source_only_wedge(
    growth_rate = 4 - 1,
    carrying_capacity = landscape$grid$Capacity,
    n0 = n0,
    migration_baseline = migration_baseline,
    delta_t = delta_t
  ) -> wild_ode_output


  wild_ode_output_population <- wild_ode_output %>%
    group_by(time) %>%
    reframe(population_count = sum(N))
  # wild_ode_output_population$time %>% plot()
  # binned_time %>% plot()
  stopifnot(
    zapsmall(sum(wild_ode_output_population$time - binned_time)) == 0
  )

  all_landscapes$ode_output[[id_landscape]] <- wild_ode_output_population

  print(p_binned_stat_ssa_output +
    geom_line(
      data = wild_ode_output_population,
      aes(time, population_count, color = "ODE"),
      linetype = "dotted"
    ) +
    labs(y = "Population count") +
    theme(legend.position = "bottom") +
    labs(color = NULL))
  error_reps <- binned_ssa_output - wild_ode_output_population$population_count

  #' ## Binned pr repetition population count plot..

  error_reps %>%
    set_colnames(seq_len(ncol(.))) %>%
    as_tibble() %>%
    mutate(
      time = binned_time,
    ) %>%
    pivot_longer(-time, names_to = "repetition", values_to = "error_count") %>%
    {
      ggplot(.) +
        aes(time, error_count, group = repetition) +
        geom_step(aes(alpha = error_count, color = repetition), show.legend = FALSE) +
        labs(x = "time [years]") +
        labs(y = "Error pr. replicate on population count") +
        # geom_hline(
        #   aes(yintercept = landscape$total_cc),
        #   linetype = "dotted",
        #   linewidth = 1.1
        # ) +
        p_landscape_caption +
        ggpubr::theme_pubclean(15) +
        # theme_light(15) +
        NULL
    } -> p_error_reps
  print(p_error_reps)

  stat_error_reps <- summarised_binned_reps(error_reps, binned_time, repetitions = repetitions)

  stat_error_reps %>%
    glimpse() %>%
    {
      ggplot(.) +
        geom_line(aes(time, mean)) +
        geom_line(linetype = "dotted", aes(time, ci_lower)) +
        geom_line(linetype = "dotdash", aes(time, ci_upper)) +
        # geom_step(aes(alpha = count, color = repetition), show.legend = FALSE) +
        labs(x = "time [years]") +
        labs(y = "Mean Error on Population Count") +
        p_landscape_caption +
        ggpubr::theme_pubclean(15) +
        # theme_light(15) +
        NULL
    } -> p_error_stat
  print(p_error_stat)
}

dev.off()
NULL

beepr::beep("ping")


# all_landscapes %>% print(width = Inf)

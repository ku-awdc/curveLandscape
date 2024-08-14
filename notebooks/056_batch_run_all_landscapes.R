devtools::load_all()

all_landscapes_trans <- purrr::transpose(all_landscapes)

seed_point <- all_landscapes$grid[[2]][1283, ] %>% st_centroid()
seed_point

seed_indices <- all_landscapes$grid %>%
  map_int(\(grid) {
    st_filter(grid %>% rowid_to_column(), seed_point)$rowid
    # browser()
    # grid[st_within(grid, seed_point),]
  })
seed_indices
map2(
  all_landscapes$grid, seed_indices, \(x, y) {
    x[y, ]
  }
) %>%
  bind_rows()



naive_landscape <- all_landscapes_trans[[5]]
naive_seed_point <- seed_indices[5]
naive_landscape$grid[naive_seed_point, ]

n0 <- integer(naive_landscape$n_len)
n0[naive_seed_point] <- 1L
n0

#' Issue: Seeding in one location is incurring extinction. We probably need to seed in a few more locations.
#' Teleportation makes the starting point not an issue. So go with that.
#'

n0 <- naive_landscape$grid$Capacity %>% as.integer()
migration_baseline <- 1 / (8 / 12)

wild_ssa_model <- WildSSA$new(
  # n0 = n0,
  n0 = naive_landscape$grid$Capacity %>% as.integer(),
  birth_baseline = rep.int(4, times = naive_landscape$n_len),
  death_baseline = rep.int(1, times = naive_landscape$n_len),
  carrying_capacity = naive_landscape$grid$Capacity,
  migration_baseline = migration_baseline
)



# WildSSA$run_and_record_patch
# WildSSA$run_and_record_population
# wild_ssa_model$run_and_record_patch(
#   t_max = 25,
#   repetitions = 1000,
#   20002224
# ) -> wild_ssa_output

# wild_ssa_output %>% map_int(
#   \(output) {
#     output$count %>% length()
#   }
# ) %>%
#   print() %>%
#   # table() %>%
#   sum() %>%
#   print()

wild_ssa_model$run_and_record_population(
  t_max = 25,
  repetitions = 100,
  20002224
) -> wild_ssa_output

# wild_ssa_output[[5]] %>%
#   glimpse() %>%
#   with({
#     tibble(
#       binned_time = seq.default(from = 0, to = 25, by = 1 / 52),
#       approx(time, count, binned_time) %>% `[`("y")
#     )
#   })
repetitions <- 250
t_max <- 25
delta_t <- 1 / 12
binned_time <- seq.default(from = 0, to = t_max, by = delta_t)
binned_outputs <- matrix(NA_integer_, nrow = length(binned_time), ncol = repetitions)

wild_ssa_model$run_and_record_population(
  t_max = t_max,
  repetitions = repetitions,
  seed = 20002224
) -> wild_ssa_output

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

#' ## Binned pr repetition population count plot..

binned_ssa_output %>%
  set_colnames(seq_len(ncol(.))) %>%
  as_tibble() %>%
  mutate(
    time = binned_time,
  ) %>%
  pivot_longer(-time, names_to = "repetition", values_to = "population_count") %>%
  {
    ggplot(.) +
      aes(time, population_count, group = repetition) +
      geom_step(aes(alpha = population_count, color = repetition), show.legend = FALSE) +
      labs(x = "time [years]") +
      geom_hline(
        aes(yintercept = naive_landscape$total_cc),
        linetype = "dotted",
        linewidth = 1.1
      ) +
      # labs(caption = glue("Landscape type {landscape_type}, and total patches {n_len}")) +
      ggpubr::theme_pubclean(15) +
      # theme_light(15) +
      NULL
  }
#'
#' # Binned and statistical bandwidth of each signal..
#'
summarised_ssa_output <- function(wild_ssa_output, binned_time, repetitions) {
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

stat_wild_ssa_output <- summarised_ssa_output(wild_ssa_output, binned_time, repetitions)

stat_wild_ssa_output %>%
  glimpse() %>%
  {
    ggplot(.) +
      geom_line(aes(time, mean)) +
      geom_line(linetype = "dotted", aes(time, ci_lower)) +
      geom_line(linetype = "dotdash", aes(time, ci_upper)) +
      # geom_step(aes(alpha = count, color = repetition), show.legend = FALSE) +
      labs(x = "time [years]") +
      geom_hline(
        aes(yintercept = naive_landscape$total_cc),
        linetype = "dotted",
        linewidth = 1.1
      ) +
      # labs(caption = glue("Landscape type {landscape_type}, and total patches {n_len}")) +
      ggpubr::theme_pubclean(15) +
      # theme_light(15) +
      NULL
  } -> p_binned_stat_ssa_output
print(p_binned_stat_ssa_output)

#' ## Full resolution
#'
#'
if (FALSE) {
  naive_landscape$total_cc

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
          aes(yintercept = naive_landscape$total_cc),
          linetype = "dotted",
          linewidth = 1.1
        ) +
        # labs(caption = glue("Landscape type {landscape_type}, and total patches {n_len}")) +
        ggpubr::theme_pubclean(15) +
        # theme_light(15) +
        NULL
    }
}


#' Compare with ODE and plot the resulting error curve.
#' 
ode_source_only_wedge(
  growth_rate = 4 - 1,
  carrying_capacity = naive_landscape$grid$Capacity,
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

p_binned_stat_ssa_output + 
  geom_line(data = wild_ode_output_population, 
    aes(time, population_count, color = "ODE"), 
  linetype = "dotted") + 
  labs(color = NULL)


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

error_reps <- abs(binned_ssa_output - wild_ode_output_population$population_count)


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
      geom_hline(
        aes(yintercept = naive_landscape$total_cc),
        linetype = "dotted",
        linewidth = 1.1
      ) +
      # labs(caption = glue("Landscape type {landscape_type}, and total patches {n_len}")) +
      ggpubr::theme_pubclean(15) +
      # theme_light(15) +
      NULL
  }
#'
#' # Binned and statistical bandwidth of each signal..
#'
summarised_binned_reps <- function(binned_ssa_output, binned_time, repetitions) {

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
      geom_hline(
        aes(yintercept = naive_landscape$total_cc),
        linetype = "dotted",
        linewidth = 1.1
      ) +
      # labs(caption = glue("Landscape type {landscape_type}, and total patches {n_len}")) +
      ggpubr::theme_pubclean(15) +
      # theme_light(15) +
      NULL
  }



NULL

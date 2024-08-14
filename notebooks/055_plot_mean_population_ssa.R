devtools::load_all()

TimeMax <- 50
DeltaT <- 1 / 12

stopifnot(DeltaT <= TimeMax)
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


wild_ssa_model <- WildSSA$new(
  # n0 = n0,
  n0 = naive_landscape$grid$Capacity %>% as.integer(),
  birth_baseline = rep.int(4, times = naive_landscape$n_len),
  death_baseline = rep.int(1, times = naive_landscape$n_len),
  carrying_capacity = naive_landscape$grid$Capacity,
  migration_baseline = 1 / (8 / 12)
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
binned_time <- seq.default(from = 0, to = 25, by = 1 / 12)
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
  # browser()
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
  }


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

# binned_ssa_output %>% {
#   tibble(
# TODO: these formulas should really be in the rust.. otherwise it becomes unmanageable.
#     binned_time = binned_time,
#     mean = colMeans(.),
#     sd <- apply(., 2, sd)
#     confint_bined_ssa_output <- sd_binned_ssa_output

#     se = sd_signal / sqrt(n),
#     ci_lower = mean_signal - qt(0.975, df = n - 1) * se,
#     ci_upper = mean_signal + qt(0.975, df = n - 1) * se
#   )
# }

NULL

# region: calculate how many records this cost

# wild_ssa_output[[50]] %>%
#   as_tibble()

# wild_ssa_output %>%
#   map_int(
#     \(output) {
#       output$count %>% length()
#     }
#   ) %>%
#   print() %>%
#   # table() %>%
#   sum() %>%
#   print()

# endregion

# bench::press(
#   repetitions = c(250, 500, 750, 1000),
#   {
#     bench::mark(
#       wild_ssa_patch_recorder = wild_ssa_model$run_and_record_patch(
#         t_max = 25,
#         repetitions = repetitions,
#         20002224
#       ),
#       check = FALSE
#     )
#   }
# ) -> bp_wild_ssa_runs

# plot(bp_wild_ssa_runs) + facet_grid(~.)

# WildSSA$internal_debug_display()
# wild_ssa_model$internal_debug_display()
# print(wild_ssa_model)

# pdf("figures/050_run_ssa_on_all_landscapes_minimum_n0.pdf")

# pdf("figures/050_run_ssa_on_all_landscapes_eq_cc.pdf")
# pdf("figures/050_run_ssa_on_all_landscapes_gt_cc.pdf")
# pdf("figures/050_run_ssa_on_all_landscapes_lt_cc.pdf")

# for (id_grid in seq_along(all_landscapes_trans)) {
# with(all_landscapes_trans[[id_grid]], {

# landscape <- all_landscapes_trans[[5]]

# ssa_source_only_wedge(
#   n0 = pmin(landscape$grid$Capacity, 1) %>% as.integer(), # minimum

#   n0 = ceiling(landscape$grid$Capacity) %>% as.integer(), # eq_cc
#   # n0 = pmax(0, ceiling(grid$Capacity - 1)) %>% as.integer(), # lt_cc
#   # n0 = pmax(0, ceiling(grid$Capacity + 1)) %>% as.integer(), # gt_cc

#   # n0 = pmin(grid$Capacity, 1) %>% as.integer(),
#   # n0 = grid$Capacity,
#   birth_baseline = rep.int(4., nrow(grid)),
#   death_baseline = rep.int(1., nrow(grid)),
#   # growth_rate = 4 - 1,
#   carrying_capacity = landscape$grid$Capacity,
#   migration_baseline = 1 / (8 / 12),
#   # delta_t = 1 / 52,
#   t_max = TimeMax,
#   reps = 100
# ) -> ssa_output

# naive_landscape$total_cc

# wild_ssa_output <- wild_ssa_output %>%
#   bind_rows()
# wild_ssa_output %>%
#   glimpse() %>%
#   identity() %>%
#   {
#     ggplot(.) +
#       aes(time, count, group = repetition) +
#       geom_step(aes(alpha = count, color = repetition), show.legend = FALSE) +
#       labs(x = "time [years]") +
#       geom_hline(
#         aes(yintercept = naive_landscape$total_cc),
#         linetype = "dotted",
#         linewidth = 1.1
#       ) +
#       # labs(caption = glue("Landscape type {landscape_type}, and total patches {n_len}")) +
#       ggpubr::theme_pubclean(15) +
#       # theme_light(15) +
#       NULL
#   }

devtools::load_all()

# all_landscapes %>%
#   glimpse()

migration_baseline <- 1 / (8 / 12)
repetitions <- 100
t_max <- 25
delta_t <- 1 / 12
migration_intercept <- 0
binned_time <- seq.default(from = 0, to = t_max, by = delta_t)

landscape <- all_landscapes %>%
  transpose() %>%
  `[[`(10)

n0 <- landscape$grid$Capacity %>%
  round() %>%
  as.integer()

wild_ssa_model <- WildSSA$new(
  n0 = n0,
  birth_baseline = rep.int(4, times = landscape$n_len),
  death_baseline = rep.int(1, times = landscape$n_len),
  carrying_capacity = landscape$grid$Capacity,
  migration_intercept = migration_intercept,
  migration_baseline = migration_baseline
)

n0
# wild_ssa_model$run_and_record_population(
#   t_max = t_max,
#   repetitions = repetitions,
#   20002224
# ) -> wild_ssa_output_pop

# wild_ssa_model$run_and_record_fixed_time_population(
#   t_max = t_max,
#   time_intervals = binned_time,
#   repetitions = repetitions,
#   20002224
# ) -> wild_ssa_output

# wild_ssa_output[[1]]$time %>% length()
# wild_ssa_output[[1]]$time %>% length()

# wild_ssa_output %>% transpose() %>% names()
# wild_ssa_output %>% transpose() %>% `[[`("time") %>% lengths()


# wild_ssa_output[[1]] %>%
#   as_tibble()

# wild_ssa_output_pop[[1]] %>%
#   as_tibble()

# approx(
#   wild_ssa_output_pop[[1]]$time,
#   wild_ssa_output_pop[[1]]$count,
#   binned_time,
#   method = "constant"
# )$y -> approx_y
# approx_y %>% length()
# wild_ssa_output[[1]]$count %>% length()
# wild_ssa_output[[1]]$count - approx_y

# binned_ssa_output <- cbind(
#   # binned_time = binned_time,
#   wild_ssa_output %>%
#     lapply(\(output) {
#       approx(output$time, output$count, binned_time, method = "constant")$y
#     }) %>%
#     {
#       do.call(cbind, .)
#     }
# )


# wild_ssa_output %>%
#   # DEBUG
#   # `[`(c(3,6)) %>%
#   bind_rows() %>%
#   {
#     ggplot(.) +
#       aes(time, count, group = repetition) +
#       geom_line(show.legend = FALSE, aes(color = repetition)) +
#       geom_hline(
#         aes(yintercept = landscape$total_cc),
#         linetype = "dotted",
#         linewidth = 1.1
#       ) +
#       ggpubr::theme_pubclean(15) +
#       NULL
#   }



bench::mark(
  # all_records = wild_ssa_model$run_and_record_population(
  #   t_max = t_max,
  #   repetitions = repetitions,
  #   20002224
  # ),
  # recording_fixed_intervals = wild_ssa_model$run_and_record_fixed_time_population(
  #   t_max = t_max,
  #   time_intervals = binned_time,
  #   repetitions = repetitions,
  #   20002224
  # ),
  recording_fixed_intervals = wild_ssa_model$run_and_record_fixed_time_population_par(
    t_max = t_max,
    time_intervals = binned_time,
    repetitions = repetitions,
    20002224
  ),
  check = FALSE
) -> bm_wild_ssa
bm_wild_ssa %>% print(width = Inf)
bm_wild_ssa %>% plot()

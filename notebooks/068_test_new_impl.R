devtools::load_all()

all_landscapes %>% 
  glimpse()

migration_baseline <- 1 / (8 / 12)
repetitions <- 10
t_max <- 25
delta_t <- 1 / 12
migration_intercept <- 0

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
wild_ssa_model$run_and_record_population(
  t_max = t_max,
  repetitions = repetitions,
  20002224
) -> wild_ssa_output
wild_ssa_output %>% 
  bind_rows() %>% {
    ggplot(.) +
      aes(time, count, group = repetition) +
      geom_line(show.legend = FALSE, aes(color = repetition)) +
      NULL
  }

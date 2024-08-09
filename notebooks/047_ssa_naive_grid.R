devtools::load_all()
naive_grid <- create_naive_grid(25) %>%
  mutate(
    cc = as.numeric(Area) * 0.75 * Prop_High +
      as.numeric(Area) * 0.25 * Prop_Low
  ) %>%
  # summarise(total_cc = sum(cc)) %>%
  identity()

nrow(naive_grid)

ssa_source_only_wedge(
  n0 = ceiling(naive_grid$cc) %>% as.integer(),
  birth_baseline = rep.int(4., nrow(naive_grid)),
  death_baseline = rep.int(1., nrow(naive_grid)),
  carrying_capacity = naive_grid$cc,
  migration_baseline = 1 / (8 / 12),
  t_max = 100,
  reps = 100
) -> ssa_output


ssa_output %>%
  mutate(time = time %/% (1 / 12)) %>%
  group_by(repetition, time) %>%
  summarise(total_n = sum(N)) %>%
  identity() %>%
  {
    ggplot(.) +
      aes(time, total_n, group = repetition) +
      geom_line(aes(color = as.factor(repetition)), show.legend = FALSE) +
      # scale_y_log10() +
      # ylim(c(0, 1))+
      NULL
  }

ssa_output %>% 
  # mutate(time = time %/% (1 / 12)) %>%
  filter(time >= 10, N > 1) %>%
  arrange(desc(N))

ode_source_only_wedge(
  growth_rate = rep.int(3, times = nrow(naive_grid)),
  carrying_capacity = naive_grid$cc,
  n0 = naive_grid$cc,
  migration_baseline = 1 / (8 / 12), delta_t = 1 / 12,
  t_max = 10
) -> ode_output


ode_output %>% print(n=100)

# devtools::load_all();


tibble(
  beta_baseline = 4,
  mu_baseline = 1,
  r_baseline = beta_baseline - mu_baseline,
  cc = 5,
) %>%
  expand_grid(
    N = seq.default(0, 20, by = 0.1)
  ) %>%
  mutate(
    # birth_death_sum = mu_baseline + (r_baseline / cc),
    death = mu_baseline + (r_baseline / cc) * pmax(0, N - cc),
    birth = mu_baseline + (r_baseline / cc) * pmax(0, cc - N),
  ) %>%
  pivot_longer(c(birth, death),
    names_to = "process_name",
    values_to = "rate"
  ) %>%
  mutate(
    # rate_unit = rate,
    rate_count = rate * N,
  ) ->
df_rate_plot

df_rate_plot %>%
  identity() %>%
  {
    ggplot(.) +
      aes(N, rate, group = process_name) +
      # geom_step(aes(color = process_name)) +
      geom_line(aes(color = process_name)) +
      labs(color = NULL) +
      lims(y = c(NA, 10)) +
      coord_fixed() +
      theme_bw(base_size = 15) +
      theme(legend.position = "bottom") +
      NULL
  }

fs::dir_create("figures")
ggsave(
  filename = "figures/037_birth_death_rate.svg",
  device = svglite::svglite
  # scaling = 3,
  # scale = 3,
  # width = 2*4.27,
  # height = 2.5
)

df_rate_plot %>%
  identity() %>%
  {
    ggplot(.) +
      aes(N, rate * N, group = process_name) +
      labs(y = "rate times count") +
      # geom_step(aes(color = process_name)) +
      geom_line(aes(color = process_name)) +
      coord_fixed() +
      lims(y = c(NA, 20)) +
      labs(color = NULL) +
      theme_bw(base_size = 15) +
      theme(legend.position = "bottom") +
      NULL
  }

fs::dir_create("figures")
ggsave(
  filename = "figures/037_birth_death_rate_times_count.svg",
  device = svglite::svglite
  # scaling = 3,
  # scale = 3,
  # width = 2*4.27,
  # height = 2.5
)

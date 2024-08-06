#'
#'
#' From: Inferring density-dependent population dynamics
#' mechanisms through rate disambiguation for logistic
#' birth-death processes
#'


tibble(
  beta_baseline = 4,
  mu_baseline = 1,
  r_baseline = beta_baseline - mu_baseline,
  cc = 5,
) %>%
  expand_grid(
    N = seq.default(0, 15, by = 0.1),
    gamma_param = c(0, .5, 1), # in [0, 1]
  ) %>%
  mutate(
    # OUR APPROACH
    # death = mu_baseline + (r_baseline / cc) * pmax(0, N - cc),
    # birth = mu_baseline + (r_baseline / cc) * pmax(0, cc - N),
    # gamma_param = 1, # in [0, 1]
    birth = pmax(0, beta_baseline - gamma_param * (r_baseline / cc) * N),
    death = mu_baseline + (1 - gamma_param) * (r_baseline / cc) * N,
    #
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
      aes(N, rate, group = str_c(process_name, gamma_param)) +
      # geom_step(aes(color = process_name)) +
      geom_line(aes(color = process_name)) +
      geom_vline(
        aes(xintercept = cc, color = "cc"),
        linetype = "dotted"
      ) +
      labs(color = NULL) +
      facet_grid(gamma_param ~ ., scales = "free_y") +
      # lims(y = c(NA, 10)) +
      # coord_fixed() +
      theme_bw(base_size = 15) +
      theme(legend.position = "bottom") +
      NULL
  }

fs::dir_create("figures")
ggsave(
  filename = "figures/039_birth_death_rate.svg",
  device = svglite::svglite,
  # scaling = 3,
  scale = 1.3
  # width = 2*4.27,
  # height = 2.5
)

df_rate_plot %>%
  identity() %>%
  {
    ggplot(.) +
      aes(N, rate * N, group = str_c(process_name, gamma_param)) +
      labs(y = "rate times count") +
      geom_line(aes(color = process_name)) +
      geom_vline(
        aes(xintercept = cc, color = "cc"),
        linetype = "dotted"
      ) +
      facet_grid(gamma_param ~ ., scales = "free_y") +
      labs(color = NULL) +
      theme_bw(base_size = 15) +
      theme(legend.position = "bottom") +
      NULL
  }

fs::dir_create("figures")
ggsave(
  filename = "figures/039_birth_death_rate_times_count.svg",
  device = svglite::svglite,
  # scaling = 3,
  scale = 1.3
  # width = 2*4.27,
  # height = 2.5
)

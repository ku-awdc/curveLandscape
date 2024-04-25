#'
#'
#'
#'
expand_grid(
  beta_0 = c(.4, 0.7),
  mu_0 = c(0.6),
  K_mu = c(8),
  K_beta = c(4, 10),
  alpha_mu = 0.1
) %>%
  rowid_to_column("config") %>%
  mutate(cat_rate = mu_0 < beta_0,
         cat_cc  = K_mu < K_beta,
         cat_rate = c("mu >= beta", "mu < beta")[1 + cat_rate],
         cat_cc = c("K_mu >= K_beta", "K_mu < K_beta")[1 + cat_cc]
  ) %>%
  mutate(
    N = map(max(K_beta, K_mu),
            \(K) seq.default(0, K * 1.5, by = 1))
  ) %>%
  # glimpse()
  unnest(N) %>%
  # mutate(beta = beta_0 * (1 - N * (beta_0 - mu_0) / (K * (beta_0 + mu_0)))) %>%
  # mutate(beta = beta_0 * pmax(0, (1 - N * (beta_0 - mu_0) / (K * (beta_0 + mu_0))))) %>%
  mutate(
    beta = beta_0 / K_beta * pmax(0, -(N - K_beta)),
    mu = mu_0 / K_mu * pmax(0, -(N - K_mu)) + alpha_mu * pmax(0, N - K_mu),
    # beta = (beta_0 * pmax(0, -(N - K_beta))) / K_beta,
    # mu = mu_0 / K_mu * pmin(0, (N - K_mu)) + alpha_mu * pmax(0, (N - K_mu)),
  ) %>%

  pivot_longer(c(beta, mu), names_to = "rate") %>%
  identity() %>%  {
    ggplot(.) +
      aes(N, value, group = str_c(rate, config)) +
      geom_line(aes(color = rate)) +
      # geom_step(aes(color = factor(config))) +

      # geom_vline(
      #   aes(xintercept = beta_limit),
      #   data = . %>% mutate(beta_limit = ceiling((beta_0 + mu_0)/(beta_0 - mu_0) * K),
      #                       beta_limit = if_else(beta_limit>0, beta_limit, NA))
      # ) +

      geom_vline(
        aes(xintercept = K_beta),
        linetype = "dotted"
      ) +
      geom_vline(
        aes(xintercept = K_mu, group = str_c(cat_cc, cat_rate)),
        linetype = "dotted"
      ) +

      facet_grid(cat_rate ~ cat_cc) +

      labs(y = "rate") +
      lims(y = c(0, NA)) +
      theme_bw(20) +
      theme(legend.position = "bottom") +
      theme(aspect.ratio = 1) +
      guides(color = "none") +
      NULL
  }
ggsave(
  # height = 14.5,
  device = svglite::svglite,
  scale = 2,
  filename = "figures/023_cases_in_params.svg"
)

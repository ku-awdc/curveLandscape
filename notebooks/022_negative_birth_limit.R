

# beta(N) = beta_0 (1 - (beta_0 - mu_0) / (beta_0 + mu_0) N/K), \

expand_grid(
  beta_0 = c(1,4),
  mu_0 = c(4,1),
  K = 13,
) %>%
  rowid_to_column("config") %>%
  glimpse() %>%
  mutate(
    N = map(K, \(K) seq.default(0, K * 2, by = 1))
  ) %>%
  unnest(N) %>%
  # mutate(beta = beta_0 * (1 - N * (beta_0 - mu_0) / (K * (beta_0 + mu_0)))) %>%
  mutate(beta = beta_0 * pmax(0, (1 - N * (beta_0 - mu_0) / (K * (beta_0 + mu_0))))) %>%

  identity() %>%  {
    ggplot(.) +
      aes(N, beta, group = config) +
      geom_line(aes(color = factor(config))) +
      # geom_step(aes(color = factor(config))) +

      geom_vline(
        aes(xintercept = beta_limit),
        data = . %>% mutate(beta_limit = ceiling((beta_0 + mu_0)/(beta_0 - mu_0) * K),
                            beta_limit = if_else(beta_limit>0, beta_limit, NA))
      ) +

      labs(y = "rate") +
      lims(y = c(0, NA)) +
      theme_bw(14) +
      theme(legend.position = "bottom") +
      guides(color = "none") +
      NULL
  }


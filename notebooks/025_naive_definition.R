#'
#'
#'
expand_grid(
  beta_0 = c(.1),
  mu_0 = c(0.4),
  K = 5
) %>%
  rowid_to_column("config") %>%
  mutate(
    N = map(max(K),
            \(K) seq.default(0, K * 2, by = .1))
  ) %>%
  # glimpse()
  unnest(N) %>%
  mutate(
    # beta = (beta_0) * pmax(0, 1 - N / K),
    # mu = (mu_0) * pmax(0, -(1 - N / K))

    # beta = (beta_0 - mu_0) * pmax(0, 1 - N / K),
    # mu = (beta_0 - mu_0) * pmax(0, -(1 - N / K))

    #  APPROACH
    #     beta =
    #       (beta_0 + mu_0) * pmax(0, 1 - N / K) +
    #       (beta_0 + mu_0) * pmax(0, (N / K) - 1),
    #     mu =
    #       (mu_0 + beta_0)/K * pmax(0, 1 - N / K) +
    #       (mu_0 + beta_0)/K * pmax(0, N / K - 1)

    beta = mu_0 - (beta_0 - mu_0) * pmax(0, K - N) / K,
    mu = mu_0 - (beta_0 - mu_0) * pmax(0, N - K) / K,
    mu = -mu,

  ) %>%
  mutate(growth = beta + mu) %>%
  pivot_longer(c(beta, mu, growth), names_to = "rate") %>%
  mutate(is_growth = rate == "growth",
         rate_label = dplyr::if_else(
           is_growth, "growth",
           "birth / death"
         )
  ) %>%
  # glimpse() %>%
  identity() %>%  {
    ggplot(.) +
      aes(N, value, group = str_c(rate, config)) +
      geom_line(aes(color = rate)) +
      # geom_step(aes(color = factor(config))) +

      # facet_grid(is_growth~.) +
      # facet_grid(rate_label~.) +
      facet_wrap(~rate_label, ncol = 1) +

      geom_line(
        aes(y = (beta_0 - mu_0) * (1 - N / K),
            color = "r(1 - N/K)"),
        linetype = "dotted",
        linewidth = 1.1,
        data = . %>% dplyr::filter(is_growth)
      ) +
      # geom_line(
      #   aes(y = abs((beta_0 - mu_0) * (1 - N / K))),
      #   linetype = "dotted",
      #   linewidth = 1.1,
      #   data = . %>% dplyr::filter(is_growth)
      # ) +

      geom_hline(
        aes(yintercept = beta_0, color = "beta_0"),
        linetype = "dashed",
        data = . %>% dplyr::filter(!is_growth)
      ) +
      geom_hline(
        aes(yintercept = -mu_0, color = "mu_0"),
        linetype = "dashed",
        data = . %>% dplyr::filter(!is_growth)
      ) +

      labs(y = "rate", color = NULL) +
      # lims(y = c(0, NA)) +
      theme_bw(20) +
      theme(legend.position = "left") +
      theme(aspect.ratio = 1) +
      # guides(color = "none") +
      theme(
        legend.margin = margin(),
      ) +
      guides(color = guide_legend(
        direction = "vertical",
        # theme = theme(legend.direction = "horizontal"),
        override.aes = list(linewidth = 1.5))) +
      NULL
  }
ggsave(
  # height = 14.5,
  device = svglite::svglite,
  scale = 2,
  filename = "figures/025_naive_definition.svg"
)

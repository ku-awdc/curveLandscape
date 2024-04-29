#'
#'
#'
carrying_capacity <- function(beta_0, mu_0, alpha_mu, K_beta, K_mu) {
  # browser()
  NA_real_
  # c(
  #   # =(mu_0 > beta_0) and (K_mu > K_beta)
  #   NA,
  #   # =(mu_0 > beta_0) and (K_mu < K_beta)
  #   ((beta_0 - mu_0) * K_beta * K_mu) / (beta_0 * K_mu - mu_0 * K_beta),
  #   # =(mu_0 < beta_0) and (K_mu > K_beta)
  #   ((beta_0 - mu_0) * K_beta * K_mu) / (beta_0 * K_mu - mu_0 * K_beta),
  #   # 1,
  #   # NA,
  #   # =(mu_0 < beta_0) and (K_mu < K_beta)
  #   (K_beta * (beta_0 + alpha_mu * K_mu)) / (beta_0 + K_beta * alpha_mu)
  # )[
  #   2 * as.numeric(mu_0 < beta_0) +
  #     as.numeric(K_mu < K_beta) +
  #     1
  # ]
}
#'
#'
expand_grid(
  beta_0 = c(.4, 0.7),
  mu_0 = c(0.6),
  K_mu = c(8.5),
  K_beta = c(4, 12),
  alpha_mu = 0.1
) %>%
  rowid_to_column("config") %>%
  mutate(cat_rate = mu_0 < beta_0,
         cat_cc  = K_mu < K_beta,
         cat_rate = c("mu >= beta", "beta > mu")[1 + cat_rate],
         cat_cc = c("K_mu >= K_beta", "K_mu < K_beta")[1 + cat_cc]
  ) %>%
  mutate(cc = carrying_capacity(beta_0, mu_0, alpha_mu, K_beta, K_mu)) %>%
  print() %>%
  mutate(
    N = map(max(K_beta, K_mu),
            \(K) seq.default(0, K * 1.05, by = .5))
  ) %>%
  # glimpse()
  unnest(N) %>%
  # mutate(beta = beta_0 * (1 - N * (beta_0 - mu_0) / (K * (beta_0 + mu_0)))) %>%
  # mutate(beta = beta_0 * pmax(0, (1 - N * (beta_0 - mu_0) / (K * (beta_0 + mu_0))))) %>%
  mutate(
    beta = beta_0 / K_beta * pmax(0, -(N - K_beta)),
    mu = mu_0 / K_mu * pmax(0, -(N - K_mu)) + alpha_mu * pmax(0, N - K_mu),
  ) %>%

  pivot_longer(c(beta, mu), names_to = "rate") %>%
  # glimpse() %>%
  identity() %>%  {
    ggplot(.) +
      aes(N, value, group = str_c(rate, config)) +
      geom_line(aes(color = rate)) +
      # geom_step(aes(color = factor(config))) +


    geom_vline(
      aes(
        color = "cc",
        xintercept = ((beta_0-mu_0) * K_beta * K_mu) / (beta_0 * K_mu - mu_0  * K_beta)
      ),
      # color = "green",
      data = . %>% dplyr::filter(
        ((K_mu >= K_beta) & (beta_0 > mu_0)) |
          (K_mu < K_beta) & (beta_0 <= mu_0)
      )
    ) +
      geom_vline(
        # color = "red",
        data = . %>% dplyr::filter((K_mu < K_beta)),
        aes(
          color = "cc",
          xintercept = (K_beta * (beta_0 + alpha_mu * K_mu))/(beta_0 + K_beta * alpha_mu)
        )) +

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
  filename = "figures/023_cases_in_params_with_cc.svg"
)

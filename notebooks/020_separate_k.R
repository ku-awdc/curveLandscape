

birth <- function(N, beta_0, K_beta) {
  beta_0 / K_beta * pmax(0, (K_beta - N))
}

death <- function(N, mu_0, K_mu, alpha_mu) {
  (mu_0/K_mu) * pmax(0, N - K_mu) - alpha_mu * pmin(0, N - K_mu)
}

carrying_capacity <- function(beta_0, K_beta, mu_0, K_mu, alpha_mu) {
  (beta_0 * K_beta + alpha_mu * K_beta * K_mu) /
    (beta_0 + alpha_mu * K_beta)
}

tibble(
  N = seq.default(0, 25, by = 1),
) %>%
  expand_grid(
    beta_0 = 4,
    mu_0 = 0.5,
    K_beta = 14,
    K_mu = 10,
    # alpha_mu = 0.1
    alpha_mu = mu_0 / K_mu
  ) %>%
  glimpse() %>%
  mutate(
    birth = birth(N, beta_0, K_beta),
    death = death(N, mu_0, K_mu, alpha_mu),
    growth = birth - death,
    carrying_capacity = carrying_capacity(beta_0, K_beta, mu_0, K_mu, alpha_mu),
    p_extinct = death / birth
  ) %>%
  pivot_longer(c(birth, death, growth), names_to = "rate") %>%
  identity() %>% {
    ggplot(.) +
      aes(N, value, group = rate) +
      geom_line(aes(color = rate)) +

      geom_vline(
        aes(xintercept = carrying_capacity)
      ) +
      guides(color = guide_legend(override.aes = list(linewidth=3))) +
      labs(color = NULL) +
      theme_bw(14) +
      NULL
  }


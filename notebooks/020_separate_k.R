
ode_params <- list(
  beta_0 = 4,
  mu_0 = 1,
  K_beta = 120,
  K_mu = 15,
  alpha_mu = 0.1
)

birth <- function(N, beta_0, K_beta) {
  beta_0 / K_beta * pmax(0, K_beta - N)
}

death <- function(N, mu_0, K_mu, alpha_mu) {
  (-mu_0/K_mu) * pmin(0, N - K_mu) + alpha_mu * pmax(0, N - K_mu)
}

carrying_capacity <- function(beta_0, K_beta, mu_0, K_mu, alpha_mu) {
  (
    (K_beta * K_mu * (beta_0  + mu_0)) /
      (beta_0 * K_mu + mu_0 * K_beta)
  ) * (K_beta > K_mu) +
    (
      (K_beta * K_mu * (beta_0  - mu_0)) /
        (beta_0 * K_mu - mu_0 * K_beta)
    ) * (K_beta <= K_mu)

  # (K_beta * K_mu * (beta_0  - mu_0 )) /
  #   (beta_0 * K_mu - mu_0 * K_beta )
}

expand_grid(ode_params %>% as_tibble()) %>%
  mutate(
    carrying_capacity = carrying_capacity(beta_0, K_beta, mu_0, K_mu, alpha_mu)
  ) %>%
  expand_grid(
    N = seq.default(0, (2) * max(carrying_capacity), by = 1)
  ) %>%
  glimpse() %>%
  mutate(
    birth = birth(N, beta_0, K_beta),
    death = death(N, mu_0, K_mu, alpha_mu),
    growth = birth - death,
    # carrying_capacity = carrying_capacity(beta_0, K_beta, mu_0, K_mu, alpha_mu),
    p_extinct = death / birth
  ) %>%
  pivot_longer(c(birth, death, growth), names_to = "rate") %>%
  identity() %>% {
    ggplot(.) +
      aes(N, value, group = rate) +
      geom_line(aes(color = rate)) +

      geom_hline(yintercept = 0) +

      geom_vline(
        aes(xintercept = carrying_capacity),
        linetype = "dotted"
      ) +

      guides(color = guide_legend(override.aes = list(linewidth=3))) +
      labs(color = NULL, y = "rate", x = "N [count]") +
      theme_bw(14) +
      theme(aspect.ratio = 1) +
      theme(legend.position = "bottom") +
      NULL
  }
#' Let's look at the resulting ODE
#'
#'
u0 <- seq.default(0, 10, by = 1)
ode_results <- deSolve::ode(
  y = c(u = u0), times = seq.default(0, 10, by = 0.001),
  parms = ode_params,
  func = function(times, N, parameters) {
    with(parameters, {
      # dN <- r * y * (1 - y / k0)
      dN <- N * (birth(N, beta_0, K_beta) - death(N, mu_0, K_mu, alpha_mu))
      list(dN)
    })
  }
)
ode_results <-
  ode_results %>%
  unclass() %>%
  as_tibble() %>%
  rename(t = time) %>%
  pivot_longer(
    -t,
    names_pattern = "u(\\d+)",
    names_to = c("id_u0"),
    values_to = "u") %>%
  mutate(u0 = u0[as.integer(id_u0)],
         id_u0 = NULL)
#'
ode_results %>% glimpse()
#'
ode_results %>%
  ggplot() +
  aes(t, u, group = u0) +
  geom_line(aes(color = u0)) +

  guides(color = guide_legend(override.aes = list(linewidth=3))) +
  scale_color_binned() +
  labs(color = NULL) +
  theme_bw(14) +
  NULL
#'
#'
ode_results %>%
  summarise(K_ode = max(u), .by = u0) %>%
  distinct(K_ode) %>%
  mutate(carrying_capacity = exec(carrying_capacity, !!!ode_params))
#'
#'
#'
#'




tibble(
  K = 9,
  # N = c(seq.default(0, K), seq.default(K, K + 5))
  N = c(seq.default(0, K), seq.default(K, 2*K))
) %>%
  expand_grid(
    # growth = 1.1,
    # death = 0.1
    birth = 4,
    death = 0.5,
  ) %>%
  mutate(
    growth_baseline = birth - death,
    growth = pmax(0, growth_baseline * (1 - N / K)),
    death = pmax(0, N - K) * growth_baseline / K
  ) %>%
  pivot_longer(c(growth, death)) %>%
  identity() %>%
  ggplot() +
  aes(N, value, group = name) +
  geom_line(aes(color = name)) +

  geom_vline(aes(xintercept = K), linetype = "dotted") +
  geom_text(
    aes(label = "Carrying Capacity", x = K, y = -Inf,
    )
  ) +
  # annotate(
  #   "text", x = {{K}}, y = -Inf, label = "Carrying Capacity", vjust = -1.5,
  #   fontface = "italic", color = "blue", size = 5) +
  labs(color = NULL, y = "rate") +
  coord_equal(clip = "off", ratio = 1) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  theme(legend.position = "bottom") +
  NULL

#'
#'
#'
#'


tibble(
  K = 9,
  # N = c(seq.default(0, K), seq.default(K, K + 5))
  N = c(seq.default(0, K), seq.default(K, 2*K))
) %>%
  expand_grid(
    # growth = 1.1,
    # death = 0.1
    birth_baseline = 4,
    death_baseline = 0.5,
  ) %>%
  mutate(
    growth_baseline = birth_baseline - death_baseline,
    growth = pmax(0, growth_baseline * (1 - N / K)),
    death = pmax(0, N - K) * death_baseline / K
    # death = pmax(0, N - K) * growth_baseline / K
  ) %>%
  pivot_longer(c(growth, death)) %>%
  identity() %>%
  ggplot() +
  aes(N, value, group = name) +
  geom_line(aes(color = name)) +

  geom_hline(aes(yintercept = birth_baseline,
                 color = "beta_0"),
             alpha = 0.8,
             linewidth = 1.5,
             linetype = "dotted") +
  geom_hline(aes(yintercept = death_baseline,
                 color = "mu_0"),
             alpha = 0.8,
             linewidth = 1.5,
             linetype = "dotted") +
  geom_hline(aes(yintercept = growth_baseline,
                 color = "r_0"),
             alpha = 0.8,
             linewidth = 1.5,
             linetype = "dotted") +
  geom_vline(aes(xintercept = K), linetype = "dotted") +
  geom_text(
    aes(label = "Carrying Capacity", x = K - 1, y = 2),
    angle = 90,
  ) +
  geom_vline(aes(xintercept = 2 * K), linetype = "dotted") +
  guides(color = guide_legend(override.aes = list(
    linewidth = 2.3
  ))) +
  # annotate(
  #   "text", x = {{K}}, y = -Inf, label = "Carrying Capacity", vjust = -1.5,
  #   fontface = "italic", color = "blue", size = 5) +
  labs(color = NULL, y = "rate") +
  coord_equal(clip = "off", ratio = 1) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  theme(legend.position = "bottom") +
  NULL


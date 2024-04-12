#' Culprit: Maya
start <- list(birth = 0.501237376089681, death = 0.480038235542738)
start <- list(birth = 4 * 0.501237376089681, death = 0.480038235542738)

tibble(
  # N = c(0, 25),
  N = seq.default(0, 25, by = 1),
  K = 20
) %>%
  mutate(
    birth = start$birth * pmax(0, (1 -
                                     (start$birth - start$death) /
                                     (start$birth + start$death) * N / K)),
    death = start$death * (1 +
                             (start$birth - start$death) /
                             (start$birth + start$death) * N / K),
    # death = start$death,
    growth = birth - death
  ) %>%
  pivot_longer(c(birth, death, growth)) %>%
  glimpse() %>%
  ggplot() +
  aes(x = N, y = value, group = name) +
  geom_line(aes(color = name)) +

  # VALIDATION
  # geom_vline(aes(xintercept = K *
  #                  (start$birth - start$death) /
  #                  (start$birth + start$death))) +
  geom_vline(aes(xintercept = K)) +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  labs(y = "rate") +

  guides(color = guide_legend(override.aes = list(linewidth = 2))) +

  labs(color = NULL) +
  theme_bw() +
  theme(legend.position = "bottom") +
  NULL

tibble(
  # N = c(0, 25),
  N = seq.default(0, 25, by = 1),
  K = 20
) %>%
  mutate(
    # birth = start$birth * pmax(0, (1 - N / K)),
    # death = start$death * (1 + N / K),
    # # death = start$death,
    birth = start$birth * pmax(0, (1 - (start$birth - start$death) /
                                     (start$birth + start$death)* N / K)),
    death = start$death * (1 + (start$birth - start$death) /
                             (start$birth + start$death) * N / K),
    # death = start$death,
    birth = birth * N,
    death = death * N,
    growth = birth - death,
    growh = growth * N,
  ) %>%
  pivot_longer(c(birth,death, growth)) %>%
  glimpse() %>%
  ggplot() +
  aes(x = N, y = value, group = name) +
  geom_line(aes(color = name)) +

  geom_vline(aes(xintercept = K)) +
  # VALIDATION
  # geom_vline(aes(xintercept = K *
  #                  (start$death - start$birth) /
  #                  (start$birth + start$death ))) +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  labs(y = "rate × count") +

  guides(color = guide_legend(override.aes = list(linewidth = 2))) +

  labs(color = NULL) +
  theme_bw() +
  theme(legend.position = "bottom") +
  NULL


#' Culprit: Maya
start <- list(birth = 0.501237376089681, death = 0.480038235542738)
start <- list(birth = 4 * 0.501237376089681, death = 0.480038235542738)

tibble(
  # N = c(0, 25),
  N = seq.default(0, 25, by = 1),
  K = 12
) %>%
  mutate(
    birth_naive = start$birth * pmax(0, (1 - N / K)),
    birth_adjust = start$birth * pmax(0, (1 -
                                     (start$birth - start$death) /
                                     (start$birth + start$death) * N / K)),
    death_naive = start$death * pmax(0, (1 + N / K)),
    death_adjust = start$death * pmax(0, (1 +
                             (start$birth - start$death) /
                             (start$birth + start$death) * N / K)),
    # death = start$death,
    growth_naive = birth_naive - death_naive,
    growth_adjust = birth_adjust - death_adjust
  ) %>%
  pivot_longer(
    matches("(naive)|(adjust)$"),
    names_pattern = "(\\w+)_(naive|adjust)",
    names_to = c(".value", "method")
  ) %>%
  mutate(method = fct_inorder(method)) %>%
  pivot_longer(c(birth, death, growth)) %>%
  glimpse() %>%
  ggplot() +
  aes(x = N, y = value, group = str_c(method, name)) +
  geom_line(aes(color = name)) +

  geom_vline(aes(xintercept = K)) +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +

  facet_grid(~method) +

  labs(y = "rate", caption = "Carrying capacity at 12") +

  guides(color = guide_legend(override.aes = list(linewidth = 2))) +

  labs(color = NULL) +
  theme_bw() +
  theme(legend.position = "bottom") +
  NULL

fs::dir_create("figures")
ggsave(
  filename = "figures/birth_death_dd_rate.svg",
  device = svglite::svglite
  # scaling = 3,
  # scale = 3,
  # width = 2*4.27,
  # height = 2.5
)
# Saving 26 x 13.5 in image
#
tibble(
  # N = c(0, 25),
  N = seq.default(0, 25, by = 1),
  K = 12
) %>%
  mutate(
    birth_naive = start$birth * pmax(0, (1 - N / K)),
    birth_adjust = start$birth * pmax(0, (1 -
                                            (start$birth - start$death) /
                                            (start$birth + start$death) * N / K)),
    death_naive = start$death * pmax(0, (1 + N / K)),
    death_adjust = start$death * pmax(0, (1 +
                                    (start$birth - start$death) /
                                    (start$birth + start$death) * N / K)),
    # death = start$death,
    growth_naive = birth_naive - death_naive,
    growth_adjust = birth_adjust - death_adjust
  ) %>%
  pivot_longer(
    matches("(naive)|(adjust)$"),
    names_pattern = "(\\w+)_(naive|adjust)",
    names_to = c(".value", "method")
  ) %>%
  mutate(method = fct_inorder(method)) %>%
  pivot_longer(c(birth, death, growth)) %>%
  glimpse() %>%
  ggplot() +
  aes(x = N, y = N * value, group = str_c(method, name)) +
  geom_line(aes(color = name)) +

  geom_vline(aes(xintercept = K)) +
  # VALIDATION
  # geom_vline(aes(xintercept = K *
  #                  (start$death - start$birth) /
  #                  (start$birth + start$death ))) +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  labs(y = "rate × count",
       caption = "Carrying capacity at 12") +
  facet_grid(~method) +


  guides(color = guide_legend(override.aes = list(linewidth = 2))) +

  labs(color = NULL) +
  theme_bw() +
  theme(legend.position = "bottom") +
  NULL
#'


fs::dir_create("figures")
ggsave(
  filename = "figures/birth_death_dd_rate_times_count.svg",
  device = svglite::svglite
  # scaling = 3,
  # scale = 3,
  # width = 2*4.27,
  # height = 2.5
)
# Saving 26 x 13.5 in image

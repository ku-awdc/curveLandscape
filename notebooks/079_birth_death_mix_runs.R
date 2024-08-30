devtools::load_all()



bdmix_model <- BirthDeathMix$new(
  n0 = 1,
  birth_baseline = 4,
  death_baseline = 1,
  carrying_capacity = 3,
  alpha_mix = 0.1
)


bdmix_model$run(
  t_max = 1., replicates = 500, seed = 20240826
)

config_df <- expand_grid(
  n0 = c(1, 2, 3),
  # carrying_capacity = c(1, 2, 3, 4, 5, 6),
  carrying_capacity = c(1, 2, 3),
  alpha_mix = seq.default(0, 1, length.out = 200)
  # alpha_mix = exp(-log1p(seq.default(0, 250)))
) %>%
  mutate(t_max = 1., replicates = 4000, seed = 20240826)
config_df %>%
  glimpse()
output_df <- config_df %>%
  {
    bind_cols(
      select(., -replicates),
      output = pmap(., \(...) {
        args <- list(...)
        # browser()
        bdmix_model <- BirthDeathMix$new(
          n0 = args$n0,
          birth_baseline = 4,
          death_baseline = 1,
          carrying_capacity = args$carrying_capacity,
          alpha_mix = args$alpha_mix
        )

        bdmix_model$run(
          t_max = args$t_max, replicates = args$replicates, seed = args$seed
        )
      }) %>%
        bind_rows(
          .id = "id_output"
        )
    )
  }
#'
#'
#'
config_df
output_df %>%
  mutate(
    prob_extinction = extinct / replicates
  ) %>%
  print(width = Inf) %>%
  mutate(
    n0_label = fct(str_c("N(0) = ", n0))
  ) %>%
  identity() %>%
  {
    ggplot(.) +
      aes(group = str_c(carrying_capacity, n0)) +
      # stat_ecdf(aes(prob_extinction)) +
      geom_step(
        aes(alpha_mix, prob_extinction, color = n0_label)
      ) +
      facet_wrap(~carrying_capacity, labeller = label_both) +
      labs(carrying_capacity = "Carrying Capacity") +
      labs(y = "Numerical Extinction Probability") +
      labs(color = NULL) +
      labs(x = bquote(alpha)) +
      # facet_grid(n0 ~ carrying_capacity, labeller = label_both) +
      guides(color = guide_legend(
        override.aes = list(linewidth = 1.1)
      )) +
      scale_colour_brewer(direction = -1, palette = "Greens") +
      # scale_x_sqrt(guide = guide_axis(angle = 35)) +
      scale_x_continuous(guide = guide_axis(angle = 35)) +
      expand_limits(y = 1) +
      ggpubr::theme_pubr() +
      ggpubr::labs_pubr() +
      theme(axis.text.x = element_text(size = 10)) +
      # coord_equal(ratio = 3, expand = TRUE) +
      # theme(strip.placement = "outside") +
      NULL
  } -> p_birth_death_mix

p_birth_death_mix

# dev.off()


fs::dir_create("figures")
ggsave(
  plot = p_birth_death_mix,
  filename = "figures/079_birth_death_mix_plot.svg",
  device = svglite::svglite,
  # scaling = 3,
  width = 8,
  height = 4,
  # scale = 1.1,
  # width = 2*4.27,
  # height = 2.5
)

devtools::load_all()

config_df <- expand_grid(
  n0 = c(1, 2, 3),
  carrying_capacity = c(1, 2, 3)
) %>%
  rowid_to_column("id_config") %>%
  expand_grid(
    alpha_mix = c(seq.default(0.95, 1, length.out = 500))
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
output_df <- output_df %>%
  mutate(
    prob_extinction = extinct / replicates
  ) %>%
  print(width = Inf)
output_df %>%
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


# fs::dir_create("figures")
# ggsave(
#   plot = p_birth_death_mix,
#   filename = "figures/079_birth_death_mix_plot.svg",
#   device = svglite::svglite,
#   # scaling = 3,
#   width = 8,
#   height = 4,
#   # scale = 1.1,
#   # width = 2*4.27,
#   # height = 2.5
# )


read_rds(".cache/080_paper2_birth_death_only_extinction_probability.rds") ->
paper2_prob_ext_df

paper2_prob_ext_df


output_df %>%
  select(-extinct, -survived) %>%
  left_join(
    paper2_prob_ext_df %>%
      select(n0, carrying_capacity, prob_extinction),
    by = c("n0", "carrying_capacity")
  ) %>%
  # print(width = Inf) %>%
  group_by(id_config) %>%
  group_map(\(dataa, ...) {
    # browser(),
    dataa %>% summarise(
      carrying_capacity = unique(carrying_capacity),
      n0 = unique(n0),
      # alpha_map = list(approxfun(prob_extinction, alpha_mix))
      estimated_prob_extinction = prob_extinction.y[[1]],
      estimated_alpha = approx(prob_extinction.x, alpha_mix, prob_extinction.y[[1]], rule = 2)$y
    )
  }) %>%
  bind_rows() %>%
  identity() ->
  projected_alpha_values

output_df %>%
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
      
      geom_point(
        data = projected_alpha_values,
        aes(estimated_alpha, estimated_prob_extinction)
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

print(p_birth_death_mix)


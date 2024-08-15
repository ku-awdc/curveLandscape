devtools::load_all()


#' Act one plot.

# write_rds(all_landscapes, ".cache/056_all_landscapes.rds")
all_landscapes <- read_rds(".cache/056_all_landscapes.rds")


#' These landscapes were simulated with:
attributes(all_landscapes)$simulation_configuration


all_landscapes %>%
  glimpse()

all_landscapes$stat_binned_output[[5]]


all_landscapes %>%
  mutate(landscape_type = fct_inorder(landscape_type)) %>% 
  select(landscape_type:n_len, stat_binned_output, -grid) %>%
  unnest(stat_binned_output) %>%
  identity() %>%
  {
    ggplot(.) +
      aes(time, mean, group = str_c(landscape_type, set_patch_size)) +
      geom_line(
        aes(color = factor(set_patch_size)),
      ) +
      
      geom_hline(
        aes(yintercept = all_landscapes$total_cc[[1]]),
        linetype = "dotted",
        linewidth = 1.1
      ) +

      labs(x = "time [years]") + 
      labs(y = "Mean Total Population over replicates") + 
      labs(caption = glue_data(attributes(all_landscapes)$simulation_configuration, 
      "Replicates: {repetitions}"))+ 

      scale_color_viridis_d(direction = 1) + 
      # confidence interval... but they are very low..
      geom_line(linetype = "dotdash", aes(time, ci_lower, color = factor(set_patch_size))) +
      geom_line(linetype = "dotdash", aes(time, ci_upper, color = factor(set_patch_size))) +
      labs(color = NULL) +
      facet_grid(rows = vars(landscape_type), labeller = labeller(landscape_type = c(naive = "grid-based", habic = "habitat-based"))) +
      ggpubr::theme_pubclean(15) +
      NULL
  }


  fs::dir_create("figures")
  ggsave(
    filename = "figures/057_mean_population_count_all_landscapes.svg",
    device = svglite::svglite,
    # scaling = 3,
    scale = 1.9
    # width = 2*4.27,
    # height = 2.5
  )

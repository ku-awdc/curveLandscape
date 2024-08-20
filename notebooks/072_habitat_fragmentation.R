library(units)

habitat_map %>%
  st_cast("POLYGON") %>%
  filter(Habitat != "Non") %>%
  mutate(sub_are = geometry %>% st_area() %>% units::set_units("km^2")) %>%
  identity() %>%
  mutate(sub_are = sub_are %>% as.numeric()) %>%
  {
    ggplot(.) +
      aes(sub_are) +
      stat_bin(
        geom = "step",
        aes(y = after_stat(density)),
        position = "identity",
        pad = TRUE,
        closed = "right"
      ) +
      geom_vline(
        linetype = "dashed",
        linewidth = 0.9,
        data = tibble(
          patch_sizes = c(1, 2, 5, 10, 25),
          patch_sizes_label = fct(patch_sizes %>% str_c(" km²"))
        ),
        aes(xintercept = patch_sizes, color = patch_sizes_label)
      ) +
      labs(x = "Habitat areas") +
      labs(color = NULL) +
      guides(color = guide_legend(override.aes = list(linewidth = 1.2))) +
      scale_color_viridis_d(direction = 1) +
      # facet_wrap(~Habitat, ncol = 1, scales = "free_y") +
      scale_x_sqrt(limits = c(0, NA)) +
      facet_wrap(
        ~Habitat,
        ncol = 1,
        strip.position = "right",
        # labeller = labeller(landscape_type = c(naive = "grid-based", habic = "habitat-based"))
      ) +
      ggpubr::theme_pubclean(15)
  }

  fs::dir_create("figures")
  ggsave(
    filename = "figures/072_patch_sizes_and_habitat_fragmentation.svg",
    device = svglite::svglite,
    # scaling = 3,
    scale = 1.9,
    # width = 2*4.27,
    # height = 2.5
  )

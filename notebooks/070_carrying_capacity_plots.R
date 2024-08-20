devtools::load_all()

all_landscapes %>%
  mutate(
    cc = grid %>% map(. %>% st_drop_geometry() %>% `[`("Capacity"))
  ) %>%
  select(-grid) %>%
  unnest(cc) %>%
  mutate(landscape_type = fct_inorder(landscape_type)) %>%
  {
    ggplot(.) +
      aes(group = str_c(landscape_type, set_patch_size)) +
      stat_bin(
        aes(Capacity,
          y = after_stat(density),
          color = fct(set_patch_size %>% str_c(" km²"))
        ),
        binwidth = 0.5, geom = "step",
        position = "identity",
        pad = TRUE,
        closed = "right"
        # position = position_jitter(height = 0.05, width = 0)
      ) +
      facet_wrap(
        landscape_type ~ .,
        ncol = 1,
        strip.position = "right",
        labeller = labeller(landscape_type = c(naive = "grid-based", habic = "habitat-based"))
      ) +
      scale_x_sqrt(limits = c(0, NA)) +
      # guides(color = guide_coloursteps())
      guides(color = guide_legend(override.aes = list(linewidth = 1.2))) +
      scale_color_viridis_d(direction = 1) +
      labs(color = "Patch-sizes") +
      labs(x = "Carrying capacity") +
      labs(y = "Proportion of Habitable Patches") +
      ggpubr::theme_pubclean(15) +
      # theme(aspect.ratio = 10/16) +
      NULL
  } 

fs::dir_create("figures")
ggsave(
  filename = "figures/070_carrying_capacity_of_landscapes.svg",
  device = svglite::svglite,
  # scaling = 3,
  scale = 1.9,
  # width = 2*4.27,
  # height = 2.5
)

